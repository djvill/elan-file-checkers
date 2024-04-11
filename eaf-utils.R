#### Utilities for working with .eaf files ####
##
## Originally developed for Elan File Checker(s), but usable(ish) as standalone functions


## File setup -------------------------------------------------------------

##Given input$files$name, extract file extension, speaker code, and neighborhood
parse_filenames <- function(x, 
                            spkrCodeRE="^(CB|FH|HD|LV)\\d+(and\\d+)?",
                            neighborhoodRE="^(CB|FH|HD|LV)") {
  library(dplyr)
  library(stringr)
  library(tools)
  
  ##Add file info
  data.frame(name = x) %>% 
    mutate(SpkrCode = str_extract(name, spkrCodeRE),
           Neighborhood = str_extract(SpkrCode, neighborhoodRE),
           FileExt = file_ext(name))
}

##Dataframe of tier information
##In app:
##- x is eaflist() reactive (output of read_eafs(fileDF()$File, fileDF()$datapath))
##- df is fileDF() reactive (output of fileInfo(input$files)); fileDF() has to 
##  come first to trigger read_eafs() to create eaflist()
##In interactive use:
##- x can be output of <file name vector> %>% set_names(basename(.)) %>% read_eafs(., .)
##- df can be omitted (in which case fileInfo() is called inside this function)
tierInfo <- function(x, df, nonSpeakerTiers=NULL) {
  library(xml2)
  library(purrr)
  library(dplyr)
  
  if (is.null(names(x))) {
    stop("x must have names")
  }
  
  if (missing(df)) {
    df <- fileInfo(data.frame(File = names(x)))
  }
  
  out <- x %>% 
    map(xml_find_all, "//TIER") %>% 
    ##One row per tier, with file info
    map_dfr(~ map_dfr(.x, xml_attrs), .id="File") %>% 
    ##Add info from fileDF
    left_join(df, by="File")
  
  if ("TIER_ID" %in% colnames(out)) {
    ##Add SpkrTier (is the tier a speaker tier?)
    if (!is.null(nonSpeakerTiers)) {
      out <- out %>% 
        mutate(SpkrTier = !(tolower(TIER_ID) %in% tolower(nonSpeakerTiers)))
    } else {
      out$SpkrTier <- TRUE
    }
  }
  
  ##Add file-wide AUTHOR attribute (or NA if missing)
  auths <- tibble(File = names(x), AUTHOR = unname(map_chr(x, xml_attr, "AUTHOR")))
  out <- out %>% 
    left_join(auths, "File")
  
  out
}

##Given a single xml_document, return a list of dataframes of tier annotations,
##  one for each tier. Includes file metadata as attributes for object, and 
##  tier metadata as attributes for dataframe-elements
##To get tier info, pipe this function's output into 
##  map_dfr(~ .x %>%
##            attributes() %>%
##            discard_at(c("class", "row.names", "names")),
##          .id="TIER_ID")
eaf_to_df_list <- function(x, annotation_metadata=FALSE) {
  library(xml2)
  library(purrr)
  library(tidyr)
  library(dplyr)
  
  if (!("xml_document" %in% class(x))) {
    stop("x must be an XML document")
  }
  
  ##Get file attributes
  fileAttr <- xml_attrs(x)
  
  ##Get tier names & attributes
  tierAttr <-
    x %>% 
    xml_find_all("//TIER") %>% 
    map_dfr(xml_attrs)
  if (!("TIER_ID" %in% colnames(tierAttr))) {
    tierAttr$TIER_ID <- seq_len(nrow(tierAttr))
  }
  
  ##Get timing data
  times <- getTimes(x, tierAttr$TIER_ID)
  if (!annotation_metadata) {
    times <- times %>% 
      map(~ select(.x, Start, End, Text))
  }
  
  ##Put data & metadata together
  out <- times
  ##Format & add tier attributes
  tierAttrList <- 
    tierAttr %>% 
    nest(data = -TIER_ID) %>% 
    pull(data, TIER_ID) %>% 
    map(as.list)
  out <- out %>% 
    ##Preserve existing attributes (i.e., column names)
    map2(tierAttrList, ~`attributes<-`(.x, c(attributes(.x), .y)))
  ##Add file attributes
  attributes(out) <- c(attributes(out), fileAttr)
  
  out
}


## Overlaps ---------------------------------------------------------------
##Function that returns a list of tiers to overlap-check for each file.
##  df should be tierDF() reactive
getOverlapTiers <- function(df, inclRedact=TRUE) {
  library(dplyr)
  library(purrr)
  
  ##Account for missing TIER_ID attribute (which blocks SpkrTier)
  if (!("TIER_ID" %in% colnames(df))) {
    df$SpkrTier <- FALSE
  }
  
  ##Prioritize Redaction > Main speaker(s) > Interviewer > Bystander(s)
  df <- df %>% 
    mutate(OverlapTier = case_when(
      TIER_ID=="Redaction" ~ "Redaction",
      TIER_ID==SpkrCode ~ "Main speaker",
      Neighborhood=="HD" & TIER_ID=="Trista Pennington" ~ "Interviewer",
      Neighborhood!="HD" & TIER_ID %in% c("Barbara Johnstone", "Jennifer Andrus") ~ "Interviewer",
      startsWith(TIER_ID, "Interviewer") ~ "Interviewer",
      SpkrTier ~ "Bystander",
      TRUE ~ NA_character_
    )) %>% 
    ##Remove tiers that aren't in priority order
    filter(!is.na(OverlapTier)) %>% 
    ##Put in order
    arrange(File,
            ##Coincidentally, priority order == reverse alphabetical order
            desc(OverlapTier),
            ##If multiple main speakers or bystanders, break ties by name order
            TIER_ID)
  
  ##Optionally exclude Redaction
  if (!inclRedact) {
    df <- df %>% 
      filter(TIER_ID != "Redaction")
  }
  
  ##Get tiers for each file
  tierNames <-
    df %>% 
    nest(data = -File) %>% 
    mutate(TierOrder = map(data, "TIER_ID")) %>% 
    pull(TierOrder, name=File)
  
  tierNames
}

##Function that takes a tier name, eaf file, and file-wide time slot DF as
##  input and outputs actual times for annotations
getTimesTier <- function(tierName, eaf, timeSlots) {
  library(stringr)
  library(xml2)
  library(dplyr)
  
  ##Get tier node, accounting for missing TIER_ID
  if (is.numeric(tierName)) {
    tierPath <- str_glue("//TIER[{tierName}]")
  } else {
    tierPath <- str_glue("//TIER[@TIER_ID='{tierName}']")
  }
  tierNode <- xml_find_first(eaf, tierPath)
  
  ##Get turn time-slot IDs as a dataframe
  childTier <- !is.na(xml_attr(tierNode, "PARENT_REF"))
  if (childTier) {
    turnPath <- ".//REF_ANNOTATION"
  } else {
    turnPath <- ".//ALIGNABLE_ANNOTATION"
  }
  turnNodes <- xml_find_all(tierNode, turnPath)
  timesTier <- 
    turnNodes %>% 
    xml_attrs() %>% 
    bind_rows()
  
  ##If tier is empty, return 0-row dataframe
  if (nrow(timesTier)==0) {
    emptyTimes <- tibble(ANNOTATION_ID = character(),
                         TIME_SLOT_REF1 = character(),
                         TIME_SLOT_REF2 = character(),
                         Start = numeric(),
                         End = numeric(),
                         Text = character())
    return(emptyTimes)
  }
  
  ##If tier is a dependency, add time-slot IDs from parent tier
  if (childTier) {
    parent <- xml_attr(tierNode, "PARENT_REF")
    ##Get parent annotation IDs
    timesParent <- 
      xml_find_all(eaf, str_glue("//TIER[@TIER_ID='{parent}']//ALIGNABLE_ANNOTATION")) %>% 
      xml_attrs() %>% 
      bind_rows()
    ##Add to tier
    timesTier <- timesTier %>% 
      left_join(timesParent %>% 
                  rename(ANNOTATION_REF = ANNOTATION_ID),
                "ANNOTATION_REF")
  }
  
  ##Add actual times
  timesTier <- timesTier %>% 
    ##Add actual times
    left_join(timeSlots %>%
                rename(TIME_SLOT_REF1 = TIME_SLOT_ID,
                       Start = TIME_VALUE),
              by="TIME_SLOT_REF1") %>%
    left_join(timeSlots %>%
                rename(TIME_SLOT_REF2 = TIME_SLOT_ID,
                       End = TIME_VALUE),
              by="TIME_SLOT_REF2") %>% 
    relocate(Start, End, .after=TIME_SLOT_REF2)
  
  ##Add Text
  timesTier <- timesTier %>% 
    mutate(Text = xml_text(turnNodes))
  
  timesTier
}

##Wrapper function around getTimesTier() that takes a single EAF file and name
##  (meant to be used with eaflist() reactive and imap()) plus multi-file tier
##  df (meant to be used with tierDF() reactive) as input, and outputs nested
##  list of dataframes of annotation times (files at level one, tier DFs at
##  level two for speaker tiers only)
##N.B. This function outputs a list of DFs rather than a single DF because the
##  list structure makes it easier to detect overlaps in fixOverlaps() (by
##  comparing the timings on a given speaker tier to all other speaker tiers)
getTimes <- function(eaf, tiers) {
  library(xml2)
  library(dplyr)
  library(purrr)
  
  ##Timeslots (maps time slot ID to actual time, in milliseconds)
  timeSlots <- 
    ##Get TIME_SLOT nodes
    eaf %>% 
    xml_find_all("//TIME_SLOT") %>% 
    ##Get attributes as a dataframe
    xml_attrs() %>% 
    bind_rows() %>% 
    ##Make actual time numeric
    mutate(across(TIME_VALUE, as.numeric))
  
  ##Get times for tiers (list of dataframes)
  timesEAF <- 
    tiers %>% 
    set_names(., .) %>% 
    map(getTimesTier, eaf, timeSlots)
  
  timesEAF
}
