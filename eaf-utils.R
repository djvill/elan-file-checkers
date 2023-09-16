#### Utilities for working with .eaf files ####
##
## Originally developed for Elan File Checker(s), but usable(ish) as standalone functions


## EAF conversion ---------------------------------------------------------
##Functions to be used interactively rather than in the actual app

##Get a dataframe from several paths to EAFs
eafs_to_df <- function(..., singleDF=TRUE, 
                       nonSpeakerTiers=c("Comment","Noise","Redaction")) {											 
  eaflist <- unlist(list(...))
  xmllist <- read_eafs(eaflist)
  names(xmllist) <- make.unique(basename(eaflist))
  xmllist_to_df(xmllist, singleDF=singleDF, nonSpeakerTiers=nonSpeakerTiers)
}

##Get a dataframe from a directory with several EAFs
eafDir_to_df <- function(eafDir, singleDF=TRUE, pattern=".+\\.eaf$", 
                         nonSpeakerTiers=c("Comment","Noise","Redaction")) {
  eaflist <- dir(eafDir, pattern=pattern, full.names=TRUE)
  xmllist <- read_eafs(eaflist)
  names(xmllist) <- make.unique(basename(eaflist))
  xmllist_to_df(xmllist, singleDF=singleDF, nonSpeakerTiers=nonSpeakerTiers)
}


## File setup -------------------------------------------------------------
##Dataframe of file information
fileInfo <- function(x, 
                     spkrRE="^((?:CB|FH|HD|LV)\\d+(?:and\\d+)?)\\.?(.+)\\..+?$",
                     spkrNumRE="([A-Z]{2})(\\d+)(?:and\\d+)?") {
  library(dplyr)
  library(tidyr)
  library(stringr)
  
  if (!is.data.frame(x) || nrow(x)==0 || all(!(c("File","name") %in% colnames(x)))) {
    stop("x must be a dataframe with at least one row & column called either File or name")
  }
  
  if ("name" %in% colnames(x) && !("File" %in% colnames(x))) {
    x <- x %>% 
      rename(File = name)
  }
  
  ##Add file info
  x %>% 
    ##Add neighborhood, speaker number, and file number
    ##Will need to be extended to multiple speakers, non-interview tasks, etc.
    tidyr::extract(File, c("SpkrCode", "FileSuffix"), spkrRE, FALSE, TRUE) %>% 
    tidyr::extract(SpkrCode, c("Neighborhood", "SpeakerNum"), spkrNumRE, FALSE, TRUE) %>% 
    ##Add file extension
    mutate(FileExt = str_extract(File, "[^\\.]+$"),
           FileExtValid = FileExt=="eaf",
           SpkrCodeValid = !is.na(SpkrCode),
           FileNameValid = FileExtValid & SpkrCodeValid,
           .after=File) %>% 
    ##Sort
    arrange(Neighborhood, SpeakerNum, FileSuffix, File)
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
  
  x <- x %>% 
    map(xml_find_all, "//TIER") %>% 
    ##One row per tier, with file info
    map_dfr(~ map_dfr(.x, xml_attrs), .id="File") %>% 
    ##Add info from fileDF
    left_join(df, by="File")
  
  if (!is.null(x$PARTICIPANT)) {
    ##Add SpkrTier (is the tier a speaker tier?)
    if (!is.null(nonSpeakerTiers)) {
      x <- x %>% 
        mutate(SpkrTier = !(tolower(PARTICIPANT) %in% tolower(nonSpeakerTiers)))
    } else {
      x$SpkrTier <- TRUE
    }
  }
  
  x
}

##Lower-level function called by eafs_to_df() and eafDir_to_df()
xmllist_to_df <- function(x, singleDF=TRUE, nonSpeakerTiers=NULL, inclRedact=TRUE) {
  library(purrr)
  
  cls <- x %>% 
    map_lgl(~ "xml_document" %in% class(.x))
  if (!all(cls)) {
    stop("All elements of x must be an XML document")
  }
  
  ##Get list of each file's tier names to include
  tierDF <- tierInfo(x, nonSpeakerTiers=nonSpeakerTiers)
  if ("TIER_ID" %in% colnames(tierDF)) {
    tierNames <- getOverlapTiers(tierDF, inclRedact)
  } else {
    tierNames <- seq_len(nrow(tierDF))
  }
  
  ##Get timing data
  times <- list(eaf = x, 
                tiers = tierNames) %>% 
    pmap(getTimes)
  
  if (singleDF) {
    times %>%
      map_dfr(~ map_dfr(.x, as.data.frame, .id="Tier"), .id="File")
  } else {
    times
  }
}

##From a character vector of paths, creates a list of XML objects w/ the file
##  basenames as element names
##It's necessary to have datapath and filename specified separately, because 
##  datapath is a temporary path *without* the original filename
##Doesn't check for valid paths, because in the app that's handled by
##  req(all(fileDF()$FileNameValid))
read_eafs <- function(datapath) {
  library(purrr)
  library(xml2)
  
  if (!is.character(datapath)) {
    stop("datapath must be a character vector")
  }
	
  map(datapath, read_xml)
}


## Overlaps ---------------------------------------------------------------
##Function that returns a list of tiers to overlap-check for each file.
##  df should be tierDF() reactive
getOverlapTiers <- function(df, inclRedact=TRUE) {
  library(dplyr)
  library(purrr)
  
  ##Account for missing Participant attribute (which blocks SpkrTier)
  if (is.null(df$PARTICIPANT)) {
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
    filter(!is.na(OverlapTier)) %>% 
    arrange(desc(OverlapTier),
            ##If multiple main speakers or bystanders, break ties with name order
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
  
  ##Construct xpath to account for missing TIER_ID
  if (is.numeric(tierName)) {
    xpath <- str_glue("//TIER[{tierName}]//ALIGNABLE_ANNOTATION")
  } else {
    xpath <- str_glue("//TIER[@TIER_ID='{tierName}']//ALIGNABLE_ANNOTATION")
  }
  
  timesTier <-
    ##Get all ALIGNABLE_ANNOTATION tags
    xml_find_all(eaf, xpath) %>% 
    ##Get attributes as a dataframe
    xml_attrs() %>% 
    bind_rows()
  
  ##Add actual times, only if tier is nonempty
  if (nrow(timesTier) > 0) {
    timesTier <- timesTier %>% 
      ##Add actual times
      left_join(timeSlots %>%
                  rename(TIME_SLOT_REF1 = TIME_SLOT_ID,
                         Start = TIME_VALUE),
                by="TIME_SLOT_REF1") %>%
      left_join(timeSlots %>%
                  rename(TIME_SLOT_REF2 = TIME_SLOT_ID,
                         End = TIME_VALUE),
                by="TIME_SLOT_REF2")
  } else {
    ##If tier is empty, return NULL (will be immediately discard()ed)
    NULL
  }
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
    map(getTimesTier, eaf, timeSlots) %>%
    ##Only nonempty tiers
    discard(is.null)
  
  timesEAF
}
