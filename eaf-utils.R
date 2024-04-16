#### Utilities for working with .eaf files ####
##
## Originally developed for Elan File Checker(s), but usable(ish) as standalone functions


## File setup -------------------------------------------------------------

##Given a vector of filenames, extract file extension, speaker code, and 
##  neighborhood, as a dataframe
##In the app, x is input$files$name
parse_filenames <- function(x, 
                            spkrCodeRE="^(CB|FH|HD|LV)\\d+(and\\d+)?",
                            neighborhoodRE="^(CB|FH|HD|LV)") {
  if (!identical(x, basename(x))) {
    warning("Found non-basename(s) in x. Using basename(x) instead")
    x <- basename(x)
  }
  
  library(dplyr)
  library(stringr)
  library(tools)
  
  ##Add file info
  data.frame(name = x) %>% 
    mutate(SpkrCode = str_extract(name, spkrCodeRE),
           Neighborhood = str_extract(SpkrCode, neighborhoodRE),
           FileExt = file_ext(name))
}

##Given a single xml_document, return a list of dataframes of tier annotations,
##  one for each tier. Includes file metadata as attributes for object, and 
##  tier metadata as attributes for dataframe-elements
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
  
  ##Handle missing TIER_ID
  rowIds <- as.character(seq_len(nrow(tierAttr)))
  if (!("TIER_ID" %in% colnames(tierAttr))) {
    tierAttr$TIER_ID <- rowIds
  }
  if (anyNA(tierAttr$TIER_ID)) {
    tierAttr <- tierAttr %>% 
      mutate(across(TIER_ID, ~ coalesce(.x, rowIds)))
  }
  
  ##Get timing data
  times <- getTimes(x, tierAttr$TIER_ID)
  if (!annotation_metadata) {
    times <- times %>% 
      map(~ select(.x, Start, End, Text))
  }
  
  ##Put data & metadata together
  out <- times
  ##Add tier attributes to each df element
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
  
  structure(out, class="transcription")
}

##Given a transcription object, output a dataframe of tier metadata
##If no tier has a TIER_ID, output dataframe's TIER_ID is row numbers. If no
##  tier has a PARTICIPANT, output dataframe's PARTICIPANT is NA
tier_metadata <- function(x) {
  library(purrr)
  if (!inherits(x, "transcription")) {
    stop("x must be an object of class transcription")
  }
  
  out <- 
    x %>% 
    map_dfr(~ .x %>%
              attributes() %>%
              discard_at(c("class", "row.names", "names")),
            .id="TIER_ID")
  
  if (!("PARTICIPANT" %in% colnames(out))) {
    out$PARTICIPANT <- NA
  }
  
  out
}


## Overlaps ---------------------------------------------------------------
##Function that takes a tier name, eaf file, and file-wide time slot DF as
##  input and outputs actual times for annotations
getTimesTier <- function(tierName, eaf, timeSlots) {
  library(stringr)
  library(xml2)
  library(dplyr)
  
  ##Get tier node, accounting for numeral TIER_ID
  ##N.B. This will return unexpected results if integers are valid tier names,
  ##  *and* these integers don't line up with the tier's index
  if (str_detect(tierName, "^\\d+$")) {
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

##x is overlapTiers
findOverlapsOneFile <- function(x, tierPriority, overlapThresh=NULL) {
  library(purrr)
  library(dplyr)
  library(tidyr)
  
  ##Check args
  if (!is.list(x) || !every(x, is.data.frame)) {
    stop("x must be a list of dataframes")
  }
  if (!is.character(tierPriority)) {
    stop("tierPriority must be a character vector")
  }
  stopifnot(is.numeric(overlapThresh) && overlapThresh >= 0)
  
  ##Convenience renaming to match fixOverlapsOneFile()
  overlapTiers <- x
  
  ##Get turn boundaries for overlap tiers
  bounds <- 
    overlapTiers %>% 
    map(~ pivot_longer(.x, c(Start, End), names_to="Bound", values_to="Time"))
  
  ##Get overlaps
  boundJoin <- join_by(between(x$Time, y$Start, y$End, bounds="()"))
  overlapBounds <-
    bounds %>% 
    imap(~ inner_join(.x, 
                      ##Get all turns for *other* overlap tiers
                      overlapTiers %>% 
                        discard_at(.y) %>% 
                        bind_rows(.id="TIER_ID_overlapped"),
                      boundJoin,
                      suffix=c("", "_overlapped")) %>% 
           rename_with(~ paste0(.x, "_overlapped"), c(Start, End)))
  
  ##Add info to help determine which boundary to change
  overlapBounds <- overlapBounds %>% 
    bind_rows(.id="TIER_ID") %>% 
    mutate(StartDiff = abs(Start_overlapped - Time),
           EndDiff = abs(End_overlapped - Time),
           CloseEnough = any(StartDiff < overlapThresh, 
                             EndDiff < overlapThresh),
           across(contains("TIER_ID"), 
                  list(priority = ~ match(.x, tierPriority)))) %>% 
    arrange(TIER_ID_priority, TIER_ID_overlapped_priority)
  
  overlapBounds
}

fixOverlapsOneFile <- function(x, nm, method=c("old","new"),
                               noOverlapCheckTiers=NULL, 
                               overlapThresh=NULL, timeDisp=NULL,
                               checkZeroWidth=c("drop","error")) {
  library(purrr)
  library(dplyr)
  library(tidyr)
  
  ##Check args
  if (!inherits(x, "transcription")) {
    stop("x must be an object of class transcription")
  }
  if (!is.character(nm) || length(nm) != 1) {
    stop("nm must be a string")
  }
  method <- match.arg(method)
  stopifnot(is.character(noOverlapCheckTiers))
  stopifnot(is.numeric(overlapThresh) && overlapThresh >= 0)
  stopifnot(is.character(timeDisp))
  checkZeroWidth <- match.arg(checkZeroWidth)
  
  ##Get tier priority order
  fileInfo <- parse_filenames(nm)
  ##Interviewers can be named either actual name or Interviewer [SpkrCode]
  interviewerTier <- c(
    case_when(
      fileInfo$Neighborhood=="HD" ~ "Trista Pennington", 
      fileInfo$SpkrCode %in% c("CB02", "CB18") ~ "Jennifer Andrus",
      TRUE ~ "Barbara Johnstone"),
    paste("Interviewer", fileInfo$SpkrCode))
  ##Get priority order
  tierInfo <- tier_metadata(x) %>% 
    filter(!(TIER_ID %in% noOverlapCheckTiers)) %>% 
    ##Prioritize Redaction > Main speaker(s) > Interviewer > Bystander(s)
    mutate(OverlapGroup = case_match(TIER_ID,
                                     "Redaction" ~ "Redaction",
                                     fileInfo$SpkrCode ~ "Main speaker",
                                     interviewerTier ~ "Interviewer",
                                     .default="Bystander"
    )) %>% 
    ##Arrange by priority order
    arrange(
      ##Coincidentally, priority order == reverse alphabetical order
      desc(OverlapGroup),
      ##If multiple main speakers or bystanders, break ties by name order
      TIER_ID)
  ##Create priority vector
  tierPriority <- tierInfo$TIER_ID
  
  ##Account for missing ANNOTATION_ID
  if (!every(x, ~ "ANNOTATION_ID" %in% colnames(.x))) {
    x <- x %>% 
      map_dfr(as.data.frame, .id="Tier") %>% 
      mutate(ANNOTATION_ID = paste0("a", row_number()),
             .before=1) %>% 
      nest(data = -Tier) %>% 
      pull(data, Tier)
  }
  
  ##Get non-empty overlap tiers in tier-priority order
  overlapTiers <-
    tierInfo %>% 
    nest_join(x %>% 
                map_dfr(~ select(.x, ANNOTATION_ID, Start, End),
                        .id="TIER_ID"),
              "TIER_ID", 
              name="data") %>% 
    pull(data, TIER_ID) %>% 
    ##Only non-empty tiers
    discard(~ nrow(.x)==0)
  
  ##Single-df version of overlapTiers
  overlapTiersDF <- bind_rows(overlapTiers, .id="TIER_ID")
  
  ##Iteratively find and fix overlaps until a stable solution has been found
  ##  OR the loop hits its maximum number of iterations
  overlapsCurr <- findOverlapsOneFile(overlapTiers, tierPriority, overlapThresh)
  overlapsOld <- NULL
  iters <- 0
  maxIter <- 10
  
  ##Main execution loop
  while (nrow(overlapsCurr) > 0 && !identical(overlapsCurr, overlapsOld)) {
    ##Increment iteration counter
    iters <- iters + 1
    ##Exit loop if maximum iterations reached
    if (iters==maxIter) {
      warning("Overlap-fixing reached maximum iterations (", maxIter, ") ",
              "without reaching a stable solution")
      break
    }
    ##Curr is now Old
    overlapsOld <- overlapsCurr
    
    ##Identify timeslots to change
    if (method=="old") {
      ##Old method: Set the overlapping boundary to the overlapped turn's
      ##  closest boundary (regardless of priority)
      ##Old-method code is written to ostensibly fix each tier in reverse-
      ##  priority order, but in reality, it's the same as if all tiers are
      ##  fixed simultaneously (as they are here)
      newStarts <-
        overlapsCurr %>% 
        filter(CloseEnough,
               Bound=="Start") %>% 
        mutate(Start = if_else(StartDiff < EndDiff, Start_overlapped, End_overlapped)) %>% 
        select(TIER_ID, ANNOTATION_ID, Start)
      newEnds <-
        overlapsCurr %>% 
        filter(CloseEnough,
               Bound=="End") %>% 
        mutate(End = if_else(StartDiff < EndDiff, Start_overlapped, End_overlapped)) %>% 
        select(TIER_ID, ANNOTATION_ID, End)
    }
    
    ##Update timeslots
    overlapTiersDF <- overlapTiersDF %>%
      rows_update(newStarts, c("TIER_ID", "ANNOTATION_ID")) %>% 
      rows_update(newEnds, c("TIER_ID", "ANNOTATION_ID"))
    
    ##Get updated state of overlaps
    overlapTiers <- 
      overlapTiersDF %>% 
      nest(data = -TIER_ID) %>% 
      pull(data, TIER_ID)
    overlapsCurr <- findOverlapsOneFile(overlapTiers, tierPriority, 
                                        overlapThresh)
  }
  
  overlapTiers
}
