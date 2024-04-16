message("****ELAN-FILE-CHECKERS****\n")

library(shiny)
library(xml2)
library(stringr)
library(purrr)
library(tidyr)
library(dplyr)
library(magrittr)


# Parameters ------------------------------------------------------------------

##Version
vers <- "1.3.1"

##Debugging
##  (See also info about "interactive use" below)
##Show additional UI element "debugPrint" at top of main panel for debugging?
showDebug <- FALSE

##File structures
##Regex for extracting SpkrCode column from filenames
spkrCodeRegex <- "^(CB|FH|HD|LV)\\d+(and\\d+)?"
##Regex for extracting Neighborhood column from SpkrCode
neighborhoodRegex <- "^(CB|FH|HD|LV)"

##Tier checking
##Required non-speaker tiers
nonSpkrTiers <- c("Comment","Noise","Redaction")
##Tiers that should never be present
prohibTiers <- c("Recheck",
                 "Text",                                                 ##From CLOx
                 paste0("SPEAKER_0", 0:9),                               ##From AI segmentation
                 paste0("PAR", 0:9), paste0("wor@", paste0("PAR", 0:9)), ##From Batchalign
                 "NoMatch - Word","NoMatch - Turn","Overlap"             ##From Fill-Batchalign-Words
                 )

##Dictionary checking
##Tiers to exclude from dictionary checking
noDictCheckTiers <- c("Comment","Noise","Redaction")
##Include local version of aplsDict.txt? Include ONLY local version?
##  Only works on Dan's machine, useful for testing new entries without
##  committing each time
inclLocalDict <- FALSE						# Not yet modularized
onlyLocalDict <- FALSE						# Not yet modularized
##Permit angle brackets for single-word interruptions?
permitAngleBrackets <- FALSE						# Not yet modularized
##Characters to accept in pronounce codes
pronChars <- "[pbtdkgNmnlrfvTDszSZjhwJ_CFHPIE{VQU@i$u312456789#'\"-]"						# Not yet modularized
##Case-sensitive?
caseSens <- FALSE						# Not yet modularized

##Overlap fixing
##Tiers to exclude from overlap checking/fixing
noOverlapCheckTiers <- c("Comment","Noise")
##Maximum cross-tier misalignment (in ms) to 'snap together'. Set lower to be
##  more conservative about what counts as an intended cross-tier alignment
overlapThresh <- 500						# Not yet modularized
##Time display type: "S" (seconds, useful for Praat TextGrids), or "HMS" (useful
##  for ELAN)
timeDisp <- "HMS"						# Not yet modularized
##Check for 0-width post-fixing annotations (can cause issues)
##  "error" = throw error in fixOverlapsTier()
##  "drop" = silently drop 0-width annotations at end of fixOverlaps()
##  NULL = don't check
checkZeroWidth <- "drop"						# Not yet modularized

##Exit-early overrides, for debugging
overrideExit <- list(fileName = FALSE, tiers = FALSE, dict = FALSE, overlaps = FALSE)

# UI ----------------------------------------------------------------------
ui <- fluidPage(
  tags$head(
    tags$title("Elan File Checker | APLS"),
    tags$link(rel="stylesheet", type="text/css", href="file-checker.css"),
    tags$link(rel="stylesheet", type="text/css", href="doc.css"),
    tags$link(rel="icon", type="image/svg", href="https://github.com/djvill/APLS/raw/main/assets/img/1f34e.svg")
  ),
  div(id="header",
    h1("Elan File Checker", class="title"),
    h3("Archive of Pittsburgh Language and Speech (APLS)", class="subtitle"),
    h4("Dan Villarreal", class="author"),
    h4(paste("Version", vers), class="version")
  ),
  sidebarLayout(
    sidebarPanel(
      fileInput("files",
                label="Drag and drop Elan files in the box below",
                buttonLabel="Browse...",
                placeholder="Box outline must turn green",
                multiple = TRUE),
      p("Source code for this app on", 
        a("GitHub", 
          href="https://github.com/djvill/elan-file-checkers/",
          target="_blank"))
    ),
    
    ##UI details are complicated, so they all take place in output$out
    mainPanel(uiOutput("debug"),
              uiOutput("export"), ##Never shown
              uiOutput("out"))
  )
)


# Functions for server-side processing ------------------------------------

source("eaf-utils.R")

## File setup =============================================================

## See eaf-utils.R:
## - getOverlapTiers()
## - getTimesTier()
## - getTimes()

##Given a list of eafs (either an xml_document or an error thrown by
##  read_xml()), generate a dataframe of information on whether each file &
##  filename are valid (with regexes passed down to parse_filenames())
##In the app, x is eaflist()
validateEaflist <- function(x, spkrCodeRE=spkrCodeRegex, 
                            neighborhoodRE=neighborhoodRegex) {
  if (is.null(names(x))) {
    stop("x must be a named list")
  }
  
  ##File info: speaker code, file extension
  fileDF <- 
    x %>% 
    names() %>% 
    parse_filenames(spkrCodeRE, neighborhoodRE)
  
  ##Dataframe of whether files were successfully read
  readDF <- 
    x %>% 
    map_lgl(~ "xml_document" %in% class(.x)) %>% 
    tibble(name = names(.), FileReadValid = .)
  
  ##Add columns for validation: speaker code initial, eaf file extension,
  ##  whether file was successfully read
  fileDF <- fileDF %>% 
    mutate(FileExtValid = FileExt=="eaf",
           SpkrCodeValid = !is.na(SpkrCode)) %>% 
    left_join(readDF, "name") %>%
    select(name, ends_with("Valid")) %>% 
    ##File is valid only if it satisfies all conditions
    rowwise() %>% 
    mutate(Valid = all(c_across(-name))) %>% 
    ungroup()
  
  fileDF
}

## Tiers ==================================================================
##Given a transcription object and filename, output character vector of tier
##  issues
tierIssuesOneFile <- function(x, nm, nonSpkrTiers=NULL, prohibTiers=NULL) {
  library(dplyr)
  library(purrr)
  library(stringr)
  
  if (!inherits(x, "transcription")) {
    stop("x must be an object of class transcription")
  }
  if (!is.character(nm) || length(nm) != 1) {
    stop("nm must be a string")
  }
  
  ##Get filename, speaker code, neighborhood
  fileInfo <- parse_filenames(nm)
  ##Get tier metadata
  tierInfo <- x %>% 
    tier_metadata() %>% 
    ##Add info for checking tier attributes
    mutate(ExistsTIER_ID = TIER_ID!=row_number(),
           ExistsPARTICIPANT = !is.na(PARTICIPANT),
           TierIDisParticipant = TIER_ID==PARTICIPANT)
  
  ##Initialize empty issues character vector
  issues <- character(0L)
  
  ##Detect missing/empty AUTHOR attribute
  author <- attr(x, "AUTHOR")
  if (is.null(author) || author=="" || tolower(author)=="unspecified") {
    issues <- c(issues, paste("File missing an AUTHOR attribute"))
  }
  
  ##Handle prohibited tiers
  if (!is.null(prohibTiers)) {
    issues <- c(issues,
                prohibTiers %>% 
                  map_if(~ any(str_detect(tolower(tierInfo$TIER_ID), tolower(.x)), na.rm=TRUE),
                         ~ paste("The completed file should not have a tier named", .x),
                         .else = ~ character(0L)) %>% 
                  flatten_chr())
  }
  
  ##Handle missing tiers (non-interviewer)
  checkTiers <- c(unique(fileInfo$SpkrCode), nonSpkrTiers)
  missingTiers <- setdiff(checkTiers, tierInfo$TIER_ID)
  if (length(missingTiers) > 0) {
    issues <- c(issues, paste("There are no tiers named", missingTiers))
  }
  
  ##Handle missing interviewer tier
  ##Interviewers can be named either actual name or Interviewer [SpkrCode]
  interviewerTier <- c(
    case_when(
      fileInfo$Neighborhood=="HD" ~ "Trista Pennington", 
      fileInfo$SpkrCode %in% c("CB02", "CB18") ~ "Jennifer Andrus",
      TRUE ~ "Barbara Johnstone"),
    paste("Interviewer", fileInfo$SpkrCode))
  ##Detect missing interviewer tier
  if (!any(interviewerTier %in% tierInfo$TIER_ID)) {
    issues <- c(issues, paste("There are no tiers named either", 
                              ##Format for printing
                              paste(interviewerTier, collapse=" or ")))
  }
  
  ##Handle missing TIER_ID
  noTIER_ID <-
    tierInfo %>% 
    filter(!ExistsTIER_ID)
  if (identical(noTIER_ID, tierInfo)) {
    issues <- c(issues, "All tiers missing a tier name")
  } else if (nrow(noTIER_ID) > 0) {
    issues <- c(issues, 
                paste("Tier", noTIER_ID$TIER_ID, "missing a tier name"))
  }
  
  ##Handle missing PARTICIPANT
  noPARTICIPANT <-
    tierInfo %>% 
    filter(!ExistsPARTICIPANT)
  if (identical(noPARTICIPANT, tierInfo)) {
    issues <- c(issues, "All tiers missing a participant attribute")
  } else if (nrow(noPARTICIPANT) > 0) {
    issues <- c(issues, 
                paste("Tier", noPARTICIPANT$TIER_ID, 
                      "missing a participant attribute"))
  }
  
  ##Handle mismatched TIER_ID & PARTICIPANT
  bothAttr <- 
    tierInfo %>% 
    filter(if_all(starts_with("Exists")))
  ##Only check tiers that have both
  if (nrow(bothAttr) > 0) {
    mismatchAttr <-
      bothAttr %>% 
      filter(!TierIDisParticipant)
    if (nrow(mismatchAttr) > 0) {
      issues <- c(issues,
                  paste0("Mismatched tier name (", mismatchAttr$TIER_ID,
                         ") & participant attribute (",
                         mismatchAttr$PARTICIPANT, ")"))
    }
  }
  
  ##Return issues (if none found, this is an empty vector)
  issues
}


## Dictionaries ===============================================================

##Possible dictionary paths
##Get Unisyn dictionary filepath
##N.B. unisynDict.txt is gitignored for copyright purposes, but its entries get
##  packaged in the deployed Shiny app
dictOptions <- c(Unisyn = "dict/unisynDict.txt",
                 local = "../APLS/files/custom-dictionary/aplsDict.txt",
                 remote = "https://github.com/djvill/APLS/raw/main/files/custom-dictionary/aplsDict.txt")
##Include Unisyn
dictFiles <- dictOptions["Unisyn"]

##Add APLS dictionary, either locally (Dan's machine only) or from GitHub
if (inclLocalDict && onlyLocalDict) {
  stop("Choose either inclLocalDict or onlyLocalDict, not both")
}
if (inclLocalDict || onlyLocalDict) {
  if (!file.exists(dictOptions["local"])) {
    stop("Can't include local dict because it doesn't exist")
  }
  dictFiles <- c(dictFiles, dictOptions["local"])
}
if (!onlyLocalDict) {
  dictFiles <- c(dictFiles, dictOptions["remote"])
}

##Import dictionary(ies)
dict <-
  dictFiles %>% 
  map(~ .x %>% 
        readLines() %>% 
        ##Ignore lines starting with "#" or empty lines
        str_subset("^(#.*|)$", negate=TRUE) %>% 
        ##Strip possessive 's clitic
        ##  N.B. If X's is in the custom dictionary, LaBB-CAT correctly finds
        ##  the phonemic representation for X
        str_remove("'s$") %>% 
        unique())

##Print dict info to R console
dictInfo <- 
  dict %>% 
  map_int(length) %>%
  {str_glue("{names(.)} ({.} entries)")} %>%
  paste(collapse=", ")
message("Dictionaries: ", dictInfo)
##Info on extra local dict entries
if (inclLocalDict) {
  localNotRemote <- setdiff(dict$local, dict$remote)
  locInfo <- if_else(length(localNotRemote)==0,
                     "No additional entries in local w/rt remote",
                     paste("Additional entries in local w/rt remote:",
                           paste(localNotRemote, collapse=" ")))
  message(locInfo)
}

##Put dictionary together
dict <- dict %>% 
  ##As single character vector
  reduce(c) %>% 
  ##Unique
  unique()
message(length(dict), " total unique entries")


##Given a transcription object, outputs a list (one element per tier), each
##  element containing a character vector of out-of-dictionary words
##Tiers without any out-of-dictionary words don't get an element
dictIssuesOneFile <- function(x, noDictCheckTiers=NULL, dict=NULL,
                              pronChars=NULL, permitAngleBrackets=FALSE, 
                              caseSens=FALSE) {
  library(stringr)
  library(purrr)
  library(dplyr)
  
  ##Check args
  if (!inherits(x, "transcription")) {
    stop("x must be an object of class transcription")
  }
  stopifnot(is.character(noDictCheckTiers))
  stopifnot(is.character(dict))
  if (!is.character(noDictCheckTiers)) {
    stop("noDictCheckTiers must be a character vector")
  }
  if (!is.character(dict)) {
    stop("dict must be a character vector")
  }
  
  ##Tokenizing function
  tokenize <- function(x) {
    x %>% 
      ##Unstrand valid punctuation within angle brackets
      str_replace_all("([[:alpha:]]~?) ([?.-])>", "\\1\\2>") %>% 
      ##Ignore text within curly braces (comments about speech or behavior)
      str_remove_all("\\{[^{]*?\\}") %>%
      ##Ignore text within brackets, not counting pronounce codes
      str_remove_all("(?<= )\\[.*?\\]") %>% 
      str_remove_all("^\\[[^\\[]*?\\]") %>% 
      ##Remove extra whitespace
      str_trim("both") %>% 
      ##Separate into words (as a character vector)
      str_split("\\s+") %>% 
      flatten_chr()
  }
  
  ##Get all lines in non-ignored tiers
  lineDF <- 
    x %>% 
    discard_at(noDictCheckTiers) %>% 
    map("Text") %>% 
    tibble(Tier = names(.), Line = .)
  ##Get all unique words
  wordDF <- 
    lineDF %>% 
    mutate(Word = map(Line, tokenize),
           .keep="unused") %>% 
    unnest(Word) %>% 
    distinct()
  
  ##Optionally strip matched angle brackets (single-word interruptions)
  if (permitAngleBrackets) {
    wordDF <- wordDF %>% 
      mutate(across(Word, ~ str_replace(.x, "^<(.+)>$", "\\1"))) %>% 
      distinct()
  }
  
  ##Single-word ignores
  wordDF <- wordDF %>% 
    filter(
      ##Words with valid bracket pronounce codes (hesitations, sui generis 
      ##  words)
      !str_detect(Word, paste0("^[[:alpha:]']+~?\\[", pronChars, "+\\]$")),
      ##Standalone valid punctuation
      !str_detect(Word, "^[.?-]$"),
      !str_detect(Word, "^--$"),
      ##Empty words
      Word!=""
    )
  
  ##Get form of words for checking
  wordDF <- wordDF %>% 
    mutate(CheckWord = Word %>%
             # ##Strip attached valid punctuation
             # str_remove("(\\s[.?-]|--)$") %>%
             ##For words with paren codes, use the paren code for checking
             str_replace(".+\\((.+)\\)$", "\\1") %>%
             ##Strip clitics for checking
             str_remove_all("'(d|ll|ve|s)") %>%
             str_replace("s'$", "s")
    )
  
  ##Optionally convert checking form to lowercase
  if (!caseSens) {
    wordDF <- wordDF %>% 
      mutate(across(CheckWord, str_to_lower))
    dict <- str_to_lower(dict)
  }
  
  ##Identify words that aren't in the dictionary in *either* form
  wordDF <- wordDF %>%
    filter(!if_any(-Word, ~ .x %in% dict))
  
  ##Return list of out-of-dictionary words
  wordDF %>% 
    select(Tier, Word) %>% 
    chop(Word) %>% 
    pull(Word, Tier)
}


## Overlaps ===================================================================

## See eaf-utils.R:
## - getOverlapTiers()
## - getTimesTier()
## - getTimes()
## - xmllist_to_df()

##Function that takes a single tier name and a nested list of annotation time
##  DFs (meant to be used with output of getTimes()), and outputs a single
##  dataframe: the relevant annotation time DF plus three boolean overlaps
##  columns (left, right, both), where TRUE means there is at least one
##  annotation on another tier whose (e.g.) left boundary is within the
##  annotation
findOverlapsTier <- function(timesTier, tierName, timesEAF) {
  library(dplyr)
  library(tidyr)
  
  ##Pull timing dataframes
  timesOtherTiers <- 
    timesEAF[names(timesEAF)!=tierName] %>% 
    bind_rows()
  
  ##For each boundary in selected tier, return the first annotation that the
  ##  boundary overlaps with, if any
  ##Currently isn't guaranteed to work with 3+ speaker tiers (because
  ##  which(x)[1] only selects the first result, not necessarily the result
  ##  closest to the boundary)
  bounds <- 
    timesTier %>%
    ##One row per boundary (new columns Side, TIME_SLOT_REF, Time)
    rename(Time1 = Start, Time2 = End) %>% 
    pivot_longer(-ANNOTATION_ID, 
                 names_to=c(".value", "Side"), names_pattern="(.+)([12])") %>% 
    ##Check row-by-row (otherwise comparison in which() doesn't work)
    rowwise() %>% 
    ##Add overlap annotation ID
    ##N.B. This works because if which(x) is integer(0), which(x)[1] is NA
    mutate(ANNOTATION_ID_overlapped = timesOtherTiers$ANNOTATION_ID %>% 
             extract(which(Time > timesOtherTiers$Start & Time < timesOtherTiers$End)[1])) %>% 
    ungroup()
  
  ##Restrict to boundaries with overlaps, and add information about overlapped
  ##  annotations
  overlapBounds <- 
    bounds %>%
    ##Only the boundaries that overlap another annotation
    filter(!is.na(ANNOTATION_ID_overlapped)) %>% 
    ##Add info about overlapped annotations
    left_join(timesOtherTiers %>%
                rename_with(~ paste0(.x, "_overlapped")),
              by="ANNOTATION_ID_overlapped")
  
  ##Only proceed if there are any overlaps
  if (nrow(overlapBounds) > 0) {
    ##Add information about overlapped annotations
    overlapBounds <- overlapBounds %>% 
      ##Determine whether the nearest boundary is close enough (rowwise for min())
      rowwise() %>%
      mutate(StartDiff = abs(Start_overlapped - Time),
             EndDiff = abs(End_overlapped - Time),
             CloseEnough = min(StartDiff, EndDiff) < overlapThresh) %>%
      ungroup() %>% 
      ##Add new timeslot ID: if not close enough, "Too far"; if close enough, closer
      ##  boundary (tie goes to start boundary); anything else is unexpected so it
      ##  triggers an error below
      mutate(NewTS = case_when(
        !CloseEnough ~ "Too far",
        StartDiff <= EndDiff ~ TIME_SLOT_REF1_overlapped,
        StartDiff > EndDiff ~ TIME_SLOT_REF2_overlapped,
        TRUE ~ NA_character_),
        ##Add node path for fixing overlap
        NodePath = str_glue("//ALIGNABLE_ANNOTATION[@ANNOTATION_ID='{ANNOTATION_ID}']"))
  } else {
    ##If no overlaps, add empty columns anyway
    overlapBounds <- overlapBounds %>% 
      mutate(StartDiff = double(0L),
             EndDiff = double(0L),
             CloseEnough = logical(0L),
             NewTS = character(0L),
             NodePath = character(0L))
  }
  
  ##Return dataframe (which may be 0-row)
  overlapBounds
}

##Check for problems with overlapBounds, fix any overlaps by modifying eaflist(),
##  and return overlapBounds with info about fixed boundaries
fixOverlapsTier <- function(overlapBounds, eaflist, eafName) {
  library(dplyr)
  library(purrr)
  library(xml2)
  library(tidyr)
  library(stringr)
  
  ##Check for boundaries where NewTS is NA
  if (any(is.na(overlapBounds$NewTS))) {
    stop("On tier ", tierName, 
         ", at least one boundary returned an error: ",
         overlapBounds %>% 
           filter(is.na(NewTS)) %>% 
           pull(ANNOTATION_ID) %>% 
           paste(collapse=" "))
  }
  
  ##Get fixed boundaries only
  overlapBoundsFixed <- 
    overlapBounds %>% 
    filter(NewTS != "Too far")
  
  ##If no boundaries fixed, return dataframe with all unresolved
  if (nrow(overlapBoundsFixed)==0) {
    overlapBounds <- overlapBounds %>% 
      mutate(Resolved = FALSE)
    return(overlapBounds)
  }
  
  ##Otherwise, check for issues in overlaps
  ##Check for annotation IDs that don't select a unique node within EAF file
  nodeCount <- 
    overlapBoundsFixed$NodePath %>% 
    map_int(~ xml_find_all(eaflist[[eafName]], .x) %>% 
              xml_length())
  if (any(nodeCount > 1)) {
    stop("On tier ", tierName, 
         ", at least one annotation ID doesn't select a unique node: ",
         overlapBoundsFixed %>% 
           filter(NodeCount > 1) %>% 
           pull(ANNOTATION_ID) %>% 
           paste(collapse=" "))
  }
  
  ##Check for annotations that have accidentally become 0-width
  ##This could happen if the annotation is less than overlapThresh wide and
  ##  is fully contained within another annotation on another tier. Could
  ##  mitigate this by re-running with a smaller overlapThresh
  if (!is.null(checkZeroWidth) && checkZeroWidth=="error") {
    zeroWidth <- 
      overlapBoundsFixed %>%
      ##Remove annotations that only have one boundary in overlapBoundsFixed
      ##This happens when an overlapping annotation has a boundary that lines up
      ##  exactly with another tier (due to findOverlapsTier()'s code for creating
      ##  ANNOTATION_ID_overlapped)
      ##But this doesn't appear to be working as of now
      group_by(ANNOTATION_ID) %>% 
      filter(n()==2) %>% 
      ungroup() %>% 
      ##One row per annotation
      select(ANNOTATION_ID:Time, NewTS) %>%
      pivot_wider(names_from=Side, names_glue="{.value}{Side}",
                  values_from=TIME_SLOT_REF:NewTS) %>%
      ##Only annotations that are zero-width
      filter(NewTS1==NewTS2)
    
    ##Error out for zero-width
    if (nrow(zeroWidth) > 0) {
      stop("On tier ", tierName, 
           ", at least one annotation was fixed to zero width:\n",
           "ANN_ID  Start   End\n",
           paste0(str_pad(zeroWidth$ANNOTATION_ID, 8, 'right'),
                  str_pad(round(zeroWidth$Time1 / 1000, 1), 8, 'right'),
                  str_pad(round(zeroWidth$Time2 / 1000, 1), 8, 'right'),
                  "\n"))
    }
  }
  
  ##Fix overlaps in eaflist()
  overlapBoundsFixed %>% 
    rowwise() %>% 
    group_walk(
      ~ eaflist %>% 
        pluck(eafName) %>% 
        xml_find_first(.x$NodePath) %>%
        ##Note xml_set_attr() modifies without assignment (!!!)
        xml_set_attr(paste0("TIME_SLOT_REF", .x$Side), .x$NewTS))
  
  ##Add fixed info to dataframe
  overlapBounds <- overlapBounds %>% 
    mutate(Resolved = NewTS!="Too far")
  
  ##Return dataframe
  overlapBounds
}

##Wrapper function around fixOverlapsTier() that takes a *list* of DFs of
##  annotation times (one file's worth) and a single EAF name (meant to be used
##  with output of getTimes() and imap()), and rotates through tiers, fixing
##  overlaps, until it reaches a stable state; modifies original eaflist
##Feeds into overlapsIssues()
fixOverlaps <- function(tierNamesFile, eafName, eaflist) {
  library(purrr)
  library(dplyr)
  
  ##Get initial timing data
  timesEAF <- getTimes(eaflist %>% pluck(eafName), tierNamesFile)
  ##If just one tier, skip overlap-checking for this file
  if (length(timesEAF)==1) {
    return(data.frame(ANNOTATION_ID = character(0L), 
                      Tier = character(0L), 
                      Side = integer(0L), 
                      Time = double(0L)))
  }
  
  ##Get initial overlaps
  ##  findOverlapsTier() needs a single tier's times DF, the name of that tier,
  ##  and the entire file's times DF
  overlapsInit <- imap(timesEAF, findOverlapsTier, timesEAF=timesEAF)
  
  ##If there's a blank tier, tierNamesFile won't have it, so just use 
  ##  overlapsInit names
  tierNamesFile <- names(overlapsInit)
  
  ##Initialize looping variables
  overlapsPre <- NULL
  overlapsPost <- overlapsInit
  iters <- 0
  maxIter <- 10
  
	if (all(map_int(overlapsPost, nrow)==0)) {
    message("No initial overlaps.")
  }
	
  ##Continue until there are no remaining overlaps, either because all
  ##  overlaps have been fixed, or because things have stablized
  while (any(map_int(overlapsPost, nrow) > 0) && !identical(overlapsPre, overlapsPost)) {
    ##Increment iteration counter & stop if above maxIter
    iters <- iters + 1
    if (iters > maxIter) {
      stop("Overlap fixing reached max iterations (", maxIter, ")")
    }
    
    ##Old post is new pre
    overlapsPre <- overlapsPost
    
    ##Fix each tier in order
    for (tier in rev(tierNamesFile)) {
      ##Re-assess overlaps now that eaflist has been modified
      newTimesEAF <- 
        eaflist %>%
        pluck(eafName) %>% 
        getTimes(tiers=tierNamesFile)
      overlapsCurr <- imap(newTimesEAF, findOverlapsTier, timesEAF=newTimesEAF)
      
      ##Hide overlaps in all other tiers from fixOverlapsTier()
      for (otherSpkr in setdiff(names(overlapsPre), tier)) {
        overlapsCurr <- overlapsCurr %>% 
          modify_in(list(otherSpkr), ~ .x %>% filter(is.na(ANNOTATION_ID)))
      }
      
      ##Fix overlaps for *other* tiers, keeping current tier
      overlapsPost[[tier]] <-
        overlapsCurr %>% 
        map(fixOverlapsTier, eaflist, eafName) %>% 
        pluck(tier) %>% 
        filter(!Resolved)
			
			message("iters: ", iters)
      message("nrow(overlapsPre): ", map_int(overlapsPre, nrow) %>% paste(collapse=" "))
      message("nrow(overlapsPost): ", map_int(overlapsPost, nrow) %>% paste(collapse=" "))
    }
  }
  
  ##Optionally remove 0-width turns from eaflist
  if (!is.null(checkZeroWidth) && checkZeroWidth=="drop") {
    ##Get updated times
    newestTimes <- 
      eaflist %>%
      pluck(eafName) %>% 
      getTimes(tiers=tierNamesFile)
    ##Get annotation IDs of 0-width turns
    noWidth <- 
      newestTimes %>% 
      map(~ .x %>% 
            filter(Start==End) %>% 
            pull(ANNOTATION_ID)) %>% 
      reduce(c)
    
    ##Xpath to select parent ANNOTATION node of each 0-width node
    noWidthXpath <-
      paste0("//ALIGNABLE_ANNOTATION[@ANNOTATION_ID='", noWidth, "']/..", 
             collapse=" | ")
    ##Remove turns
    eaflist %>% 
      pluck(eafName) %>% 
      xml_find_all(noWidthXpath) %>% 
      xml_remove()
  }
  
  ##Return overlapsPost
  overlapsPost
}

##Format millisecond times as HH:MM:SS.SSS
formatTimes <- function(time, type=c("S","HMS")[2]) {
  time <- time / 1000
  if (type=="S") {
    round(time, 3)
  } else if (type=="HMS") {
    time <- c((time %/% 60) %/% 60,
              (time %/% 60) %% 60,
              time %% 60)
    paste(sprintf("%02i", time[1]), sprintf("%02i", time[2]),
          sprintf("%06.3f", time[3]), sep=":")
  }
}

##Fix overlaps in all files, returning any unresolved overlaps, formatted
##  nicely for printing
##x should be eaflist(), df should be tierDF() (passed down to fixOverlaps())
overlapsIssues <- function(x, df, inclRedact=TRUE) {
  library(purrr)
  library(dplyr)
  library(tidyr)
  
  ##Get list of each file's tier names to check
  tierNames <- getOverlapTiers(df, inclRedact)
  
  ##Get initial timing data
  times <- list(eaf = x,
                tiers = tierNames) %>%
    pmap(getTimes)
  
  ##If all files have just one tier, skip overlap-checking
  nTiers <- map_int(times, length)
  if (all(nTiers==1)) {
    return(list())
  }
  
  ##Fix overlaps & format output for display
  fixed <- 
    tierNames %>% 
    ##Fix overlaps
    imap(fixOverlaps, eaflist=x) %>% 
    ##Get fixed-overlap times
    map(~ .x %>% 
          ##All in one DF
          bind_rows(.id="Tier") %>%
          ##One row per annotation (nicer labels)
          select(ANNOTATION_ID, Tier, Side, Time) %>%
          mutate(Side = if_else(Side==1, "Start", "End")) %>%
          pivot_wider(names_from=Side, values_from=Time))
  
  ##Remove empty entries from times and fixed
  nonempty <- map_int(fixed, nrow) > 0
  times <- times[nonempty]
  fixed <- fixed[nonempty]
  
  ##Add empty End columns to fixed if needed
  ##This circumvents an error in weird edge casess
  fixed <- fixed %>% 
    map_if(~ !("End" %in% colnames(.x)),
           ~ .x %>% mutate(End = NA_real_))
  
  ##If any NAs in Start/End, patch rows
  if (any(fixed %>% 
          map_lgl(~ .x %>% 
                  select(Start, End) %>% 
                  is.na() %>% 
                  any()))) {
    ##Get just relevant annotations (otherwise rows_patch() will complain)
    times <- map2(times, fixed,
                  ~ .x %>%
                    bind_rows() %>%
                    select(ANNOTATION_ID, Start, End) %>%
                    semi_join(.y, "ANNOTATION_ID"))
    
    ##Add missing timestamps
    fixed <- map2(fixed, times, rows_patch, by="ANNOTATION_ID")
  }
  
  ##Sort by start time, remove annotation ID, nicer Start/End formatting, & return
  fixed %>%
    map(~ .x %>%
          arrange(Start) %>%
          select(-ANNOTATION_ID) %>%
          rowwise() %>%
          mutate(across(c(Start,End), ~ formatTimes(.x, timeDisp))))
}

## UI convenience functions ===================================================

##Convenience functions to display/undisplay HTML elements
display <- function(x) {
  library(stringr)
  
  if (!("shiny.tag" %in% class(x))) {
    stop("display() only works with shiny.tag objects, not ", 
         paste(class(x), collapse="/"), " objects.")
  }
  
  if (is.null(x$attribs$style)) {
    x$attribs$style <- "display: inherit;"
  } else {
    x$attribs$style <- x$attribs$style %>% 
      str_remove("display: \\w+;") %>%
      paste0("display: inherit;")
  }
  
  x
}
undisplay <- function(x) {
  library(stringr)
  
  if (!("shiny.tag" %in% class(x))) {
    stop("undisplay() only works with shiny.tag objects, not ", 
         paste(class(x), collapse="/"), " objects.")
  }
  
  if (is.null(x$attribs$style)) {
    x$attribs$style <- "display: none;"
  } else {
    x$attribs$style <- x$attribs$style %>% 
      str_remove("display: \\w+;") %>%
      paste0("display: none;")
  }
  
  x
}
is.displayed <- function(x) {
  if (!any(c("shiny.tag") %in% class(x))) {
    stop("is.displayed() only works with shiny.tag objects, not ", 
         paste(class(x), collapse="/"), " objects.")
  }
  
  sty <- x$attribs$style
  !grepl("display:\\s*none", sty)
}



# Server ------------------------------------------------------------------
server <- function(input, output) {
  # Set up file structures --------------------------------------------------
  
  ##Read files: Get a list with one element per file. Each element is either an
  ##  xml_document (if read_xml() succeeded) or the error message thrown by 
  ##  read_xml()
  eaflist <- eventReactive(input$files, {
    message("Uploaded ", nrow(input$files), " files:\n",
            str_flatten(paste("-", input$files$name), "\n"))
    input$files %>% 
      pull(datapath, name) %>% 
      ##Get safely() list of results and errors
      map(safely(read_xml)) %>% 
      ##Turn into a list of results *or* errors
      map_if(~ !is.null(.x$result), "result", .else="error")
  })
  
  ##Convert eaflist to dflist
  dflist <- reactive({ 
    eaflist() %>%
      keep(~ "xml_document" %in% class(.x)) %>% 
      map(~ eaf_to_df_list(.x, annotation_metadata=TRUE))
  })
  
  # Debugging output --------------------------------------------------------
  ##Wrapper to include verbatim debugging text in UI or UI elements
  output$debug <- renderUI({
    out <- verbatimTextOutput("debugPrint")
    
    ##Optionally display or undisplay
    if (showDebug) {
      display(out)
    } else {
      undisplay(out)
    }
  })
  
  ##Verbatim debugging text (to 'peek into' environment)
  output$debugPrint <-
    renderPrint({
      list(
        ##To use:
        ## 1. Set showDebug to TRUE
        ## 2. Put reactive objects here with name from environment or expression, such as
        ##      `eaflist()` = eaflist()
        ##      `tierDF()$TIER_ID` = tierDF()$TIER_ID,
        ##    or if that's too cumbersome, just give it a temporary name, such as
        ##      jon = tierDF()$TIER_ID %>% 
        ##        unique()
        # `overlapsIssues(eaflist(), tierDF())` = overlapsIssues(eaflist(), tierDF())
      )
    })
  
  ##Export test values (packed away in 1+ <div>s)
  ##To unpack in shinytest, use the following: 
  ##  app <- ShinyDriver$new()
  ##  fileDF <- ##for example
  ##    app$getAllValues() %>%
  ##    pluck("output", "export", "html") %>%
  ##    read_html() %>%
  ##    rvest::html_element("#fileDF") %>% 
  ##    html_text() %>% 
  ##    jsonlite::fromJSON()
  output$export <- renderUI({
    ##Package a value into a <div>
    pack_val <- function(x, nm) {
      require(jsonlite)
      undisplay(div(prettify(toJSON(x), 2), id=nm))
    }

    ##First element: dataframe of file info
    ##N.B. This is different than fileInfo, which is for a single element in 
    ##  dflist
    fileDF <-
      eaflist() %>%
      names() %>% 
      parse_filenames()
    export <- tagList(pack_val(fileDF, "fileDF"))
    
    ##Add the rest of the data
    validationDF <- validateEaflist(eaflist())
    fileCheckValid <- all(validationDF$Valid)
    if (!fileCheckValid) { 
      ##If step 0 failed, add validationDF
      export <- c(export, tagList(pack_val(validationDF, "validationDF")))
    } else {
      ##If step 0 passed, add tier info & at least one eaflist()
      tierDF <- map(dflist(), tier_metadata)
      turnsDF <- 
        dflist() %>% 
        map_dfr(~ .x %>% 
                  map_dfr(as_tibble, .id="Tier"),
                .id="File") %>% 
        select(-Text)
      export <- c(export, 
                  tagList(pack_val(tierDF, "tierDF")),
                  tagList(pack_val(turnsDF, "turnsDF")))
    }
    
    export
  })
  
  # Output: UI --------------------------------------------------------------
  output$out <- renderUI({
    ##Always-displayed headings
    stepHeads <- list(tiers = h2("Step 1: Validating tier names and attributes...",
                                 id="tierHead"),
                      dict = h2("Step 2: Checking for out-of-dictionary words...",
                                id="dictHead"),
                      overlaps = h2("Step 3: Checking for overlaps...",
                                    id="overlapsHead"))
    
    ##If no uploaded files, just display headings
    if (!isTruthy(input$files)) {
      return(tagList(
        h1("Waiting for uploaded files..."), 
        stepHeads))
    }
    
    ##If uploaded files, initialize exitEarly sentinel & proceed to checking steps
    exitEarly <- FALSE
    
    
    # Step 0: File check -----------------------------------------------------
    fileCheckHead <- h1("Checking the following files...",
                        id="fileCheckHead")
    
    ##Get validation info
    validationDF <- validateEaflist(eaflist())
    
    ##Get bullet-list of filenames (styling bad ones in red)
    fileList <- tags$ul(
      validationDF %>% 
        mutate(Class = if_else(Valid, "", "bad")) %>% 
        pull(Class, name) %>% 
        imap(~ tags$li(.y, class=.x)),
      id="fileList", class="details"
    )
    
    ##Style headings based on whether filenames are valid
    fileCheckValid <- all(validationDF$Valid)
    if (fileCheckValid) {
      fileCheckSubhead <- h3("", id="fileCheckSubhead") %>% 
        undisplay()
      message("Step 0: Pass")
    } else {
      stepHeads <- stepHeads %>%
        map(tagAppendAttributes, class="grayout")
      
      ##Extra-informative error message
      fileNameTips <- character(0L)
      if (any(!validationDF$SpkrCodeValid)) {
        fileNameTips <- c(fileNameTips, "begin with a speaker code")
      }
      if (any(!validationDF$FileExtValid)) {
        fileNameTips <- c(fileNameTips, "end with the .eaf file extension")
      }
      if (any(!validationDF$FileReadValid)) {
        fileNameTips <- c(fileNameTips, "be readable by Elan")
      }
      if (length(fileNameTips) > 1) {
        fileNameTips <- paste0("(", seq_along(fileNameTips), ") ", fileNameTips)
      }
      fileCheckSubhead <- h3(paste0("Files must ", 
                                    str_flatten_comma(fileNameTips, ", and "),
                                    "."), 
                            id="fileCheckSubhead") %>% 
        display()
      
      ##Edge case: Throw error if !fileCheckValid but no fileNameTips
      stopifnot(length(fileNameTips) > 0)
      
      ##Exit early & message
      exitEarly <- TRUE
      message("Step 0: Fail")
    }
    
    
    # Step 1: Tier check ------------------------------------------------------
    ##Content
    tierSubhead <- h3(paste("The tier checker returned the following issue(s),",
                            "which can be resolved in Elan using Set Author and/or Change Tier Attributes:"),
                      id="tierSubhead") %>% 
      ##By default, don't display
      undisplay()
    tierDetails <- tags$ul("", id="tierDetails", class="details") %>% 
      ##By default, don't display
      undisplay()
    
    ##If not exiting early yet, check for tier issues
    if (!exitEarly || overrideExit$fileName) {
      ##Get tier issues
      tierIss <- dflist() %>% 
        imap(tierIssuesOneFile, 
             nonSpkrTiers=nonSpkrTiers, prohibTiers=prohibTiers) %>% 
        keep(~ length(.x) > 0)
      ##If no tier issues, don't display anything & make step heading green
      if (length(tierIss)==0) {
        tierSubhead <- undisplay(tierSubhead)
        tierDetails <- undisplay(tierDetails)
        stepHeads$tiers <- stepHeads$tiers %>% 
          tagAppendAttributes(class="good")
        message("Step 1: Pass")
      } else {
        ##If tier issues, display issues in nested list
        tierSubhead <- display(tierSubhead)
        tierDetails <- tierDetails %>%
          display() %>% 
          tagAppendChild(
            ##For each element in tierIss (each file with an issue), create a
            ##  bullet-list headed by name of file
            tierIss %>% 
              imap(
                ~ tags$li(
                  paste0("In file ", .y, ":"),
                  tags$ul(
                    map(.x, tags$li),
                    class="file-headed"
                  )))
          )
        
        ##Style headers
        stepHeads$tiers <- stepHeads$tiers %>%
          tagAppendAttributes(class="bad")
        stepHeads[2:3] <- stepHeads[2:3] %>% 
          map(tagAppendAttributes, class="grayout")
        
        ##Exit early & message
        exitEarly <- TRUE
        message("Step 1: Fail")
      }
    }
    
    
    # Step 2: Dictionary check ------------------------------------------------
    ##Content
    dictSubhead <- h3("The following word(s) are not currently in the corpus dictionary.", 
                      "These could be:",
                      tags$ul(
                        tags$li(
                          strong("Misspellings/typos:"),
                          "Check commonly misspelled words like", em("embarrassed"),
                          "or", em("conscious")
                        ),
                        tags$li(
                          strong("Incorrect punctuation:"),
                          "For example,", em("that."), "(need a space between word and period)",
                          "or", em("bec~(bIk)"), "(pronounce code goes in [], not ())"
                        ),
                        tags$li(
                          strong("Incorrect DISC characters:"),
                          "Make sure you're not using any characters that aren't in the", 
                          a("DISC phonemic alphabet",
                            href="https://djvill.github.io/APLS/doc/phonemic-transcription",
                            target="_blank"),
                          "(e.g., IPA /j/ is DISC", code("j", .noWS="after"), 
                          ", not ", code("y", .noWS="after"), ")"
                        ),
                        tags$li(
                          strong("Words that should be in APLS's phonemic dictionary:"),
                          "To look up phonemic representations, APLS uses: (1) the Unisyn English dictionary, and (2)",
                          a("custom entries", 
                            href="https://djvill.github.io/APLS/files/custom-entries",
                            target="_blank",
                            .noWS="after"),
                          ". If any words should be added to the dictionary, send suggestions to Dan, including ",
                          a("DISC representations",
                            href="https://djvill.github.io/APLS/doc/phonemic-transcription.html#suggesting-new-dictionary-entries",
                            target="_blank",
                            .noWS="after"),
                          ". If you're not sure whether a word should be added to the dictionary, ask Dan"
                        ),
                        tags$li(
                          strong("Words that need an inline pronounce code (aka DISC code):"),
                          "Words made up on the spot (e.g., ", em("yinzerific", .noWS="after"), "),",
                          "words unlikely to come up in any other interview (e.g., a specific schoolteacher's name),",
                          "obvious misprounciations (e.g., \"havring\" for", em("having", .noWS="after"), "),",
                          "and hesitations (e.g., \"hesi~\")"
                        )
                      ),
                      id="dictSubhead") %>% 
      ##By default, don't display
      undisplay()
    dictDetails <- tags$ul("", id="dictDetails", class="details") %>% 
      ##By default, don't display
      undisplay()
    
    ##If not exiting early yet, check for dictionary issues
    if (!exitEarly || overrideExit$tiers) {
      ##Get dictionary check
      dictIss <- dflist() %>% 
        map(dictIssuesOneFile, noDictCheckTiers=noDictCheckTiers, dict=dict, 
            pronChars=pronChars, permitAngleBrackets=permitAngleBrackets, 
            caseSens=caseSens) %>% 
        keep(~ length(.x) > 0)
      ##If no dictionary issues, don't display anything & make step heading green
      if (length(dictIss)==0) {
        dictSubhead <- undisplay(dictSubhead)
        dictDetails <- undisplay(dictDetails)
        stepHeads$dict <- stepHeads$dict %>% 
          tagAppendAttributes(class="good")
        message("Step 2: Pass")
      } else {
        ##If dict issues, display issues in nested list
        dictSubhead <- display(dictSubhead)
        dictDetails <- dictDetails %>%
          display() %>% 
          tagAppendChild(
            ##For each element in dictIss (each file with an issue), create a
            ##  bullet-list headed by name of file, with a nested bullet-list
            ##  headed by name of tier
            dictIss %>%
              imap(
                ~ tags$li(
                  paste0("In file ", .y, ":"),
                  tags$ul(
                    imap(.x,
                         ~ tags$li(
                           paste0("On tier ", .y, ":"),
                           tags$ul(
                             map(.x, tags$li),
                             class="tier-headed"
                           )
                         )),
                    class="file-headed"
                  ))
              )
          )
        
        ##Style headers
        stepHeads$dict <- stepHeads$dict %>% 
          tagAppendAttributes(class="bad")
        stepHeads[3] <- stepHeads[3] %>% 
          map(tagAppendAttributes, class="grayout")
        
        ##Exit early & message
        exitEarly <- TRUE
        message("Step 2: Fail")
      }
    }
    
    
    # Step 3: Overlaps check --------------------------------------------------
    ##Content
    overlapsSubhead <- h3(paste("The overlap checker could not resolve the following overlaps.",
                                "Please fix these overlaps; remember to make overlaps a",
                                "separate turn on each speaker's tier."),
                          id="overlapsSubhead") %>% 
      ##By default, don't display
      undisplay()
    overlapsDetails <- tags$ul("", id="overlapsDetails", class="details") %>% 
      ##By default, don't display
      undisplay()
    
    ##If not exiting early yet, check for dictionary issues
    if (!exitEarly || overrideExit$dict) {
      ##Get overlaps issues
      overlapsIss <- overlapsIssues(eaflist(), tierDF(), fixOverlapRedact)
      ##If no overlaps issues, don't display anything & make step heading green
      if (length(overlapsIss)==0) {
        overlapsSubhead <- undisplay(overlapsSubhead)
        overlapsDetails <- undisplay(overlapsDetails)
        stepHeads$overlaps <- stepHeads$overlaps %>% 
          tagAppendAttributes(class="good")
        message("Step 3: Pass")
      } else {
        ##If overlaps issues, display issues in nested list
        overlapsSubhead <- display(overlapsSubhead)
        overlapsDetails <- overlapsDetails %>%
          display() %>% 
          tagAppendChild(
            ##For each element in overlapsIss (each file with an issue), create a
            ##  bullet-list headed by name of file, with a nested bullet-list
            ##  headed by name of tier
            overlapsIss %>%
              imap(
                ~ tags$li(
                  paste0("In file ", .y, ":"),
                  tags$table(
                    tags$tr(
                      map(colnames(.x), tags$th)
                    ),
                    .x %>% 
                      rowwise() %>% 
                      group_map(~ tags$tr(map(.x, tags$td))),
                    class="table shiny-table table- spacing-s overlap-table file-headed"
                  )))
          )
        
        ##Style headers
        stepHeads$overlaps <- stepHeads$overlaps %>% 
          tagAppendAttributes(class="bad")
        
        ##Exit early & message
        exitEarly <- TRUE
        message("Step 3: Fail")
      }
    }
    
    
    # Download ----------------------------------------------------------------
    ##Content
    downloadHead <- h1("The file(s) passed all checks. Great job!",
                       id="downloadHead")
    downloadSubhead <- h3("Please download the corrected file(s) and upload to the Completed folder",
                          id="downloadSubhead")
    downloadBtn <- downloadButton("OutputFile", "Download corrected file(s)")
    
    # UI output ---------------------------------------------------------------
    ##Reupload heading
    reuploadHead <- h1("Please fix issues and re-upload.", 
                       id="reuploadHead") %>% 
      undisplay()
    ##Style reupload heading
    if (exitEarly) {
      reuploadHead <- display(reuploadHead)
      downloadHead <- undisplay(downloadHead)
      downloadSubhead <- undisplay(downloadSubhead)
      downloadBtn <- undisplay(downloadBtn)
    } else {
      reuploadHead <- undisplay(reuploadHead)
      downloadHead <- display(downloadHead)
      downloadSubhead <- display(downloadSubhead)
      downloadBtn <- display(downloadBtn)
    }
    
    ##PAGE FUNCTIONALITY:
    ##At page load, all that's visible in mainPanel is a temporary h2 "Waiting
    ##  for uploaded files..." and stepHeads (tagList below is bypassed)
    ##Once file(s) are uploaded, below tagList is displayed
    ##If a step passes, stepHead styled as .good (except step 0, which only
    ##  displays if failed)
    ##If a step fails, stepHead styled as .bad, and subhead+details display
    ##  (plus reuploadHead)
    ##If a step is not reached, stepHead styled as .grayout
    
    ##Construct tag list
    tagList(
      ##"Checking the following files" and bullet-list (styling bad files)
      fileCheckHead,
      fileList,
      ##Step 0: Check file names (only displays if failed)
      fileCheckSubhead,
      ##Step 1: Check tiers
      stepHeads$tiers,
      tierSubhead,
      tierDetails,
      ##Step 2: Check dictionary
      stepHeads$dict,
      dictSubhead,
      dictDetails,
      ##Step 3: Check & fix overlaps
      stepHeads$overlaps,
      overlapsSubhead,
      overlapsDetails,
      ##If not all files passed, display "Please fix issues and re-upload."
      reuploadHead,
      ##If all files passed, instructions to download
      downloadHead,
      downloadSubhead,
      downloadBtn
    )
  }) ##End output$out <- renderUI({})
  
  
  # Create output file(s) ---------------------------------------------------
  output$OutputFile <- downloadHandler(
    filename=function() {
      if (length(eaflist())==1) {
        names(eaflist())
      } else {
        "corrected_eafs.zip"
      }
    },
    content=function(file) {
      if (length(eaflist())==1) {
        write_xml(eaflist()[[1]], file)
      } else {
        eaflist() %>% 
          iwalk(write_xml)
        zip(file, names(eaflist()))
      }
    }
  )
}

shinyApp(ui = ui, server = server)
