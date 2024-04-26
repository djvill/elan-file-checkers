message("****ELAN-FILE-CHECKERS****\n")

library(shiny)
library(xml2)
library(stringr)
library(purrr)
library(tidyr)
library(dplyr)

source("eaf-utils.R")


# Parameters ------------------------------------------------------------------

##Version
vers <- "1.4.2"

##File structures
##Named list of functions to handle each file extension to be read
readHandlers <- list(eaf = xml2::read_xml,
                     ##read_textgrid() defined in eaf-utils.R
                     textgrid = read_textgrid)
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
                 "NoMatch - Word","NoMatch - Turn","Overlap")            ##From Fill-Batchalign-Words

##Dictionary checking
##Tiers to exclude from dictionary checking
noDictCheckTiers <- c("Comment","Noise","Redaction")
##Include local version of aplsDict.txt? Include ONLY local version?
##  Only works on Dan's machine, useful for testing new entries without
##  committing each time
inclLocalDict <- FALSE
onlyLocalDict <- FALSE
##Permit angle brackets for single-word interruptions?
permitAngleBrackets <- FALSE
##Characters to accept in pronounce codes
pronChars <- "[pbtdkgNmnlrfvTDszSZjhwJ_CFHPIE{VQU@i$u312456789#'\"-]"
##Case-sensitive?
caseSens <- FALSE

##Overlap fixing
##Tiers to exclude from overlap checking/fixing
noOverlapCheckTiers <- c("Comment","Noise")
##Maximum cross-tier misalignment (in ms) to 'snap together'. Set lower to be
##  more conservative about what counts as an intended cross-tier alignment
overlapThresh <- 500
##Overlap-fixing method: old or new
fixMethod <- "old"
##Check for 0-width post-fixing annotations (can cause issues)
##  "error" = throw error if any annotations have been set to 0-width
##  "warn" = throw warning
##  "silent" = silently drop 0-width annotations
##  NULL = don't check
checkZeroWidth <- "silent"
##Maximum number of overlap-fixing iterations to attempt before exiting (must be
##  positive number)
maxIters <- 10
##Behavior when reaching maximum overlap-fixing iterations: "error", "warn",
##  "silent"
reachMaxIters <- "warn"
##Time display type: "S" (seconds, useful for Praat TextGrids), or "HMS" (useful
##  for ELAN)
timeDisp <- "HMS"

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
    mainPanel(uiOutput("export"), ##Never shown
              uiOutput("out"))
  )
)


# Functions for server-side processing ------------------------------------

## File setup =============================================================

## See eaf-utils.R:
## - getTimesTier()
## - getTimes()

##Given a dataframe of file information, generate a dataframe of information on
##  whether each file & filename are valid
##In the app, x is fileDF()
validateFiles <- function(x, validExts=names(readHandlers)) {
  library(dplyr)
  
  if (!is.data.frame(x) || 
      !all(c("name", "FileExt", "SpkrCode", "file") %in% colnames(x))) {
    stop("x must be a data.frame with columns name, FileExt, SpkrCode, file")
  }
  
  x %>% 
    ##Add columns for validation: speaker code initial, valid file extensions,
    ##  whether file was successfully read
    mutate(FileExtValid = tolower(FileExt) %in% tolower(validExts),
           SpkrCodeValid = !is.na(SpkrCode),
           FileReadValid = map_lgl(file, ~ !inherits(.x, "error"))) %>% 
    select(name, ends_with("Valid")) %>% 
    ##File is valid only if it satisfies all conditions
    rowwise() %>% 
    mutate(Valid = all(c_across(-name))) %>% 
    ungroup()
}

## Tiers ==================================================================
##Given a transcription object and filename, output character vector of tier
##  issues
tierIssuesOneFile <- function(x, nm, nonSpkrTiers=NULL, prohibTiers=NULL) {
  library(dplyr)
  library(purrr)
  library(stringr)
  
  if (!inherits(x, c("trs_transcription", "trs_nesttiers"))) {
    stop("x must be an object of classes trs_transcription and trs_nesttiers")
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
  if (!inherits(x, c("trs_transcription", "trs_nesttiers"))) {
    stop("x must be an object of classes trs_transcription and trs_nesttiers")
  }
  if (!is.null(noDictCheckTiers) && !is.character(noDictCheckTiers)) {
    stop("noDictCheckTiers must be NULL or a character vector")
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

overlapsIssuesOneFile <- function(x, nm, 
                                  noOverlapCheckTiers=NULL, overlapThresh=NULL, 
                                  fixMethod=c("old","new"), 
                                  checkZeroWidth=c("error","warn","silent"),
                                  maxIters=NULL, reachMaxIters=c("error","warn","silent")[2],
                                  timeDisp=c("HMS","S")) {
  library(purrr)
  library(dplyr)
  library(tidyr)
  
  ##Check args
  if (!inherits(x, c("trs_transcription", "trs_nesttiers"))) {
    stop("x must be an object of classes trs_transcription and trs_nesttiers")
  }
  if (!is.character(nm) || length(nm) != 1) {
    stop("nm must be a string")
  }
  stopifnot(is.character(noOverlapCheckTiers))
  stopifnot(is.numeric(overlapThresh) && overlapThresh >= 0)
  fixMethod <- match.arg(fixMethod)
  checkZeroWidth <- match.arg(checkZeroWidth)
  stopifnot(is.numeric(maxIters) && maxIters > 0)
  reachMaxIters <- match.arg(reachMaxIters)
  timeDisp <- match.arg(timeDisp)
  
  ##Fix overlaps and/or get remaining overlaps
  x <- handleOverlapsOneFile(x, nm, TRUE, noOverlapCheckTiers, overlapThresh, 
                             fixMethod, checkZeroWidth, maxIters, reachMaxIters)
  overlaps <- attr(x, "overlaps")
  
  ##If no remaining overlaps, make overlapsNice a 0-row dataframe & exit early
  if (nrow(overlaps)==0) {
    attr(x, "overlapsNice") <- tibble(Tier = character(), Start = character(), End = character())
    return(x)
  }
  
  ##Format remaining overlaps for printing
  overlapsNice <- 
    overlaps %>% 
    ##Display overlapping turns with both boundaries
    distinct(ANNOTATION_ID, TIER_ID) %>% 
    left_join(x %>% map_dfr(as_tibble, .id="TIER_ID"),
              by=c("ANNOTATION_ID", "TIER_ID")) %>% 
    ##Nicer formatting
    select(Tier = TIER_ID, Start, End) %>% 
    arrange(Start) %>% 
    rowwise() %>% 
    mutate(across(c(Start,End), ~ formatTimes(.x, timeDisp))) %>% 
    ungroup()
  
  ##Add to x as an attribute
  attr(x, "overlapsNice") <- overlapsNice
  
  x
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
  
  ##Read files: Get a dataframe with one row per file. Each value in the file
  ##  column is either an object of class trs_[file extension] (if read
  ##  successfully by the read handler for [file extension]) or an error (thrown
  ##  by the read handler)
  fileDF <- eventReactive(input$files, {
    message("Uploaded ", nrow(input$files), " files:\n",
            str_flatten(paste("-", input$files$name), "\n"))
    
    ##Parse filenames
    fileDF <- parse_filenames(input$files$name, 
                              spkrCodeRE=spkrCodeRegex, 
                              neighborhoodRE=neighborhoodRegex,
                              readHandlers=readHandlers) %>% 
      left_join(input$files, "name")
    
    ##Try to use read handlers to read each file; if unsuccessful, return the 
    ##  error
    fileDF <- fileDF %>% 
      ##Get safely() list of results and errors
      mutate(file = map2(ReadHandler, map(datapath, as.list), 
                         safely(do.call)) %>% 
               ##Turn into a list of results *or* errors
               map_if(~ !is.null(.x$result), "result", .else="error")) %>% 
      ##Add class attributes to pass down to as.trs_transcription() if read 
      ##  successfully
      mutate(fileClass = if_else(map_lgl(file, ~ !inherits(.x, "error")),
                                 paste0("trs_", tolower(FileExt)),
                                 NA_character_),
             file = map2(file, fileClass, add_class)) %>% 
      select(-fileClass)
    
    fileDF
  })
  
  ##Convert valid files to list of trs_transcription objects
  trsList <- reactive({
    fileDF() %>%
      ##As list of files (and/or errors)
      pull(file, name) %>% 
      ##Only valid files
      discard(~ inherits(.x, "error")) %>% 
      ##Convert to trs_transcription via method dispatch
      map(~ as.trs_transcription(.x, annotation_metadata=TRUE,
                                 tierAttributes=TRUE))
  })
  
  # Export element for shinytest ---------------------------------------------
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

    ##First element: dataframe of file info (just the first 4 columns)
    export <- tagList(pack_val(fileDF() %>% 
                                 select(name, SpkrCode, Neighborhood, FileExt), 
                               "fileDF"))
    
    ##Add the rest of the data
    validationDF <- validateFiles(fileDF(), validExts=names(readHandlers))
    fileCheckValid <- all(validationDF$Valid)
    if (!fileCheckValid) { 
      ##If step 0 failed, add validationDF
      export <- c(export, tagList(pack_val(validationDF, "validationDF")))
    } else {
      ##If step 0 passed, add tier info & at least one fileDF()
      tierDF <- map(trsList(), tier_metadata)
      turnsDF <- 
        trsList() %>% 
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
  
  # Main execution block ----------------------------------------------------
  main <- reactive({
    ##Always-displayed headings
    stepHeads <- list(tiers = h2("Step 1: Validating tier names and attributes...",
                                 id="tierHead"),
                      dict = h2("Step 2: Checking for out-of-dictionary words...",
                                id="dictHead"),
                      overlaps = h2("Step 3: Checking for overlaps...",
                                    id="overlapsHead"))
    
    ##If no uploaded files, just display headings
    if (!isTruthy(input$files)) {
      tl <- tagList(
        h1("Waiting for uploaded files..."), 
        stepHeads)
      return(list(tags = tl, outdata = list()))
    }
    
    ##If uploaded files, initialize exitEarly sentinel & proceed to checking
    ##  steps
    exitEarly <- FALSE
    
    
    ## Step 0: File check =====================================================
    fileCheckHead <- h1("Checking the following files...",
                        id="fileCheckHead")
    
    ##Get validation info
    validationDF <- validateFiles(fileDF(), validExts=names(readHandlers))
    
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
      tierIss <- trsList() %>% 
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
      dictIss <- trsList() %>% 
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
      ##Fix overlaps
      trsListFixed <- trsList() %>% 
        imap(overlapsIssuesOneFile, noOverlapCheckTiers=noOverlapCheckTiers, 
             overlapThresh=overlapThresh, fixMethod=fixMethod, 
             checkZeroWidth=checkZeroWidth, maxIters=maxIters, 
             reachMaxIters=reachMaxIters, timeDisp=timeDisp)
      ##Get list of remaining overlaps
      overlapsIss <-
        trsListFixed %>% 
        map(attr, "overlapsNice") %>% 
        discard(~ nrow(.x)==0)
      
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
    
    
    # Download elements -----------------------------------------------------
    ##List of transcriptions to download
    if (exitEarly) {
      outdata <- list()
    } else {
      outdata <- map(trsListFixed, trs_to_eaf)
    }
    ##Content
    downloadHead <- h1("The file(s) passed all checks. Great job!",
                       id="downloadHead")
    downloadSubhead <- h3("Please download the corrected file(s) and upload to the Completed folder",
                          id="downloadSubhead")
    downloadBtn <- downloadButton("OutputFile", "Download corrected file(s)")
    
    # UI tags ---------------------------------------------------------------
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
    tl <- tagList(
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
    
    ##Return tag list and modified data
    list(tags = tl, outdata = outdata)
  }) ##End main <- reactive({})
  
  # Output: UI --------------------------------------------------------------
  output$out <- renderUI({
    req(main())
    main()$tags
  })
  
  # Create output file(s) ---------------------------------------------------
  output$OutputFile <- downloadHandler(
    filename=function() {
      if (length(main()$outdata)==1) {
        names(main()$outdata)
      } else {
        "corrected_eafs.zip"
      }
    },
    content=function(file) {
      if (length(main()$outdata)==1) {
        write_xml(main()$outdata[[1]], file)
      } else {
        main()$outdata %>% 
          iwalk(write_xml)
        zip(file, names(main()$outdata))
      }
    }
  )
}

shinyApp(ui = ui, server = server)
