
library(shiny)
library(xml2)
library(stringr)
library(purrr)
library(tidyr)
library(dplyr)
library(magrittr)
library(here)


# Parameters --------------------------------------------------------------

##Dictionary checking
##Permit angle brackets for single-word interruptions?
permitAngleBrackets <- FALSE
##Characters to accept in pronounce codes
pronChars <- "[pbtdkgNmnlrfvTDszSZjhwJ_CFHPIE\\{VQU@i$u312456]"
##Case-sensitive?
caseSens <- FALSE

##Overlap fixing
##Maximum cross-tier misalignment (in ms) to 'snap together'. Set lower to be
##  more conservative about what counts as an intended cross-tier alignment
overlapThresh <- 500
##Time display type: "S" (seconds, useful for Praat TextGrids), or "HMS" (useful
##  for ELAN)
timeDisp <- "HMS"

##Exit-early overrides, for debugging
overrideExit <- list(fileExt = FALSE, tiers = FALSE, dict = FALSE, overlaps = FALSE)

# UI ----------------------------------------------------------------------
ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "file-checker.css")
  ),
  titlePanel("Elan File Checker for APLS"),
  p("Created by Dan Villarreal"),
  p("Updated 24 October 2021"),
  sidebarLayout(
    sidebarPanel(
      fileInput("files",
                label="Drag and drop Elan files in the box below",
                buttonLabel="Browse...",
                placeholder="Box outline must turn green",
                multiple = TRUE)),
    
    # mainPanel(verbatimTextOutput("debug"),
    mainPanel(uiOutput("debug"),
              uiOutput("out"))
    
    # mainPanel(
    #   h1(textOutput("checkHead")),
    #   h2(textOutput("tiersHead")),
    #   textOutput("tiersTop"),
    #   verbatimTextOutput("tiers"),
    #   h2(textOutput("dictHead")),
    #   textOutput("dictTop"),
    #   verbatimTextOutput("dict"),
    #   h2(textOutput("overlapsHead")),
    #   textOutput("overlapsTop"),
    #   verbatimTextOutput("overlaps"),
    #   uiOutput("download")
    # )
  )
)


# Functions for server-side processing ------------------------------------

## Tiers ==================================================================
##Function that takes a one-file tier df as input and outputs tier issues
tierIssuesOneFile <- function(df, filename) {
  ##Initialize empty issues character vector
  issues <- character(0L)
  
  ##Add tier number (as backup for IDing tiers w/o TIER_ID attr)
  df <- df %>% 
    mutate(tierNum = paste("Tier", row_number()))
  
  ##Handle missing tiers
  checkTiers <- c(paste0(c("", "Interviewer "), unique(df$SpkrCode)),
                  "Comments", "Noises", "Redactions")
  issues <- c(issues,
              checkTiers %>% 
                map(
                  ~ if(!(.x %in% df$TIER_ID)) {
                    paste("There are no tiers with tier name", .x)
                  }) %>% 
                reduce(c))
  
  ##Handle missing attributes
  checkAttrs <- c("ANNOTATOR", "PARTICIPANT", "TIER_ID")
  missingAttr <- function(x) {
    attrTitle <- str_to_title(x)
    attrArticle <- if_else(str_detect(tolower(x), "^[aeiou]"), "an", "a")
    
    tryCatch({
      attrCol <- df[,x]
      if (any(is.na(attrCol))) {
        noAttrDF <- df[is.na(attrCol), ]
        noAttr <-
          noAttrDF %>% 
          mutate(tierName = if_else(is.na(TIER_ID), tierNum, TIER_ID)) %>% 
          pull(tierName)
        paste("Tier missing", attrArticle, attrTitle, "attribute:", noAttr)
      }
    }, error = function(e) {
      paste("All tiers missing", attrArticle, attrTitle, "attribute")
    })
    
  }
  issues <- c(issues,
              checkAttrs %>% 
                map(missingAttr) %>% 
                reduce(c))
  
  ##Handle mismatched tier ID & participant attrs
  if (!is.null(df$PARTICIPANT) && !is.null(df$TIER_ID) &&
      !identical(df$TIER_ID, df$PARTICIPANT)) {
    issues <- c(issues,
                df %>% 
                  filter(TIER_ID != PARTICIPANT) %>% 
                  mutate(msg = paste0("Mismatched tier name (", TIER_ID,
                                      ") & Participant attribute (",
                                      PARTICIPANT, ")")) %>% 
                  pull(msg))
  }
  
  issues
  
}

##Wrapper function around tierIssuesOneFile() that takes a multi-file tier df as
##  input (meant to be used with tierInfo() reactive) and outputs tier issues if
##  any; if no issues, outputs an empty list
tierIssues <- function(df) {
  df %>%
    ##Look for tier issues for each file
    nest(data = -File) %>% 
    mutate(issues = map(data, tierIssuesOneFile)) %>% 
    ##Turn into list with one element for each file
    pull(issues, name=File) %>% 
    ##Only keep files with issues
    keep(~ length(.x) > 0)
}

## Dictionaries ===============================================================

##Generate dictionary
dict <-
  ##Read dictionary file(s)
  # list.files(here("dict/"), pattern="\\.txt", full.names=TRUE) %>% 
  list.files("dict/", pattern="\\.txt", full.names=TRUE) %>% 
  map(readLines) %>% 
  ##As single character vector
  reduce(c) %>% 
  ##Ignore lines starting with "#" or empty lines
  str_subset("^(#.*|)$", negate=TRUE) %>% 
  ##Unique
  unique()

##Function that takes a tier name and eaf file as input and outputs
##  non-dictionary words
dictCheckTier <- function(tierName, eaf) {
  ##Get all lines in tier
  tierLines <- str_glue("//TIER[@TIER_ID='{tierName}']//ANNOTATION_VALUE") %>%
    xml_find_all(eaf, .) %>%
    xml_text()
  
  ##Get all unique words 
  tierWords <- 
    tierLines %>% 
    ##Unstrand valid punctuation within angle brackets
    str_replace_all("([[:alpha:]]~?) ([?.-])>", "\\1\\2>") %>% 
    ##Ignore text within curly braces (comments about speech or behavior)
    str_subset("\\{.*?\\}", negate=TRUE) %>% 
    ##Strip matched brackets
    str_replace_all("(?<![\\w~])\\[(.+?)\\](?!\\w)", "\\1") %>%
    ##Remove extra whitespace
    str_trim("both") %>% 
    str_squish() %>% 
    ##Separate into words (as a vector)
    str_split(" ") %>% 
    flatten_chr() %>% 
    ##Unique words only
    unique()
  
  ##Optionally strip matched angle brackets (single-word interruptions)
  if (permitAngleBrackets) {
    tierWords <- tierWords %>% 
      str_replace("^<(.+)>$", "\\1") %>% 
      unique()
  }
  
  ##Single-word ignores
  tierWords <- tierWords %>% 
    ##Ignore words with valid bracket pronounce codes (sui generis words)
    str_subset(paste0("^[[:alpha:]']+~?\\[", pronChars, "+\\]$"),
               negate=TRUE) %>% 
    ##Ignore standalone valid punctuation
    str_subset("[.?-]|--", negate=TRUE)
  
  ##Get forms of words for checking
  wordDF <- tibble(
    Word = tierWords,
    ##First checking form
    CheckWord1 = Word %>%
      ##Strip attached valid punctuation
      str_remove("([.?-]|--)$") %>%
      ##For words with paren codes, use the paren code for checking
      str_replace(".+\\((.+)\\)$", "\\1") %>%
      ##Strip clitics for checking
      str_remove_all("'(d|ve|s)") %>%
      str_replace("s'$", "s"),
    ##Second form (without final -s)
    CheckWord2 = CheckWord1 %>%
      str_remove("s$")
  )
  
  ##Optionally convert checking forms to lowercase
  if (!caseSens) {
    wordDF <- wordDF %>% 
      mutate(across(-Word, str_to_lower))
    dict <- str_to_lower(dict)
  }
  
  ##Return words that aren't in the dictionary in either form
  wordDF %>%
    filter(!if_any(-Word, ~ .x %in% dict)) %>% 
    pull(Word)
}

##Wrapper function around dictCheckTier() that takes a multi-file tier df
##  and a list of EAF files as input (meant to be used with tierInfo() &
##  eaflist() reactives) and outputs non-dictionary words if any (in a
##  nested list); if no issues, outputs an empty list
dictCheck <- function(df, x) {
  ##Get nested list of speaker tier names
  spkrTierNames <- 
    df %>% 
    filter(SpkrTier) %>% 
    group_by(File) %>% 
    summarise(across(TIER_ID, list)) %>% 
    pull(TIER_ID, name=File) %>% 
    map(~ set_names(.x, .x))
  
  ##Loop over files & speaker tiers for dictionary-checking
  map2(spkrTierNames, x, 
       ~ map(.x, dictCheckTier, .y) %>% 
         ##Only keep tiers with issues
         keep(~ length(.x) > 0)) %>% 
    ##Only keep files with issues
    keep(~ length(.x) > 0)
}

## Overlaps ===================================================================

##Function that takes a tier name, eaf file, and file-wide time slot DF as
##  input and outputs actual times for annotations
getTimesTier <- function(tierName, eaf, timeSlots) {
  ##Get all ALIGNABLE_ANNOTATION tags
  str_glue("//TIER[@TIER_ID='{tierName}']//ALIGNABLE_ANNOTATION") %>%
    xml_find_all(eaf, .) %>% 
    ##Get attributes as a dataframe
    xml_attrs() %>% 
    bind_rows() %>% 
    ##Add actual times
    left_join(timeSlots %>%
                rename(TIME_SLOT_REF1 = TIME_SLOT_ID,
                       Start = TIME_VALUE),
              by="TIME_SLOT_REF1") %>%
    left_join(timeSlots %>%
                rename(TIME_SLOT_REF2 = TIME_SLOT_ID,
                       End = TIME_VALUE),
              by="TIME_SLOT_REF2")
}

##Wrapper function around getTimesTier() that takes a single EAF file and name
##  (meant to be used with eaflist() reactive and imap()) plus multi-file tier
##  df (meant to be used with tierInfo() reactive) as input, and outputs nested
##  list of dataframes of annotation times (files at level one, tier DFs at
##  level two for speaker tiers only)
##N.B. This function outputs a list of DFs rather than a single DF because the
##  list structure makes it easier to detect overlaps in findOverlaps() (by
##  comparing the timings on a given speaker tier to all other speaker tiers)
getTimes <- function(eaf, eafName, df) {
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
  
  ##Get speaker tier names
  spkrTierNames <- 
    ##df intended to be tierInfo() reactive
    df %>% 
    filter(File==eafName, SpkrTier) %>% 
    pull(TIER_ID)
  
  ##Get times for speaker tiers (list of dataframes)
  spkrTimes <- 
    spkrTierNames %>% 
    set_names(., .) %>% 
    map(getTimesTier, eaf, timeSlots) %>%
    ##Only nonempty tiers
    keep(~ nrow(.x) > 0)
}

##Function that takes a single tier name and a nested list of annotation time
##  DFs (meant to be used with output of getTimes()), and outputs a single
##  dataframe: the relevant annotation time DF plus three boolean overlaps
##  columns (left, right, both), where TRUE means there is at least one
##  annotation on another tier whose (e.g.) left boundary is within the
##  annotation
findOverlapsTier <- function(tierName, timesEAF) {
  ##Get annotation timing DF for selected speaker
  spkr <- timesEAF %>% pluck(tierName)
  ##Get annotation timing DF that combines all other speakers
  otherSpkrs <-
    timesEAF %>%
    extract(names(.) != tierName) %>%
    bind_rows()
  
  ##For each boundary in selected tier, return the first annotation that the
  ##  boundary overlaps with, if any
  ##Currently isn't guaranteed to work with 3+ speaker tiers (because
  ##  which(x)[1] only selects the first result, not necessarily the result
  ##  closest to the boundary)
  bounds <- 
    spkr %>%
    ##One row per boundary (new columns Side, TIME_SLOT_REF, Time)
    rename(Time1 = Start, Time2 = End) %>% 
    pivot_longer(-ANNOTATION_ID, 
                 names_to=c(".value", "Side"), names_pattern="(.+)([12])") %>% 
    ##Check row-by-row (otherwise comparison in which() doesn't work)
    rowwise() %>% 
    ##Add overlap annotation ID
    ##N.B. This works because if which(x) is integer(0), which(x)[1] is NA
    mutate(ANNOTATION_ID_overlapped = otherSpkrs$ANNOTATION_ID %>% 
             extract(which(Time > otherSpkrs$Start & Time < otherSpkrs$End)[1])) %>% 
    ungroup()
  
  ##Restrict to boundaries with overlaps, and add information about overlapped
  ##  annotations
  overlapBounds <- 
    bounds %>%
    ##Only the boundaries that overlap another annotation
    filter(!is.na(ANNOTATION_ID_overlapped)) %>% 
    ##Add info about overlapped annotation
    left_join(otherSpkrs %>%
                rename_with(~ paste0(.x, "_overlapped")),
              by="ANNOTATION_ID_overlapped") %>% 
    ##Determine whether the nearest boundary is close enough (rowwise for min())
    rowwise() %>%
    mutate(StartDiff = abs(Start_overlapped - Time),
           EndDiff = abs(End_overlapped - Time),
           CloseEnough = min(StartDiff, EndDiff) < overlapThresh) %>%
    ungroup()
  
  ##Add new timeslot ID: if not close enough, "Too far"; if close enough, closer
  ##  boundary (tie goes to start boundary); anything else is unexpected so it
  ##  triggers an error below
  overlapBounds <- overlapBounds %>% 
    mutate(NewTS = case_when(
              !CloseEnough ~ "Too far",
              StartDiff <= EndDiff ~ TIME_SLOT_REF1_overlapped,
              StartDiff > EndDiff ~ TIME_SLOT_REF2_overlapped,
              TRUE ~ NA_character_),
            ##Add node path for fixing overlap
            NodePath = str_glue("//ALIGNABLE_ANNOTATION[@ANNOTATION_ID='{ANNOTATION_ID}']"))
  
  ##Return dataframe
  overlapBounds
}

##Check for problems with overlapBounds, fix any overlaps by modifying eaflist,
##  and return overlapBounds with info about fixed boundaries
fixOverlapsTier <- function(overlapBounds, eaflist, eafName) {
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
  zeroWidth <- 
    overlapBoundsFixed %>%
    select(ANNOTATION_ID:Time, NewTS) %>%
    pivot_wider(names_from=Side, names_glue="{.value}{Side}",
                values_from=TIME_SLOT_REF:NewTS) %>%
    filter(NewTS1==NewTS2)
  if (nrow(zeroWidth) > 0) {
    stop("On tier ", tierName, 
         ", at least one annotation was fixed to zero width:\n",
         "ANN_ID  Start   End\n",
         paste0(str_pad(zeroWidth$ANNOTATION_ID, 8, 'right'),
                str_pad(round(zeroWidth$Time1 / 1000, 1), 8, 'right'),
                str_pad(round(zeroWidth$Time2 / 1000, 1), 8, 'right'),
                "\n"))
  }
  
  ##Fix overlaps in EAF list
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

##Wrapper function around getTimesTier() that takes a *list* of DFs of
##  annotation times (one files' worth) and a single EAF name (meant to be used
##  with output of getTimes() and imap()), and outputs a *single* DF with 
##  annotation times plus overlaps columns
findOverlaps <- function(timesEAF, eafName) {
  ##Loop over tiers within this file to get overlaps
  timesEAF %>%
    names() %>%
    set_names(., .) %>%
    map(findOverlapsTier, timesEAF=timesEAF)
}

##Wrapper function around fixOverlapsTier() that takes a *list* of DFs of
##  annotation times (one file's worth) and a single EAF name (meant to be used
##  with output of getTimes() and imap()), and rotates through tiers, fixing
##  overlaps, until it reaches a stable state
fixOverlaps <- function(timesEAF, eaflist, eafName) {
  # ##Initialize empty overlap log
  # overlapLog <- data.frame(Tier = character(), 
  #                          NumBounds = character(),
  #                          NumOverlaps = character(),
  #                          NumFixed = character())
  
  ##Get initial overlaps
  overlapsInit <- 
    ##Get tier names
    timesEAF %>%
    names() %>% 
    set_names(., .) %>%
    ##Loop over tier names to find overlaps
    map(findOverlapsTier, timesEAF)
  
  ##Initialize looping variables
  overlapsPre <- NULL
  overlapsPost <- overlapsInit
  iters <- 0
  maxIter <- 10
  
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
    
    ##Go through another fixing round
    overlapsPost <- 
      overlapsPre %>%
      map(fixOverlapsTier, eaflist, eafName) %>% 
      map(filter, !Resolved)
    
    message("iters: ", iters)
    message("nrow(overlapsPre): ", map_int(overlapsPre, nrow) %>% paste(collapse=" "))
    message("nrow(overlapsPost): ", map_int(overlapsPost, nrow) %>% paste(collapse=" "))
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
##x should be eaflist(), df should be tierInfo()
overlapsIssues <- function(x, df) {
  ##Get timing data
  times <- imap(x, getTimes, df=df)
  
  ##Fix overlaps & format output for display
  fixed <- 
    x %>% 
    names() %>% 
    set_names(., .) %>% 
    ##Fix overlaps
    map(~ fixOverlaps(times[[.x]], x, .x) %>% 
          ##All in one DF
          bind_rows(.id="Tier")) %>% 
    imap(~ .x %>%
           ##One row per annotation (nicer labels)
           select(ANNOTATION_ID, Tier, Side, Time) %>%
           mutate(Side = if_else(Side==1, "Start", "End")) %>%
           pivot_wider(names_from=Side, values_from=Time))
  ##Remove empty entries from times and fixed
  nonempty <- map_int(fixed, nrow) > 0
  times <- times[nonempty]
  fixed <- fixed[nonempty]
  
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
          mutate(across(c(Start,End), formatTimes, type=timeDisp)))
}

## UI display =================================================================

##Convenience functions to display/undisplay HTML elements
display <- function(x) {
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




# Server ------------------------------------------------------------------
server <- function(input, output) {
  # Set up file structures --------------------------------------------------
  
  ##Dataframe of files
  files <- eventReactive(input$files, {
    ##Start with input files dataframe
    input$files %>% 
      ##More informative column name
      rename(File = name) %>% 
      ##Add neighborhood, speaker number, and file number
      ##Will need to be extended to multiple speakers, non-interview tasks, etc.
      tidyr::extract(File, c("SpkrCode", "FileNum"), 
                     "((?:CB|FH|HD|LV)\\d+)-(\\d+).+", FALSE, TRUE) %>% 
      tidyr::extract(SpkrCode, c("Neighborhood", "SpeakerNum"),
                     "([A-Z]{2})(\\d+)", FALSE, TRUE) %>% 
      ##Add file extension
      mutate(FileExt = str_extract(File, "[^\\.]+$"),
             FileExtValid = FileExt=="eaf",
             .after=File) %>% 
      ##Sort
      arrange(Neighborhood, SpeakerNum, FileNum, File)
  })
  
  ##Read files: Get a list that's nrow(files()) long, each element an xml_document
  eaflist <- reactive({
    req(all(files()$FileExtValid) || overrideExit$fileExt)
    files() %>% 
      pull(datapath, name=File) %>% 
      map(read_xml)
  })
  
  ##Number of files
  numFiles <- reactive({
    req(eaflist())
    length(eaflist())
  })
  
  ##Extract tiers: Get a list that's nrow(files()) long, each element an xml_nodeset with one node per tier
  tiers <- reactive({
    req(eaflist())
    map(eaflist(), xml_find_all, "//TIER")
  })
  
  ##Get tier info as a single dataframe
  tierInfo <- reactive({
    req(tiers())
    tiers() %>% 
      ##One row per tier, with file info
      map_dfr(~ map_dfr(.x, xml_attrs), .id="File") %>% 
      ##Add SpkrTier (is the tier a speaker tier?)
      mutate(SpkrTier = str_detect(tolower(PARTICIPANT), "comments|noises|redactions", negate=TRUE)) %>% 
      ##Add info from files()
      left_join(files(), by="File")
  })
  
  # Debugging output --------------------------------------------------------
  # output$debug <- renderPrint({
  output$debug <- renderUI({
    a() %>% undisplay()
  })
  
  
  # Output: UI --------------------------------------------------------------
  output$out <- renderUI({
    ##Always-displayed headings
    stepHeads <- list(tiers = h2("Step 1: Validating tier names and attributes..."),
                      dict = h2("Step 2: Checking for out-of-dictionary words..."),
                      overlaps = h2("Step 3: Checking for overlaps..."))
    
    ##If no uploaded files, just display headings
    if (!isTruthy(input$files)) {
      return(tagList(
        h1("Waiting for uploaded files..."), 
        stepHeads))
    }
    
    ##If uploaded files, initialize exitEarly sentinel & proceed to checking steps
    exitEarly <- FALSE
    
    
    
    # Step 0: File extension check --------------------------------------------
    checkHead <- h1("Checking the following files...",
                    id="checkHead")
    
    ##Get bullet-list of filenames (styling bad ones in red)
    checkDetails <- tags$ul(
      map_if(files()$File,
             ~ !endsWith(.x, "eaf"),
             ~ tags$li(.x, class="bad"),
             .else=tags$li),
      id="checkDetails"
    )
    
    ##Style headings based on whether file extensions are valid
    noEafHead <- h2("The checker only works on files with an .eaf file extension",
                    id="noEafHead", class="bad")
    fileExtValid <- all(files()$FileExtValid)
    if (fileExtValid) {
      noEafHead <- undisplay(noEafHead)
    } else {
      noEafHead <- display(noEafHead)
      stepHeads <- stepHeads %>%
        map(tagAppendAttributes, class="grayout")
      
      ##Exit early
      exitEarly <- TRUE
    }
    
    
    
    # Step 1: Tier check ------------------------------------------------------
    ##Content
    tierSubhead <- h3(paste("The tier checker returned the following issue(s),",
                            "which can be resolved using Change Tier Attributes in Elan:"),
                      id="tierSubhead") %>% 
      ##By default, don't display
      undisplay()
    tierDetails <- tags$ul("", id="tierDetails") %>% 
      ##By default, don't display
      undisplay()
    
    ##If not exiting early yet, check for tier issues
    if (!exitEarly || overrideExit$fileExt) {
      ##Get tier issues
      tierIss <- tierIssues(tierInfo())
      ##If no tier issues, don't display anything & make step heading green
      if (length(tierIss)==0) {
        tierSubhead <- undisplay(tierSubhead)
        tierDetails <- undisplay(tierDetails)
        stepHeads$tiers <- stepHeads$tiers %>% 
          tagAppendAttributes(class="good")
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
                    map(.x, tags$li)
                  )))
          )
        
        ##Style headers
        stepHeads$tiers <- stepHeads$tiers %>% 
          tagAppendAttributes(class="bad")
        stepHeads[2:3] <- stepHeads[2:3] %>% 
          map(tagAppendAttributes, class="grayout")
        
        ##Exit early
        exitEarly <- TRUE
      }
    }
    
    
    # Step 2: Dictionary check ------------------------------------------------
    ##Content
    dictSubhead <- h3(paste("The following word(s) are not currently in the dictionary.",
                            "Please correct misspellings, fix punctuation, and/or add pronunciation codes.",
                            "Notify Dan of any words that should be added to the dictionary."),
                      id="dictSubhead") %>% 
      ##By default, don't display
      undisplay()
    dictDetails <- tags$ul("", id="dictDetails") %>% 
      ##By default, don't display
      undisplay()
    
    ##If not exiting early yet, check for dictionary issues
    if (!exitEarly || overrideExit$tiers) {
      ##Get dictionary check
      dictIss <- dictCheck(tierInfo(), eaflist())
      ##If no dictionary issues, don't display anything & make step heading green
      if (length(dictIss)==0) {
        dictSubhead <- undisplay(dictSubhead)
        dictDetails <- undisplay(dictDetails)
        stepHeads$dict <- stepHeads$dict %>% 
          tagAppendAttributes(class="good")
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
                             map(.x, tags$li)
                           )
                         ))
                  )))
          )
        
        ##Style headers
        stepHeads$dict <- stepHeads$dict %>% 
          tagAppendAttributes(class="bad")
        stepHeads[3] <- stepHeads[3] %>% 
          map(tagAppendAttributes, class="grayout")
        
        ##Exit early
        exitEarly <- TRUE
      }
    }
    
    
    # Step 3: Overlaps check --------------------------------------------------
    ##Content
    overlapsSubhead <- h3(paste("The overlap checker could not resolve the following overlaps.",
                                "Please fix these overlaps; remember to make overlaps a",
                                "separate turn on each speaker's tier."),
                          id="dictSubhead") %>% 
      ##By default, don't display
      undisplay()
    overlapsDetails <- tags$ul("", is="overlapsDetails") %>% 
      ##By default, don't display
      undisplay()
    
    ##If not exiting early yet, check for dictionary issues
    if (!exitEarly || overrideExit$dict) {
      ##Get overlaps issues
      overlapsIss <- overlapsIssues(eaflist(), tierInfo())
      ##If no dictionary issues, don't display anything & make step heading green
      if (length(overlapsIss)==0) {
        overlapsSubhead <- undisplay(overlapsSubhead)
        overlapsDetails <- undisplay(overlapsDetails)
        stepHeads$overlaps <- stepHeads$overlaps %>% 
          tagAppendAttributes(class="good")
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
                    class="table shiny-table table- spacing-s overlap-table"
                  )))
          )
        
        ##Style headers
        stepHeads$overlaps <- stepHeads$overlaps %>% 
          tagAppendAttributes(class="bad")
        
        ##Exit early
        exitEarly <- TRUE
      }
    }
    
    
    # Download ----------------------------------------------------------------
    ##Content
    downloadHead <- h1("The file(s) passed all checks. Great job!",
                       id="downloadHead")
    downloadSubhead <- h3("Please download the corrected file(s) and upload to the server",
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
    
    ##Construct tag list
    tagList(
      ##Checking head: display bullet-list of files, marking non-eaf as bad
      checkHead,
      ##Bullet-list
      checkDetails,
      ##Display message if there are non-eaf files
      noEafHead,
      stepHeads$tiers,
      tierSubhead,
      tierDetails,
      stepHeads$dict,
      dictSubhead,
      dictDetails,
      stepHeads$overlaps,
      overlapsSubhead,
      overlapsDetails,
      reuploadHead,
      downloadHead,
      downloadSubhead,
      downloadBtn
    )
    
  })
  
  
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