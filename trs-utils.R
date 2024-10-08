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
  library(tibble)
  library(stringr)
  library(tools)
  
  ##Check args
  if (!is.character(x)) {
    stop("x must be a character vector")
  }
  if (!identical(x, basename(x))) {
    warning("Found non-basename(s) in x. Using basename(x) instead")
    x <- basename(x)
  }
  stopifnot(is.character(spkrCodeRE) && length(spkrCodeRE)==1)
  stopifnot(is.character(neighborhoodRE) && length(neighborhoodRE)==1)
  
  ##Collect file info
  tibble(name = x,
         SpkrCode = str_extract(name, spkrCodeRE),
         Neighborhood = str_extract(SpkrCode, neighborhoodRE),
         FileExt = file_ext(name))
}

##Check that Praat exists in praatDir, either as Praat.exe or a zipfolder
##  praat...zip, and return its path (including ./ prefix, suitable for use in
##  shell scripts to run it as an executable)
check_for_praat <- function(praatDir=".") {
  ##Check arg
  stopifnot(is.character(praatDir) && dir.exists(praatDir))
  
  ##Ensure existence of executable and/or zipped archive
  os <- Sys.info()[["sysname"]]
  if (os=="Windows") {
    pathExe <- "Praat.exe"
    pathZip <- "praat.+\\.zip"
  } else if (os=="Linux") {
    pathExe <- "praat_barren"
    pathZip <- "praat.+barren\\.tar\\.gz"
  } else {
    stop("Operating system ", os, " not supported")
  }
  praatExe <- file.path(praatDir, pathExe)
  praatZip <- dir(praatDir, pathZip, full.names=TRUE, ignore.case=TRUE)
  if (!file.exists(praatExe) && length(praatZip) == 0) {
    stop("No Praat executable or zipfolder found in praatDir ", praatDir)
  }
  
  ##Unzip if necessary
  if (!file.exists(praatExe)) {
    ##If more than one zipfile found, try to get the latest one
    ##This is imperfect because (e.g.) praat649.zip will incorrectly be sorted
    ##  after praat6410.zip
    numPraatZip <- length(praatZip)
    if (numPraatZip > 1) {
      praatZip <- sort(praatZip, decreasing=TRUE)[1]
      warning("Found ", numPraatZip, " Praat zipfolders in praatDir ", praatDir,
              "\n  Using ", basename(praatZip))
    }
    
    ##Function to deal with interface inconsistencies between unzip() & untar()
    ##Unzips/untars x into dir, and invisibly returns extracted paths
    ##N.B. Doesn't (1) check that paths are valid or (2) handle other OSs, since
    ##  the larger script does both
    extractZip <- function(x, dir, os=Sys.info()[["sysname"]]) {
      if (os=="Windows") {
        path <- unzip(x, exdir=dir)
      }
      if (os=="Linux") {
        path <- file.path(dir, untar(x, list=TRUE))
        untar(x, exdir=dir)
      }
      invisible(path)
    }
    
    ##Unzip to praatDir
    message("Extracting Praat from ", praatZip)
    ##Returning filepaths only works for unzip()
    praatExe <- extractZip(praatZip, praatDir) 
    ##Ensure praatZip is a valid Praat zipfolder
    if (length(praatExe) != 1 || basename(praatExe) != pathExe) {
      stop(praatZip, " is not a valid Praat zipfolder ", 
           "(it should contain only ", pathExe, ")")
    }
  }
  
  ##Ensure Praat is runnable
  versScript <- tempfile(fileext=".praat")
  c('form: "Write Praat version to file"',
    '  sentence: "outPath", ""',
    'endform',
    '',
    'writeFileLine: outPath$, praatVersion$') %>% 
    writeLines(versScript)
  on.exit(file.remove(versScript))
  versFile <- tempfile(fileext=".txt")
  system2(praatExe, c("--run", versScript, versFile))
  if (!file.exists(versFile)) {
    stop("Praat executable ", praatExe, " failed to run")
  }
  message("Running Praat version ", readLines(versFile))
  file.remove(versFile)
  
  praatExe
}

##Given a path to a Praat TextGrid, read as an R dataframe (using Praat's
##  built-in "Down to Table..." command)
read_textgrid <- function(x, praatDir=".", customScript=NULL, 
                          tmpcsv=tempfile(fileext=".csv"), clean=TRUE) {
  library(tibble)
  library(fs)
  library(readr)
  
  ##Check args
  praatExe <- check_for_praat(praatDir)
  stopifnot(file.exists(x))
  
  ##Get script
  ##If customScript is NULL, supply default script
  if (is.null(customScript)) {
    praatScript <- tempfile(fileext=".praat")
    c('form: "Convert to csv"',
      '  sentence: "inPath", ""',
      '  sentence: "outPath", ""',
      'endform',
      '',
      'tg = Read from file: inPath$',
      'table = Down to Table: 0, 6, 1, 1',
      'Formula (column range): "text", "text", "replace$(self$, """""""", ""\\"""""", 0)"',
      'if outPath$ = "" ',
      '  basename$ = replace_regex$(inPath$, ".+/", "", 0)',
      '  outPath$ = replace_regex$(basename$, "\\.[Tt]ext[Gg]rid", ".csv", 1)',
      'endif',
      'Save as comma-separated file: outPath$') |>
      writeLines(praatScript)
    on.exit(file.remove(praatScript))
  }
  
  ##If customScript doesn't exist, try with path relative to praatDir
  if (!is.null(customScript) && !file.exists(customScript)) {
    praatScript <- file.path(praatDir, customScript)
    if (!file.exists(praatScript)) {
      stop("customScript file ", customScript, " does not exist")
    }
  }
  
  ##If customScript already existed, use it as-is
  if (!exists("praatScript")) {
    praatScript <- customScript
  }
  
  ##Run Praat script and check that it created an output
  inPath <- path_rel(x, dirname(praatScript))
  outPath <- path_rel(tmpcsv, dirname(praatScript))
  system2(praatExe, c("--run", praatScript, inPath, outPath), stderr=FALSE)
  if (!file.exists(tmpcsv)) {
    stop(x, " is not a valid TextGrid")
  }
  
  ##Read csv, add class, and clean up tempfile
  enco <- guess_encoding(tmpcsv)$encoding[1]
  out <- read_delim(tmpcsv, delim=",", escape_backslash=TRUE, 
                    locale=locale(encoding=enco), show_col_types=FALSE) %>% 
    as_tibble() %>% 
    add_class("trs_textgrid")
  if (clean) {
    file.remove(tmpcsv)
  }
  
  out
}

##Given a path to an Elan transcription, read as an XML file
read_eaf <- function(x) {
  library(xml2)
  stopifnot(file.exists(x))
  out <- read_xml(x) %>% 
    add_class("trs_eaf")
  
  out
}


# trs_transcription -------------------------------------------------------

##S3 generic for as.trs_transcription
as.trs_transcription <- function(x, ...) {
  UseMethod("as.trs_transcription")
}


##Given a single trs_textgrid, return a list of dataframes of tier annotations, 
##  one for each tier. Adds tier metadata as attributes for dataframe-elements
##  to mirror trs_eaf objects (and pass validation checks)
as.trs_transcription.trs_textgrid <- function(x, tierAttributes=FALSE, ...) {
  library(dplyr)
  library(tidyr)
  
  ##Create list of tier dataframes
  out <- 
    x %>%
    ##Rename and reorder columns
    select(TIER_ID = tier, Start = tmin, End = tmax, Text = text) %>% 
    ##As list of dataframes
    nest(data = -TIER_ID) %>% 
    pull(data, TIER_ID) %>% 
    ##Remove blank turns
    map(~ filter(.x, !is.na(Text)))
  
  ##Optionally add tier attributes to mirror trs_eaf objects
  if (tierAttributes) {
    out <- out %>% 
      imap(~ add_attributes(.x, list(LINGUISTIC_TYPE_REF="default-lt",
                                     PARTICIPANT = .y)))
  }
  
  ##Remove trs_textgrid class from nested dataframes
  out <- out %>% 
    map(as_tibble) %>% 
    ##Add trs_transcription and trs_nesttiers classes to object
    add_class(c("trs_transcription", "trs_nesttiers", "trs_from_textgrid"))
  
  out
}


##Given a single trs_eaf, return a list of dataframes of tier annotations, one
##  for each tier. Includes file metadata as attributes for object, and tier
##  metadata as attributes for dataframe-elements
as.trs_transcription.trs_eaf <- function(x, annotation_metadata=FALSE, ...) {
  library(xml2)
  library(purrr)
  library(tidyr)
  library(dplyr)
  
  ##Get file attributes
  fileAttr <- xml_attrs(x)
  ##Restore xsi: prefix to noNamespaceSchemaLocation
  ns <- which(names(fileAttr)=="noNamespaceSchemaLocation")
  if (length(ns) > 0) {
    names(fileAttr)[ns] <- "xsi:noNamespaceSchemaLocation"
  }
  
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
  ##Convert milliseconds to seconds
  times <- times %>% 
    map(~ .x %>% 
          mutate(across(c(Start, End), ~ .x / 1000)))
  
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
  
  structure(out, class=c("trs_transcription", "trs_nesttiers", "trs_from_eaf",
                         class(out)))
}



##Given a trs_transcription object, output a dataframe of tier metadata
##If no tier has a TIER_ID, output dataframe's TIER_ID is row numbers. If no
##  tier has a PARTICIPANT, output dataframe's PARTICIPANT is NA
tier_metadata <- function(x) {
  library(purrr)
  if (!inherits(x, c("trs_transcription", "trs_nesttiers"))) {
    stop("x must be an object of classes trs_transcription and trs_nesttiers")
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

##Utility to convert times while retaining class attributes
convert_times <- function(x, from=c("s","ms"), to=c("s","ms")) {
  ##Check args
  if (!inherits(x, c("trs_transcription", "trs_nesttiers"))) {
    stop("x must be an object of classes trs_transcription and trs_nesttiers")
  }
  from <- match.arg(from)
  to <- match.arg(to)
  if (from==to) {
    warning("No time conversion performed (from and to are identical)")
    return(x)
  }
  
  ##Determine multiplier
  if (from == "s" && to == "ms") {
    multiplier <- 1000
  }
  if (from == "ms" && to == "s") {
    multiplier <- 1/1000
  }
  
  ##Convert times and restore class/attributes
  out <- 
    x %>% 
    map(~ .x %>% 
          mutate(across(c(Start, End), ~ .x * multiplier)))
  class(out) <- class(x)
  attributes(out) <- attributes(x)
  
  out
}

##Add annotation IDs to a transcription
##TODO: Make this a generic so x can be a trs_transcription, trs_tierset, or 
##  trs_tier
add_annotation_ids <- function(x, overwrite=c("error","warn","silent")) {
  library(purrr)
  library(dplyr)
  
  ##Check args
  if (!inherits(x, c("trs_transcription", "trs_nesttiers"))) {
    stop("x must be an object of classes trs_transcription and trs_nesttiers")
  }
  overwrite <- match.arg(overwrite)
  
  ##Check if there are already ANNOTATION_IDs, and act accordingly
  if (some(x, ~ "ANNOTATION_ID" %in% colnames(.x))) {
    if (overwrite=="error") {
      stop("add_annotation_ids() would overwrite existing ANNOTATION_ID attributes")
    }
    if (overwrite=="warn") {
      warning("Overwriting existing ANNOTATION_ID attributes")
    }
  }
  
  ##Get attributes so they can be restored after manipulation
  ##This is a temporary shim---once trs_ objects are implemented in an OO way,
  ##  these attributes will persist through object manipulation
  xAttr <- attributes(x)
  
  ##Get a vector of total rows from preceding tiers
  startIDs <- x %>% 
    map_int(nrow) %>% 
    lag(default=0) %>% 
    cumsum()
  ##Add ANNOTATION_IDs in sequence
  x <- map2(x, startIDs, 
            ~ .x %>% 
              mutate(ANNOTATION_ID = paste0("a", row_number() + .y),
                     .before=1))
  
  ##Restore attributes
  attributes(x) <- xAttr
  
  x
}

##Remove tier(s) from trs_transcription (while preserving attributes)
remove_tiers <- function(x, tiers=NULL, notier=c("error","warn","silent")) {
  ##Check args
  if (!inherits(x, c("trs_transcription", "trs_nesttiers"))) {
    stop("x must be an object of classes trs_transcription and trs_nesttiers")
  }
  if (is.null(tiers) || !is.character(tiers)) {
    stop("tiers must be a character vector")
  }
  notier <- match.arg(notier)
  
  ##Account for tiers not in x
  missingTiers <- setdiff(tiers, names(x))
  if (notier != "silent" && length(missingTiers) > 1) {
    msg <- paste("Some tiers are missing from x:",
                 paste(missingTiers, collapse=" "))
    if (notier=="error") {
      stop(msg)
    } 
    if (notier=="warn") {
      if (identical(tiers, missingTiers)) {
        stop("All tiers are missing from x")
      }
      warning(msg, "\nOnly removing tiers ",
              paste(setdiff(tiers, missingTiers), collapse=" "))
    }
  }
  
  ##Remove tier(s) (even if they don't exist)
  out <- x
  for (tier in tiers) {
    out[[tier]] <- NULL
  }
  
  out
}


##S3 generic for as.trs_eaf
as.trs_eaf <- function(x, ...) {
  UseMethod("as.trs_eaf")
}


##Convert a trs_transcription to an xml_document suitable for opening in Elan
as.trs_eaf.trs_transcription <- function(x, mediaFile=NULL, 
                                         minElan="misc/minimal-elan.xml", ...) {
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(xml2)
  
  ##Check args
  if (!inherits(x, c("trs_transcription", "trs_nesttiers"))) {
    stop("x must be an object of classes trs_transcription and trs_nesttiers")
  }
  minXML <- tryCatch(readLines(minElan),
                     error = \(e) stop("Minimal Elan file ", minElan, " not found"))
  
  # ##Convert Praat seconds to Elan milliseconds, if needed
  # if (inherits(x, "trs_from_textgrid")) {
  #   trs <- trs %>% 
  #     convert_times(from="s", to="ms")
  # }
  
  ##Reshape trs
  trs <- 
    x %>% 
    ##Convert to times milliseconds
    convert_times(from="s", to="ms") %>% 
    map(~ .x %>% 
          ##Remove old ANNOTATION_ID and TIME_SLOT_REF columns, if they exist
          select(-any_of(c("ANNOTATION_ID", "TIME_SLOT_REF1", "TIME_SLOT_REF2"))) %>% 
          ##Ensure integer, non-NA TIME_VALUEs
          mutate(across(c(Start, End), round)) %>%
          filter(if_all(c(Start, End), ~ !is.na(.x)))
    )
  
  ##Get time slots
  timeSlots <-
    trs %>% 
    map_dfr(as_tibble, .id="TIER_ID") %>% 
    mutate(ANNOTATION_ID = paste0("a", row_number())) %>% 
    pivot_longer(c(Start, End), names_to="Boundary", values_to="TIME_VALUE") %>% 
    arrange(TIME_VALUE) %>% 
    mutate(TIME_SLOT_ID = paste0("ts", row_number()),
           .before=TIME_VALUE)
  
  ##Create TIME_ORDER node
  TIME_ORDER <- 
    timeSlots %>% 
    with(map2(TIME_SLOT_ID, TIME_VALUE, 
              ##TIME_SLOT nodes have attributes r/t elements or values
              ~ `attributes<-`(list(),
                               list(TIME_SLOT_ID = .x, 
                                    TIME_VALUE = .y)))) %>% 
    set_names("TIME_SLOT") %>% 
    list(TIME_ORDER = .) %>%
    as_xml_document()
  
  ##Add timeslots to annotation dataframe
  tierDF <-
    timeSlots %>% 
    mutate(Boundary = paste0("TIME_SLOT_REF", if_else(Boundary=="Start", "1", "2"))) %>% 
    select(-TIME_VALUE) %>% 
    pivot_wider(names_from=Boundary, values_from=TIME_SLOT_ID) %>% 
    mutate(ANNOTATION_VALUE = replace_na(Text, "")) %>% 
    select(TIER_ID, ANNOTATION_VALUE, ANNOTATION_ID, 
           TIME_SLOT_REF1, TIME_SLOT_REF2) %>% 
    nest(ANNOTATION = -TIER_ID)
  
  ##List of TIER nodes
  TIERs <-
    tierDF %>% 
    ##Create lists of annotation nodes
    mutate(TIER = map(ANNOTATION,
                      ~ .x %>% 
                        rowwise() %>% 
                        mutate(ANNOTATION = list(
                          ANNOTATION = list(
                            ALIGNABLE_ANNOTATION = structure(list(
                              ANNOTATION_VALUE = list(ANNOTATION_VALUE)),
                              ANNOTATION_ID = ANNOTATION_ID, 
                              TIME_SLOT_REF1 = TIME_SLOT_REF1,
                              TIME_SLOT_REF2 = TIME_SLOT_REF2)))) %>% 
                        pull(ANNOTATION))) %>%
    ##Add tier-level attributes to lists of annotation nodes
    mutate(TIER = map2(TIER, TIER_ID,
                       ~ `attributes<-`(.x, list(LINGUISTIC_TYPE_REF="default-lt",
                                                 TIER_ID = .y,
                                                 PARTICIPANT = .y)) %>% 
                         ##Restore node names zapped by `attributes<-`()
                         set_names("ANNOTATION"))) %>%
    ##Extract from dataframe and add node name
    pull(TIER) %>% 
    set_names("TIER") %>%
    ##Convert to XML
    lmap(as_xml_document)
  
  ##Account for tiers without annotations (which are missing from TIERs list)
  missingTiers <- 
    trs %>% 
    names() %>% 
    setdiff(map_chr(TIERs, xml_attr, "TIER_ID"))
  ##Create empty TIER nodes if needed
  if (length(missingTiers) > 0) {
    emptyTIERs <- 
      missingTiers %>% 
      map(~ structure(list(),
                      LINGUISTIC_TYPE_REF="default-lt",
                      TIER_ID = .x,
                      PARTICIPANT = .x)) %>% 
      ##Add node name & convert to XML
      set_names("TIER") %>% 
      lmap(as_xml_document)
  }
  
  ##Remove comments from minimal XML
  minXML <- minXML[!startsWith(minXML, "<!--")]
  
  ##Create output file by filling elements into minimal XML
  outXML <- read_xml(paste(minXML, collapse=""))
  xml_replace(xml_find_first(outXML, "//TIME_ORDER"), TIME_ORDER)
  defaultTier <- xml_find_first(outXML, "//TIER[@TIER_ID='default']")
  if (length(missingTiers) > 0) {
    walk(rev(emptyTIERs), 
         ~ xml_add_sibling(defaultTier, .x))
  }
  walk(rev(TIERs),
       ~ xml_add_sibling(defaultTier, .x))
  xml_remove(defaultTier)
  ##Optionally add media file
  if (!is.null(mediaFile)) {
    header <- xml_find_first(outXML, "//HEADER")
    xml_attr(header, "MEDIA_FILE") <- mediaFile
  }
  
  ##Add new root-tag attributes, update overlapping attributes, and leave intact
  ##  attributes in minimal XML but not x
  ##Get xml attributes, leaving off R & trs attributes
  xAttr <- attributes(x) %>% 
    discard_at(c("names", "class", "overlapsInit", "overlaps", "overlapsNice"))
  ##Restore AUTHOR attribute from TextGrid Transcriber tier, if applicable
  if (inherits(x, "trs_from_textgrid")) {
    xAttr <- c(xAttr, 
               AUTHOR = x %>% 
                 pluck("Transcriber", "Text") %>% 
                 unique())
  }
  
  ##Get minimal XML attributes, restoring xsi: prefix to
  ##  noNamespaceSchemaLocation
  minAttr <- xml_attrs(outXML)
  ns <- which(names(minAttr)=="noNamespaceSchemaLocation")
  if (length(ns) > 0) {
    names(minAttr)[ns] <- "xsi:noNamespaceSchemaLocation"
  }
  ##Change attributes
  diffAttr <- minAttr[setdiff(names(minAttr), names(xAttr))]
  xml_attrs(outXML) <- c(xAttr, diffAttr)
  
  ##Add trs_eaf class
  outXML <- add_class(outXML, "trs_eaf")
  
  ##Return
  outXML
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
           CloseEnough = StartDiff < overlapThresh | EndDiff < overlapThresh,
           across(contains("TIER_ID"), 
                  list(priority = ~ match(.x, tierPriority)))) %>% 
    arrange(TIER_ID_priority, TIER_ID_overlapped_priority)
  
  ##Add metadata
  overlapBounds <- overlapBounds %>% 
    structure(class=c("trs_overlaps", class(overlapBounds)),
              tierPriority=tierPriority,
              overlapThresh=overlapThresh)
  
  overlapBounds
}

##Wrapper to handle two use cases: (1) finding overlaps, (2) finding then fixing
##  overlaps (corresponding to fixOverlaps=FALSE or TRUE, respectively).
##In case (1), returns a trs_overlaps object. In case (2), returns the original
##  trs_transcription object with a (possibly 0-row) trs_overlaps object in the
##  remainingOverlaps slot
##TODO: Split out fixOverlaps(), which adds overlap metadata to trs_overlaps
##  (so, taking the "Add info" bit from findOverlapsOneFile()) and resolves
##  overlaps. handleOverlapsOneFile() will still handle the looping and the 
##  messages
handleOverlapsOneFile <- function(x, nm, fixOverlaps=TRUE, 
                                  noOverlapCheckTiers=NULL, overlapThresh=NULL, 
                                  fixMethod=c("old","new"), 
                                  checkZeroWidth=c("error","warn","silent"),
                                  maxIters=NULL, reachMaxIters=c("error","warn","silent")[2]) {
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
  stopifnot(is.logical(fixOverlaps))
  stopifnot(is.character(noOverlapCheckTiers))
  stopifnot(is.numeric(overlapThresh) && overlapThresh >= 0)
  ##Additional args only come into play if fixing overlaps
  if (fixOverlaps) {
    fixMethod <- match.arg(fixMethod)
    checkZeroWidth <- match.arg(checkZeroWidth)
    stopifnot(is.numeric(maxIters) && maxIters > 0)
    reachMaxIters <- match.arg(reachMaxIters)
  }
  
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
                                     .default="Bystander")) %>% 
    ##Arrange by priority order
    arrange(
      ##Coincidentally, priority order == reverse alphabetical order
      desc(OverlapGroup),
      ##If multiple main speakers or bystanders, break ties by name order
      TIER_ID)
  ##Create priority vector
  tierPriority <- tierInfo$TIER_ID
  
  ##If any tier is missing ANNOTATION_ID, add them
  if (!every(x, ~ "ANNOTATION_ID" %in% colnames(.x))) {
    if (inherits(x, "trs_from_eaf")) {
      warning("Adding missing ANNOTATION_IDs")
    }
    x <- add_annotation_ids(x, "warn")
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
  
  ##If not fixing overlaps, exit early
  if (!fixOverlaps) {
    overlapsCurr <- findOverlapsOneFile(overlapTiers, tierPriority, 
                                        overlapThresh)
    return(overlapsCurr)
  }
  
  ##Single-df version of overlapTiers
  overlapTiersDF <- bind_rows(overlapTiers, .id="TIER_ID")
  
  ##Set up messages
  message("Overlaps:")
  msgCols <- c("Iteration", names(overlapTiers))
  msgHeader <- str_flatten(c("", msgCols), "  ")
  message(msgHeader)
  ##Function to construct each line of the overlaps table, with values right-
  ##  aligned under the header
  overlapsMsg <- function(overlapsCurr, iters, msgCols) {
    ##Get overlap counts
    numOverlaps <- 
      msgCols[-1] %>% 
      map_int(~ overlapsCurr %>% 
                as.data.frame() %>% ##Temporary while I wait to implement filter.trs_overlaps()
                filter(TIER_ID==.x) %>% 
                nrow())
    ##Print with iterations, padding with whitespace to width of each column 
    ##  header (including 2-space cushion)
    msgWidths <- str_width(msgCols) + 2
    c(iters, numOverlaps) %>% 
      str_pad(msgWidths) %>% 
      message()
  }
  
  #### MAIN EXECUTION ####
  ##Find initial overlaps
  overlapsInit <- findOverlapsOneFile(overlapTiers, tierPriority, overlapThresh)
  
  ##Initialize looping variables
  overlapsCurr <- overlapsInit
  overlapsOld <- NULL
  iters <- 0
  overlapsMsg(overlapsCurr, iters, msgCols)
  
  ##Iteratively find and fix overlaps until a stable solution has been found
  ##  OR the loop hits its maximum number of iterations
  while (nrow(overlapsCurr) > 0 && !identical(overlapsCurr, overlapsOld)) {
    ##Increment iteration counter
    iters <- iters + 1
    ##Exit loop if maximum iterations reached
    if (iters==maxIters) {
      ##Optionally error out or warn
      maxIterMsg <- paste0("Overlap-fixing reached maximum iterations (", 
                           maxIters, ") without reaching a stable solution")
      if (reachMaxIters=="error") {
        stop(maxIterMsg)
      }
      if (reachMaxIters=="warn") {
        warning(maxIterMsg)
      }
      
      break
    }
    ##Curr is now Old
    overlapsOld <- overlapsCurr
    
    ##Identify timeslots to change
    if (fixMethod=="old") {
      ##Old method: Set the overlapping boundary to the overlapped turn's
      ##  closest boundary (regardless of priority)
      ##Old-method code is written to ostensibly fix each tier in reverse-
      ##  priority order, but in reality, it's the same as if all tiers are
      ##  fixed simultaneously (as they are here)
      for (tier in rev(tierPriority)) {
        newStarts <-
          overlapsCurr %>%
          filter(CloseEnough,
                 TIER_ID==tier,
                 Bound=="Start") %>%
          mutate(Start = if_else(StartDiff < EndDiff, Start_overlapped, End_overlapped)) %>%
          select(TIER_ID, ANNOTATION_ID, Start) %>% 
          ##Just get first row for each TIER_ID + ANNOTATION_ID (in case the 
          ##  boundary overlaps multiple tiers), for the sake of rows_update()
          slice(1, .by=c("TIER_ID","ANNOTATION_ID"))
        newEnds <-
          overlapsCurr %>%
          filter(CloseEnough,
                 TIER_ID==tier,
                 Bound=="End") %>%
          mutate(End = if_else(StartDiff < EndDiff, Start_overlapped, End_overlapped)) %>%
          select(TIER_ID, ANNOTATION_ID, End) %>% 
          ##Just get first row for each TIER_ID + ANNOTATION_ID (in case the 
          ##  boundary overlaps multiple tiers), for the sake of rows_update()
          slice(1, .by=c("TIER_ID","ANNOTATION_ID"))
        
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
        overlapsMsg(overlapsCurr, iters, msgCols)
      }
    }
  }
  
  ##Modify original transcription with new timestamps
  for (tierName in names(overlapTiers)) {
    x[[tierName]] <- x[[tierName]] %>% 
      rows_update(overlapTiers[[tierName]], "ANNOTATION_ID")
  }
  
  ##Add overlaps as an attribute
  attr(x, "overlapsInit") <- overlapsInit
  attr(x, "overlaps") <- overlapsCurr
  
  x
}


# Miscellaneous utilities -------------------------------------------------

##Low-level utility for adding new class(es) to x, facilitating cleaner code
##  in functional-programming settings (e.g., purrr:::map*()) or in pipe chains
##If newClass is NA, it won't be added
add_class <- function(x, newClass) {
  stopifnot(all(is.na(newClass) | is.character(newClass)))
  curr <- class(x)
  
  ##Handle cases where x already inherits from 1+ newClass
  overlap <- intersect(curr, newClass)
  if (identical(overlap, newClass)) {
    stop("x already has class(es) ", paste(newClass, collapse=" "))
  }
  if (length(overlap) > 0) {
    newClass <- setdiff(newClass, overlap)
    warning("x already has class(es) ", paste(overlap, collapse=" "), "\n",
            "  Only adding class(es) ", paste(newClass, collapse=" "))
  }
  
  ##Remove NAs
  newClass <- newClass[!is.na(newClass)]
  
  ##Add new class and return
  class(x) <- c(newClass, curr)
  x
}

##Low-level utility for adding new attribute(s) to x, facilitating cleaner code
##  in functional-programming settings (e.g., purrr::map*()) or in pipe chains
add_attributes <- function(x, newAttr, 
                           overwrite=c("error","warn","warn-overwrite","silent")) {
  ##Check args
  if (is.atomic(newAttr)) {
    newAttr <- as.list(newAttr)
  }
  newAttrNames <- names(newAttr)
  if (any(duplicated(newAttrNames))) {
    stop("Duplicate names in newAttr")
  }
  overwrite <- match.arg(overwrite)
  
  ##Get current
  curr <- attributes(x)
  currNames <- names(curr)
  
  ##Handle cases where x already has 1+ newAttr
  overlap <- intersect(currNames, newAttrNames)
  if (length(overlap) > 0) {
    if (overwrite=="error") {
      ##Error out if any overlap
      stop("x already has attribute(s) ", paste(overlap, collapse=" "))
      
    } else if (overwrite=="warn") {
      ##Warn and only add new attributes (error out if all newAttr already exist)
      newAttr <- newAttr[!(newAttrNames %in% overlap)]
      if (length(newAttr)==0) {
        stop("x already has attribute(s) ", paste(overlap, collapse=" "), "\n",
             "  No new attributes would be added ")
      }
      warning("x already has attribute(s) ", paste(overlap, collapse=" "), "\n",
              "  Only adding attribute(s) ", paste(names(newAttr), collapse=" "))
      
    } else if (overwrite=="warn-overwrite") {
      ##Warn but overwrite attributes anyway
      warning("Overwriting existing attribute(s) ", paste(overlap, collapse=" "))
    }
  }
  
  ##Add/overwrite attributes and return
  attributes(x) <- c(curr, newAttr)
  x
}


# Object-oriented ---------------------------------------------------------

##Classes:
##- Basic structures:
##  - trs_transcription
##  - trs_tier
##  - trs_overlaps
##- Subset of tiers that's *not* a full transcription (and maybe doesn't have full metadata):
##  - trs_tierset
##- Additional classes for trs_transcription and trs_tierset that denote shape:
##  - trs_nesttiers
##  - trs_single

##Constructors


##Validators


##Helpers


##Low-level methods:
##- Subset (turns .trs_transcription.trs_nesttiers into .trs_nesttiers)
##- Combine?

##Change shape:
##- tierset to single (make TIER_ID a factor so we don't lose information about empty tiers)
##- single to tierset
##- add tier
##- remove tier

##Overlaps:
##- detect
##- fix

##I/O:
##- read from eaf/textgrid
##- write to eaf/textgrid
