
library(shiny)
library(xml2)
library(stringr)
library(purrr)
library(tidyr)
library(dplyr)
library(magrittr)
library(here)


# UI ----------------------------------------------------------------------
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .btn-file , .form-control {
        height: 300px;
      }"))
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
    
    mainPanel(verbatimTextOutput("debug"))
    
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


# Server ------------------------------------------------------------------
server <- function(input, output) {
  # Set up file structures --------------------------------------------------
  
  ##Dataframe of files
  files <- eventReactive(input$files, {
    ##Start with input files dataframe
    input$files %>% 
      ##Add neighborhood, speaker number, and file number
      ##Will need to be extended to multiple speakers, non-interview tasks, etc.
      tidyr::extract(name, c("SpkrCode", "FileNum"), 
                     "((?:CB|FH|HD|LV)\\d+)-(\\d+).+", FALSE, TRUE) %>% 
      tidyr::extract(SpkrCode, c("Neighborhood", "SpeakerNum"),
                     "([A-Z]{2})(\\d+)", FALSE, TRUE) %>% 
      rename(File = name) %>% 
      ##Sort
      arrange(Neighborhood, SpeakerNum, FileNum, File)
  })
  
  
  ##Are all files .eaf?
  fileExtValid <- reactive({
    is.null(input$files) || all(endsWith(input$files$name, ".eaf"))
  })
  fileExtValidOverride <- TRUE
  
  ##From here on out, things only run nicely if fileExtValid() or fileExtValidOverride
  
  ##Read files: Get a list that's nrow(files()) long, each element an xml_document
  eaflist <- reactive({
    req(fileExtValid() || fileExtValidOverride)
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
      mutate(SpkrTier = str_detect(tolower(PARTICIPANT), "comments|noise|redact", negate=TRUE)) %>% 
      ##Add info from files()
      left_join(files(), by="File")
  })
  
  spkrTiers <- reactive({
    setNames(lapply(eaflist(), function(eaf) {
      xml_find_all(eaf, paste("//TIER[@TIER_ID!='comment' and @TIER_ID!='comments' and",
                              "@TIER_ID!='Comment' and @TIER_ID!='Comments' and", 
                              "@TIER_ID!='noise' and @TIER_ID!='noises' and",
                              "@TIER_ID!='Noise' and @TIER_ID!='Noises' and",
                              "(@LINGUISTIC_TYPE_REF='default-lt' or @LINGUISTIC_TYPE_REF='UtteranceType')]"))
    }), names(eaflist()))
  })
  ##Could also make a list of objects that includes file name, eaf, and speaker tiers, so I don't have to do things like lapply(names(eaflist()), function(x) {spkrTiers()[[x]] ... })
  
  # Debugging output --------------------------------------------------------
  output$debug <- renderPrint({
    
    ##Function that takes a tier df as input and outputs tier issues
    # tierIssues <- function(df) {
    tierIssues <- function(df, filename) {
      ##Initialize empty issues character vector
      issues <- character(0L)
      
      ##Add tier number (as backup for IDing tiers w/o TIER_ID attr)
      df <- df %>% 
        mutate(tierNum = paste("Tier", row_number()))
      
      ##Handle missing tiers
      checkTiers <- c(paste0(c("", "Interviewer "), unique(df$SpkrCode)),
        "Comments", "Noise", "Redact")
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
    
    tierMsg <- 
      tierInfo() %>% 
      ##Look for tier issues for each file
      nest(data = -File) %>% 
      mutate(issues = map(data, tierIssues)) %>% 
      ##Turn into list with one element for each file
      pull(issues, name=File) %>% 
      ##Only keep files with issues
      keep(~ length(.x) > 0)
    
    list(tierMsg = tierMsg)
    
  })
  
  
  # Output: header ----------------------------------------------------------
  output$checkHead <- renderText({
    paste("Checking",
          paste(files()$name, collapse=", "))
  })
  
  # Output: tier issues -----------------------------------------------------
  output$tiersHead <- renderPrint({
    if (fileExtValid()) {
      cat("Step 1: Validating tier names and attributes...")
    } else {
      cat("ERROR: The checker only works on files with an .eaf file extension")
    }
  })
  
  tierIssues <- reactive({
    req(tiers())
    
    issues <- lapply(names(eaflist()), function (x) {
      eaf <- eaflist()[[x]]
      message <- character(0)
      mainSpkrName <- strsplit(x, "-")[[1]][1]
      if (any(is.na(sapply(xml_find_all(eaf, "//TIER[@LINGUISTIC_TYPE_REF='default-lt' or @LINGUISTIC_TYPE_REF='UtteranceType']"), xml_attr, "ANNOTATOR")))) {
        message <- c(message, "One or more tiers is missing an Annotator attribute")
      }
      if (length(xml_find_all(eaf, paste0("//TIER[@TIER_ID='", mainSpkrName, "']")))==0) {
        message <- c(message, paste0("There are no tiers with Tier Name '", mainSpkrName, "'"))
      } 
      if (length(xml_find_all(eaf, paste0("//TIER[@TIER_ID='Interviewer ", mainSpkrName, "']")))==0) {
        message <- c(message, paste0("There are no tiers with Tier Name 'Interviewer ", mainSpkrName, "'"))
      }
      if (length(spkrTiers()[[x]]) > 0) {
        message <- c(message, unlist(sapply(spkrTiers()[[x]], function(spkr) {
          tierID <- xml_attr(spkr, "TIER_ID")
          participant <- xml_attr(spkr, "PARTICIPANT")
          if (is.na(participant)) {
            paste0("The tier with name '", tierID, "' is missing a Participant attribute")
          } else if (participant != tierID) {
            str_glue("Mismatched tier name ({tierID}) & Participant attribute ({participant})")
          }
        })))
      }
      message
    })
    names(issues) <- names(eaflist())
    if (length(issues)==1) issues <- issues[[1]] #Don't show file name if only one file uploaded
    issues <- issues[lapply(issues, length)>0] ##Only show files with issues/if no issues in any file, return 0-length list
    issues
  })
  
  output$tiersTop <- renderPrint({
    if (fileExtValid()) {
      if (length(tierIssues())==0) {
        cat("All good!")
      } else {
        cat("The tier checker returned the following issue(s), which can be resolved using Change Tier Attributes in Elan:")
      }
    }
  })
  output$tiers <- renderPrint({
    if (fileExtValid()) {
      if (length(tierIssues())>0) {
        tierIssues()
      }
    }
  })
  
  # Output: dictionary checker ----------------------------------------------
  output$dictHead <- renderPrint({
    if (fileExtValid()) {
      cat("Step 2: Checking for out-of-dictionary words...")
    }
  })
  
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
    
  dictIssues <- reactive({
    if (length(tierIssues())==0) {
      issues <- lapply(names(eaflist()), function (x) {
        eaf <- eaflist()[[x]]
        badWords <- map(spkrTiers()[[x]], function(spkr) {
          wordChunk <- xml_text(xml_find_all(spkr, "./ANNOTATION/ALIGNABLE_ANNOTATION/ANNOTATION_VALUE"))
          wordChunk <- gsub("([[:alpha:]]) ([?.-])>", "\\1\\2>", wordChunk) ##Unstrand valid punctuation within angle brackets
          wordChunk <- gsub("\\{.*?\\}", "", wordChunk) ##Ignore text within curly braces ("behaviour of speech")
          words <- strsplit(wordChunk, " ") %>% unlist() %>% unique()
          words <- words[words != ""] ##Ignore line-leading/line-trailing whitespace
          words <- words[!(words %in% c(".", "?", "-", "--"))] ##Ignore standalone valid punctuation
          permitAngleBrackets <- TRUE ##Set to TRUE to relax restrictions on angle brackets (allow single words in angle brackets)
          if (permitAngleBrackets) words <- gsub("^<(.+)>$", "\\1", words) ##Strip matched angle brackets
          words <- words[!grepl("\\[.+\\]$", words) | grepl("\\[.*\\]\\(.*\\)$", words)] ##Ignore words with valid bracket pronounce codes (sui generis words)
          # words <- words %>% gsub("^\\{", "", .) %>% gsub("\\}$", "", .) ##Strip curly braces ("behaviour of speech")
          # words <- gsub("[][]", "", words) ##Strip brackets
          checkWords <- gsub("\\[", "", words) %>% gsub("\\]$", "", .) ##Strip brackets
          checkWords <- gsub("[.?-]$", "", checkWords) ##Strip attached valid punctuation
          # checkWords <- tolower(gsub("(.+)\\((.+)\\)", "\\2", checkWords)) ##For words with paren codes, use the paren code for checking
          checkWords <- gsub("(.+)\\((.+)\\)", "\\2", checkWords) ##For words with paren codes, use the paren code for checking
          checkWords <- checkWords %>% ##Use a clitic-stripped version of the word for checking
            gsub("'s$", "", .) %>% gsub("s'$", "s", .) %>% gsub("'ve$", "", .) %>% gsub("'d$", "", .)
          bw <- words[!(checkWords %in% dict)]
          checkBW <- gsub("s$", "", bw) %>% tolower() ##For words ending in -s not in the dictionary, recheck based on version w/o -s 
          ##(See https://sourceforge.net/p/labbcat/code/HEAD/tree/WEB-INF/classes/nz/ac/canterbury/ling/celex/english/CelexEnglishDictionary.java#l846 [r2458])
          bw <- bw[!(checkBW %in% dict)]
        })
        badWords <- badWords[sapply(badWords, length)>0]
        badWords
      })
      names(issues) <- names(eaflist())
      if (length(issues)==1) issues <- issues[[1]] #Don't show file name if only one file uploaded
      issues <- issues[sapply(issues, length)>0]
      issues
    }
  })
  output$dictTop <- renderPrint({
    if (length(tierIssues())==0) {
      if (length(dictIssues())==0) {
        cat("No out-of-dictionary words.")
      } else {
        cat(paste("The dictionary checker returned the following word(s).",
                  "Please correct misspellings, fix punctuation, and/or add pronunciation codes;",
                  "Email Dan with any words that should be added to the dictionary."))
      }
    }
  })
  output$dict <- renderPrint({
    if (length(dictIssues()) > 0) {
      dictIssues()
    }
  })
  
  # Output: overlap checker/fixer -------------------------------------------
  output$overlapsHead <- renderPrint({
    if (fileExtValid()) {
      cat("Step 3: Checking for overlaps...")
    }
  })
  formatTimes <- function(time) {
    time <- c(((time/1000) %/% 60) %/% 60,
              ((time/1000) %/% 60) %% 60,
              (time/1000) %% 60)
    paste(sprintf("%02i", time[1]), sprintf("%02i", time[2]), sprintf("%06.3f", time[3]), sep=":")
  }
  closeEnough <- 500
  eaflistNew <- reactive({
    if (length(tierIssues())==0 & length(dictIssues())==0) {
      setNames(lapply(names(eaflist()), function(x) {
        numOverlaps <- numOverlapsFixed <- 0
        eaf <- eaflist()[[x]]
        tmStamps <- as.numeric(xml_attr(xml_children(xml_find_first(eaf, "TIME_ORDER")), "TIME_VALUE"))
        names(tmStamps) <- xml_attr(xml_children(xml_find_first(eaf, "TIME_ORDER")), "TIME_SLOT_ID")
        tmStamps <- sort(tmStamps)
        spkrTiersNonempty <- spkrTiers()[[x]][sapply(spkrTiers()[[x]], function(spkr) length(xml_children(spkr)))>0]
        names(spkrTiersNonempty) <- sapply(spkrTiersNonempty, xml_attr, attr="TIER_ID")
        ##Construct by-tier list of data.frames of turn IDs, start times, end times,
        spkrTimesAll <- map(spkrTiersNonempty, function(tier) {
          spkrTimes <- data.frame(AnnID=xml_attr(xml_find_all(tier, "./ANNOTATION/ALIGNABLE_ANNOTATION"), "ANNOTATION_ID"),
                                  Start=tmStamps[xml_attr(xml_find_all(tier, "./ANNOTATION/ALIGNABLE_ANNOTATION"), "TIME_SLOT_REF1")],
                                  End=tmStamps[xml_attr(xml_find_all(tier, "./ANNOTATION/ALIGNABLE_ANNOTATION"), "TIME_SLOT_REF2")])
        })
        ##For each turn in each nonempty speaker tier, see if there's an overlap. If so, resolve it.
        for (spkr in seq_len(length(spkrTimesAll))) {
          spkrTimes <- spkrTimesAll[[spkr]]
          otherSpkrs <- do.call(rbind, spkrTimesAll[-spkr])
          tier <- names(spkrTimesAll)[spkr]
          for (turn in seq_len(nrow(spkrTimes))) {
            if (any(spkrTimes$Start[turn] > otherSpkrs$Start & spkrTimes$Start[turn] < otherSpkrs$End)) {
              numOverlaps <- numOverlaps + 1
              if (min(abs(spkrTimes$Start[turn] - c(otherSpkrs$Start, otherSpkrs$End))) <= closeEnough) {
                numOverlapsFixed <- numOverlapsFixed + 1
                nearest <- which.min(abs(spkrTimes$Start[turn] - c(otherSpkrs$Start, otherSpkrs$End)))
                nearest <- c(otherSpkrs$Start, otherSpkrs$End)[nearest]
                oll <- names(tmStamps)[which(tmStamps==nearest)[1]]
                nodeStr <- paste0("//TIER[@TIER_ID='", tier, "']/ANNOTATION/ALIGNABLE_ANNOTATION[@ANNOTATION_ID='",
                                  spkrTimes$AnnID[turn], "']")
                xml_set_attr(xml_find_first(eaf, nodeStr), "TIME_SLOT_REF1", oll)
                spkrTimes$Start[turn] <- nearest
              }
            }
            if (any(spkrTimes$End[turn] > otherSpkrs$Start & spkrTimes$End[turn] < otherSpkrs$End)) {
              numOverlaps <- numOverlaps + 1
              if (min(abs(spkrTimes$End[turn] - c(otherSpkrs$Start, otherSpkrs$End))) <= closeEnough) {
                numOverlapsFixed <- numOverlapsFixed + 1
                nearest <- which.min(abs(spkrTimes$End[turn] - c(otherSpkrs$Start, otherSpkrs$End)))
                nearest <- c(otherSpkrs$Start, otherSpkrs$End)[nearest]
                olr <- names(tmStamps)[which(tmStamps==nearest)[1]]
                nodeStr <- paste0("//TIER[@TIER_ID='", tier, "']/ANNOTATION/ALIGNABLE_ANNOTATION[@ANNOTATION_ID='",
                                  spkrTimes$AnnID[turn], "']")
                xml_set_attr(xml_find_first(eaf, nodeStr), "TIME_SLOT_REF2", olr)
                spkrTimes$End[turn] <- nearest
              }
            }
          }
          spkrTimesAll[[spkr]] <- spkrTimes
        }
        attr(eaf, "NumOverlaps") <- numOverlaps
        attr(eaf, "NumOverlapsFixed") <- numOverlapsFixed
        eaf
      }), names(eaflist())
      )
    }
  })
  overlapIssues <- reactive({
    if (length(tierIssues())==0 & length(dictIssues())==0) {
      issues <- lapply(names(eaflistNew()), function (x) {
        eaf <- eaflistNew()[[x]]
        tmStamps <- as.numeric(xml_attr(xml_children(xml_find_first(eaf, "TIME_ORDER")), "TIME_VALUE"))
        names(tmStamps) <- xml_attr(xml_children(xml_find_first(eaf, "TIME_ORDER")), "TIME_SLOT_ID")
        tmStamps <- sort(tmStamps)
        spkrTiersNonempty <- spkrTiers()[[x]][sapply(spkrTiers()[[x]], function(spkr) length(xml_children(spkr)))>0]
        spkrTimesAll <- lapply(spkrTiersNonempty, function(spkr) {
          data.frame(AnnID=xml_attr(xml_find_all(spkr, "./ANNOTATION/ALIGNABLE_ANNOTATION"), "ANNOTATION_ID"),
                     Start=tmStamps[xml_attr(xml_find_all(spkr, "./ANNOTATION/ALIGNABLE_ANNOTATION"), "TIME_SLOT_REF1")],
                     End=tmStamps[xml_attr(xml_find_all(spkr, "./ANNOTATION/ALIGNABLE_ANNOTATION"), "TIME_SLOT_REF2")])
        })
        names(spkrTimesAll) <- sapply(spkrTiersNonempty, xml_attr, attr="TIER_ID")
        
        message <- unlist(sapply(1:length(spkrTimesAll), function(spkr) {
          spkrTimes <- spkrTimesAll[[spkr]]
          spkrName <- names(spkrTimesAll)[spkr]
          otherSpkrs <- do.call(rbind, spkrTimesAll[-spkr])
          tier <- names(spkrTimesAll)[spkr]
          sapply(1:nrow(spkrTimes), function(turn) {
            overlapLeft <- any(spkrTimes$Start[turn] > otherSpkrs$Start & spkrTimes$Start[turn] < otherSpkrs$End)
            overlapRight <- any(spkrTimes$End[turn] > otherSpkrs$Start & spkrTimes$End[turn] < otherSpkrs$End)
            if (overlapLeft & overlapRight) {
              c(paste("Left margin overlap for", spkrName, "interval", turn,
                      "(interval starts at", paste0(formatTimes(spkrTimes$Start[turn]), ")")),
                paste("Right margin overlap for", spkrName, "interval", turn,
                      "(interval ends at", paste0(formatTimes(spkrTimes$End[turn]), ")")))
            } else if (overlapLeft) {
              paste("Left margin overlap for", spkrName, "interval", turn,
                    "(interval starts at", paste0(formatTimes(spkrTimes$Start[turn]), ")"))
            } else if (overlapLeft) {
              paste("Right margin overlap for", spkrName, "interval", turn,
                    "(interval ends at", paste0(formatTimes(spkrTimes$End[turn]), ")"))
            }
          })
        }))
        message
      })
      names(issues) <- paste0(names(eaflistNew()), " (",
                              sapply(eaflistNew(), attr, "NumOverlapsFixed"), 
                              " overlaps automatically resolved)")
      if (length(issues)==1) issues <- issues[[1]] #Don't show file name if only one file uploaded
      issues <- issues[sapply(issues, length)>0]
      issues
    }
  })
  output$overlapsTop <- renderPrint({
    req(files())
    if (length(tierIssues())==0 & length(dictIssues())==0) {
      overlapsFixed <- sum(sapply(eaflistNew(), attr, "NumOverlapsFixed"))
      if (length(overlapIssues())==0) {
        if (overlapsFixed > 0) {
          header <- paste("No more overlaps!", overlapsFixed, "overlaps were resolved automatically.")
        } else {
          header <- "There were no overlaps in the files you uploaded. High five!"
        }
      } else {
        header <- paste("The overlap checker automatically resolved", overlapsFixed, 
                        "overlaps, but could not resolve the following:")
      }
      cat(header)
    }
  })
  output$overlaps <- renderPrint({
    if (length(tierIssues())==0 & length(overlapIssues())>0) {
      overlapIssues()
    }
  })
  
  # Create output file(s) ---------------------------------------------------
  output$OutputFile <- downloadHandler(
    filename=function() {
      if (length(eaflistNew())==1) {
        names(eaflistNew())
      } else {
        "corrected_eafs.zip"
      }
    },
    content=function(file) {
      if (length(eaflistNew())==1) {
        write_xml(eaflistNew()[[1]], file)
      } else {
        sapply(names(eaflistNew()), function(eafname) write_xml(eaflistNew()[[eafname]], eafname))
        zip(file, names(eaflistNew()))
      }
    }
  )
  ##Display download button, or instruct the user to finish the job.
  output$download <- renderUI({
    if (length(tierIssues())==0 & length(dictIssues())==0) {
      overlapsFixed <- sum(sapply(eaflistNew(), attr, "NumOverlapsFixed"))
      if (overlapsFixed > 0) {
        downloadButton("OutputFile", "Download corrected file(s)") ##Change this to reflect # of corrected files
      } else {
        h4("Please", 
           strong("upload these files to the NZILBB server"), 
           "(if you haven't already) and",
           strong("mark these files as done on the", 
                  a("Transcription Log", 
                    href="https://docs.google.com/spreadsheets/d/1hSBUvD7OnbdTO5QQt4xKvIs14qFwh6WzTUoYDILYDM4/edit#gid=0")))
      }
    }
  })
}

shinyApp(ui = ui, server = server)