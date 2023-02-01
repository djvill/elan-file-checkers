##Utilities for running tests

##Is an element (shiny.tag or xml_node) displayed?
is.displayed <- function(x) {
  if (!any(c("shiny.tag", "xml_node") %in% class(x))) {
    stop("is.displayed() only works with shiny.tag or xml_node objects, not ", 
         paste(class(x), collapse="/"), " objects.")
  }
  
  if ("shiny.tag" %in% class(x)) {
    sty <- x$attribs$style
  } else {
    require(xml2)
    sty <- xml_attr(x, "style")
  }
  
  !grepl("display:\\s*none", sty)
}

##shinytest:::write_utf8() clone
write_utf8 <- function (text, ...) {
  writeBin(charToRaw(enc2utf8(text)), ...)
}

##Take snapshot: compare to shinytest:::sd_snapshot()
snap <- function(app, name=NULL, path=getwd(), incl_eaflist=FALSE) {
  library(rvest)
  library(purrr)
  library(dplyr)
  library(stringr)
  library(jsonlite)
  
  ##Get output html pane (to be parsed for test values)
  allVals <- app$getAllValues()
  mainUI <- read_html(allVals$output$out$html)
  
  ##"Unpack" test values from html container
  testVals <- allVals$output$export$html %>% 
    read_html() %>% 
    html_elements("div")
  exported <- 
    testVals %>% 
    map(~ .x %>% html_text() %>% fromJSON()) %>% 
    set_names(testVals %>% html_attr("id"))
  
  ##Get info on step headings, subheads, and details
  stepHeads <- data.frame(
    id = html_attr(html_elements(mainUI, "h2"), "id"),
    class = html_attr(html_elements(mainUI, "h2"), "class"),
    displayed = map_lgl(html_elements(mainUI, "h2"), is.displayed)
  )
  stepSubheads <- html_elements(mainUI, "h3") %>% 
    {data.frame(id = html_attr(., "id"),
                displayed = map_lgl(., is.displayed))}
  success <- is.displayed(html_element(mainUI, "#downloadSubhead"))
  stepDetails <- html_elements(mainUI, ".details") %>%
    {data.frame(id = html_attr(., "id"),
                hasContent = map_lgl(., ~ nchar(html_text(.x)) > 0),
                content = map_chr(., as.character))}
  
  ##Get formatted version of the last details element with content
  lastDetailID <- 
    stepDetails %>% 
    filter(hasContent) %>% 
    tail(1) %>% 
    pull(id)
  lastDetailElem <- html_elements(mainUI, paste0("#", lastDetailID))
  
  ##Translate from HTML to vector (checkDetails) or named list (others)
  if (lastDetailID=="checkDetails") {
    lastDetails <- 
      lastDetailElem %>% 
      html_children() %>% 
      html_text()
  } else {
    ##Get file names from li children
    fileNames <- 
      lastDetailElem %>% 
      html_children() %>% 
      html_text2() %>% 
      str_replace(regex("In file (.+?):.+", dotall=TRUE), "\\1")
    
    if (lastDetailID=="tierDetails") {
      lastDetails <- 
        lastDetailElem %>% 
        html_children() %>% 
        map(~ .x %>% 
              html_elements("li") %>% 
              html_text()) %>% 
        set_names(fileNames)
    } else if (lastDetailID=="dictDetails") {
      tierNames <-
        lastDetailElem %>% 
        html_children() %>% 
        map(~ .x %>% 
              html_elements(".file-headed > li") %>% 
              html_text2() %>% 
              str_replace(regex("On tier (.+?):.+", dotall=TRUE), "\\1"))
      lastDetails <- 
        lastDetailElem %>% 
        html_elements(".file-headed") %>% 
        map2(tierNames,
             ~ .x %>% 
               html_elements(".tier-headed") %>% 
               map(
                 ~ .x %>% 
                   html_children() %>% 
                   html_text()) %>% 
               set_names(.y)
        ) %>% 
        set_names(fileNames)
    } else if (lastDetailID=="overlapsDetails") {
      require(lubridate)
      lastDetails <-
        lastDetailElem %>% 
        html_elements(".file-headed") %>% 
        ##Parse as dataframe with added columns for times in seconds
        map(~ .x %>% 
              html_table() %>% 
              mutate(across(-Tier, list(sec = ~ .x %>% 
                              hms() %>% 
                              period_to_seconds())))
              ) %>% 
        set_names(fileNames)
    }
  }
  
  ##Put together snapshot
  ##Keep eaflist out of exported (since it takes up a lot of space)
  eaflist <- exported$eaflist
  exported$eaflist <- NULL
  ##Put together list
  out <- list(success = success,
              export = exported,
              elements = list(stepHeads = stepHeads,
                              stepSubheads = stepSubheads,
                              stepDetails = stepDetails,
                              lastDetailID = lastDetailID,
                              lastDetails))
  names(out$elements)[length(out$elements)] <- lastDetailID
  ##Optionally include eaflist
  if (incl_eaflist) {
    out <- c(out, eaflist=list(eaflist))
  }
  
  ##Optionally save snapshot
  if (!is.null(name)) {
    ##Set up directory name
    if (!endsWith(path, "-current/")) {
      current_dir <- paste0(path, "-current/")
    } else {
      current_dir <- path
    }
    
    ##If this is the first snapshot, start with empty directory
    if (!exists("fileCounter")) {
      fileCounter <<- 0
    }
    if (fileCounter == 0) {
      if (dir.exists(current_dir)) {
        unlink(current_dir, recursive=TRUE)
      }
      dir.create(current_dir, recursive = TRUE)
    }
    
    ##Save snapshot & increment counter
    out %>% 
      toJSON() %>% 
      prettify(indent=2) %>% 
      write_utf8(paste0(current_dir, "/", name, ".json"))
    fileCounter <<- fileCounter + 1
  }
  
  invisible(out)
}

##Get file from snapshot: compare to shinytest:::sd_snapshotDownload()
snapDownload <- function(app, name=NULL, path=getwd(), 
                         id="OutputFile", suffix="_out") {
  ##Only works if just 1 file has been uploaded
  files <- app$getAllValues()$input$files
  if (nrow(files) != 1) {
    stop("snapDownload() only works if exactly 1 file has been uploaded")
  }
  
  ##Get file from app
  url <- app$findElement(paste0("#", id))$getAttribute("href")
  req <- shinytest:::httr_get(url)
  
  ##Optionally save file
  if (!is.null(name)) {
    ##Set up directory name
    if (!endsWith(path, "-current/")) {
      current_dir <- paste0(path, "-current/")
    } else {
      current_dir <- path
    }
    
    ##If this is the first snapshot, start with empty directory
    if (!exists("fileCounter")) {
      fileCounter <<- 0
    }
    if (fileCounter == 0) {
      if (dir.exists(current_dir)) {
        unlink(current_dir, recursive=TRUE)
      }
      dir.create(current_dir, recursive = TRUE)
    }
    
    ##Save file and increment fileCounter
    fName <- sub("\\.eaf", paste0(suffix, ".eaf"), name)
    writeBin(req$content, paste0(current_dir, "/", fName))
    fileCounter <<- fileCounter + 1
  }
  
  invisible(req$content)
}
