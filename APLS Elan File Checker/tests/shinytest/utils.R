##Utilities for running tests

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

##snapshot: compare to shinytest:::sd_snapshot()
snap <- function(app, name, path) {
  library(rvest)
  library(purrr)
  library(dplyr)
  library(stringr)
  library(jsonlite)
  
  current_dir <- paste0(path, "-current")
  if (!exists("fileCounter")) {
    fileCounter <- 0
  }
  if (fileCounter == 0) {
    if (dir.exists(current_dir)) {
      unlink(current_dir, recursive=TRUE)
    }
    dir.create(current_dir, recursive = TRUE)
  }
  
  allVals <- app$getAllValues()
  mainUI <- read_html(allVals$output$out$html)
  testVals <- allVals$export
  
  stepHeads <- data.frame(
    id = html_attr(html_elements(mainUI, "h2"), "id"),
    class = html_attr(html_elements(mainUI, "h2"), "class"),
    displayed = map_lgl(html_elements(mainUI, "h2"), is.displayed)
  )
  stepSubheads <- html_elements(mainUI, "h3") %>% 
    {data.frame(id = html_attr(., "id"),
                displayed = map_lgl(., is.displayed))}
  stepDetails <- html_elements(mainUI, ".details") %>%
    {data.frame(id = html_attr(., "id"),
                hasContent = map_lgl(., ~ nchar(html_text(.x)) > 0),
                content = map_chr(., as.character))}
  lastDetails <- stepDetails %>% 
    filter(hasContent) %>% 
    tail(1) %>% 
    pull(id)
  formatDetails <- function(x) {
    children <- 
      x %>% 
      html_children() %>% 
      map(~ html_text(html_elements(.x, "li")))
    fileNames <- 
      x %>% 
      html_children() %>% 
      html_text2() %>% 
      str_replace(regex("In file (.+?):.+", dotall=TRUE), "\\1")
    set_names(children, fileNames)
  }
  
  out <- list(export = list(fileDF = testVals$fileDF %>% 
                              ##Datapaths are in random tmp directories so they
                              ##  create false positive diffs
                              select(-datapath),
                            # eaflist = testVals$eaflist,
                            tierInfo = testVals$tierInfo %>% 
                              select(-datapath)),
              elements = list(stepHeads = stepHeads,
                              stepSubheads = stepSubheads,
                              stepDetails = stepDetails,
                              lastDetails = lastDetails,
                              html_elements(mainUI, paste0("#", lastDetails)) %>% 
                                formatDetails()))
  names(out$elements)[length(out$elements)] <- lastDetails
  
  out <- prettify(toJSON(out), indent=2)
  if (!dir.exists(current_dir)) dir.create(current_dir)
  write_utf8(out, paste0(current_dir, "/", name, ".json"))
  fileCounter <<- fileCounter + 1
}
