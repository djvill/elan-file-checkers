library(shinytest)

app <- ShinyDriver$new("../../")
testPath <- "test-step1"

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

snap <- function(app, name, path=testPath) {
  library(rvest)
  library(purrr)
  library(jsonlite)
  
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
  # stepDetails <- html_elements(mainUI, ".details") %>% 
  #   {data.frame(id = html_attr(., "id"),
  #               content = map_chr(., is.character))}
  
  out <- list(export = list(fileDF = testVals$fileDF,
                            # eaflist = testVals$eaflist,
                            tierInfo = testVals$tierInfo),
              elements = list(stepHeads = stepHeads,
                              stepSubheads = stepSubheads))
  out <- prettify(toJSON(out), indent=2)
  write_json(out, paste0(testPath, "/", name, ".json"))
}


testFiles <- dir("step1", ".+eaf", full.names=TRUE)

##All files
app$uploadFile(files=testFiles)
snap(app, "All")


##Indiv files
for (f in testFiles) {
  app$uploadFile(files=f)
  snap(app, basename(f))
}

##Stop app
# app$stop()
