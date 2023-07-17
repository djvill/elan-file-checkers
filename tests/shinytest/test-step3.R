##Test the files for step 3
source("utils.R")
tryCatch(app <- ShinyDriver$new("../../"),
         error = function(e) {
           if (grepl("Cannot find shiny port number", e$message)) {
             stop("Try again or kill the PhantomJS process.\nCannot find shiny port number.")
           }
         })
testPath <- paste0("test-step3", "-current/")
testFiles <- dir("step3", ".+eaf", full.names=TRUE)
fileCounter <<- 0

##For step 3 *only*, include eaflist (for debugging overlap fixing)
incl_eaflist <- TRUE

##All files
app$uploadFile(files=testFiles)
snap(app, "All", testPath, incl_eaflist=incl_eaflist)
app$takeScreenshot(paste0(testPath, "All.png"))

##Indiv files
for (f in testFiles) {
  app$uploadFile(files=f)
  sn <- snap(app, basename(f), testPath, incl_eaflist=incl_eaflist)
  ##Download output file only if successful; otherwise it throws the error
  ##  Error in shinytest:::httr_get(url) : Shiny app is no longer running
  if (sn$success) {
    snapDownload(app, basename(f), testPath)
  }
}

##Stop app
# app$stop()
