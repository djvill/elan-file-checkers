##Test the files for step 2
source("utils.R")
tryCatch(app <- ShinyDriver$new("../../"),
         error = function(e) {
           if (grepl("Cannot find shiny port number", e$message)) {
             stop("Try again or kill the PhantomJS process.\nCannot find shiny port number.")
           }
         })
testPath <- paste0("test-step2", "-current/")
testFiles <- dir("step2", ".+eaf", full.names=TRUE)
fileCounter <<- 0

##All files
app$uploadFile(files=testFiles)
snap(app, "All", testPath)
app$takeScreenshot(paste0(testPath, "All.png"))

##Indiv files
for (f in testFiles) {
  app$uploadFile(files=f)
  snap(app, basename(f), testPath)
}

##Stop app
# app$stop()
