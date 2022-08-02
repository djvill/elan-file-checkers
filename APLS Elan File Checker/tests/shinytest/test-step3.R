##Test the files for step 3
source("utils.R")
app <- ShinyDriver$new("../../")
testPath <- paste0("test-step3", "-current/")
testFiles <- dir("step3", ".+eaf", full.names=TRUE)
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
