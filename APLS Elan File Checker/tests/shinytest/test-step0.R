##Test the files for step 0
source("utils.R")
app <- ShinyDriver$new("../../")
testPath <- "test-step0"
testFiles <- dir("step0", ".+eaf", full.names=TRUE)
fileCounter <- 0

##All files
app$uploadFile(files=testFiles)
snap(app, "All", testPath)

##Indiv files
for (f in testFiles) {
  app$uploadFile(files=f)
  snap(app, basename(f), testPath)
}

##Stop app
# app$stop()
