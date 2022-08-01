app <- ShinyDriver$new("../../")
app$snapshotInit("test-step1")

testFiles <- dir("step1", ".+eaf", full.names=TRUE)

##All files
app$uploadFile(files=testFiles)
app$snapshot(list(output = "out",
                  export = c("eaflist", "tierInfo", "tierIss")))

##Indiv files
for (f in testFiles) {
  app$uploadFile(files=f)
  app$snapshot(list(output = "out",
                    export = c("eaflist", "tierInfo", "tierIss")))
}
