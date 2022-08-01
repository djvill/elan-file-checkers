app <- ShinyDriver$new("../../")
app$snapshotInit("clickfirst")

app$setInputs(inc = "click")
app$snapshot()
