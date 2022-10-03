thisDir <- basename(getwd())
if (thisDir != "tests") {
  stop("Working directory must be tests/, not ", thisDir, "/")
}

library(shinytest)
# testApp("../", testnames=dir("shinytest/", "test.+R$"))
testApp("../", testnames=dir("shinytest/", "test-step3.R"))
