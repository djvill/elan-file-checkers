thisDir <- basename(getwd())
if (thisDir != "tests") {
  stop("Working directory must be tests/, not ", thisDir, "/")
}

library(shinytest)
##All steps
testApp("../", testnames=dir("shinytest/", "test.+R$"))
##Single step
# testApp("../", testnames=dir("shinytest/", "test-step2.R$"))
