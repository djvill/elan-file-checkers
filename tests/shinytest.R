thisDir <- basename(getwd())
if (thisDir != "tests") {
  stop("Working directory must be tests/, not ", thisDir, "/")
}

library(shinytest)
##All steps
testApp("../", testnames=dir("shinytest/", "^test-step.+R$"))
##Single step
# testApp("../", testnames=dir("shinytest/", "test-step1.R$"))
