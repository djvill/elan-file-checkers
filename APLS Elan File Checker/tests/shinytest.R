library(shinytest)
testApp("../", testnames=dir("shinytest/", "test.+R$"))
