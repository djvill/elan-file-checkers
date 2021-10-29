##Run Shiny app as a background job
##To use, set the working directory to the app location, and source this file
##From https://github.com/sol-eng/background-jobs/tree/master/shiny-job
options(shiny.autoreload = TRUE)
shiny::runApp()
