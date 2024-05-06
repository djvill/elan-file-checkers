##Run Shiny app as a background job
##To use, set the working directory to the app location, and source this file *as a background job*
##From https://github.com/sol-eng/background-jobs/tree/main/shiny-job
options(shiny.autoreload = TRUE)
shiny::runApp(launch.browser=TRUE)
