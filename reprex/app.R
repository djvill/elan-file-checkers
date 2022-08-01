shinyApp(
  ui = basicPage(
    actionButton("inc", "Increment x")
  ), 
  server=function(input, output, session) {
    vals <- reactiveValues(x = 1)
    y <- eventReactive(input$inc, { vals$x + 1 })
    
    observeEvent(input$inc, {
      vals$x <<- vals$x + 1
    })
    
    exportTestValues(
      x = vals$x,
      y = y()
    )
  }
)
