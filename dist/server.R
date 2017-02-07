library(shiny)

# Define server logic required to plot various variables against mpg
shinyServer(function(input, output) {
  output$distribution <- renderPlot(
    {
      sup <- seq(0, 1, by=0.01)
      plot(sup, dbeta(sup, 2, input$alpha), type = "l", col = "darkgrey", lwd = 2,
           xlab = "x", ylab = "Density")
    }
  )
})