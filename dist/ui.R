library(shiny)

# Define UI for miles per gallon application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Beta Distribution"),
  
  sidebarPanel(
    sliderInput("alpha",
                "Alpha",
                min = 1,
                max = 5,
                value = 2
    )
  ),
  
  mainPanel(
    plotOutput('distribution')
  )
))