# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Hello Shiny!"),
  
    # Show a plot of the generated distribution
    mainPanel(
      
      tabsetPanel(type = "tabs",
        tabPanel("Plot1", plotOutput("distPlot",width = "100%")),
        tabPanel("Plot2", plotOutput("distPlot2",width="100%"))
      ),style='width: 1000px; height: 1000px' # end of tabsetPanel
      )
    )
  )
