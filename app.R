# ui.R
library(shiny)

# Define UI for miles per gallon application
ui <- shinyUI(fluidPage( 
  
  # Application title
  headerPanel("Miles Per Gallon"),
  
  # Sidebar with controls to select the variable to plot against
  # mpg and to specify whether outliers should be included
  sidebarLayout(
    sidebarPanel(
      selectInput("variable", "Variable:", 
                  c("Cylinders" = "cyl", 
                    "Transmission" = "am",
                    "Gears" = "gear")
      ),
      
      checkboxInput("outliers", "Show outliers", FALSE)
    ),
    
    # Show the caption and plot of the requested variable against mpg
    mainPanel(
      h3(textOutput("caption")),
      plotOutput("mpgPlot")
    )
  )
))

# server.R
library(shiny)
library(datasets)

mpgData <- mtcars
mpgData$am <- factor(mpgData$am, labels = c("Automatic", "Manual"))
# Define server logic required to plot various variables against mpg
server <- shinyServer(function(input, output) {
  # Compute the forumla text in a reactive expression since it is 
  # shared by the output$caption and output$mpgPlot expressions
  formulaText <- reactive({
    paste("mpg ~", input$variable)
  })
  # Return the formula text for printing as a caption
  output$caption <- renderText({formulaText()})
  
  # Generate a plot of the requested variable against mpg and only 
  # include outliers if requested
  output$mpgPlot <- renderPlot({
    boxplot(as.formula(formulaText()), 
            data = mpgData,
            outline = input$outliers)
  })
})

shinyApp(ui = ui,server = server)

