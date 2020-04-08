# rference: https://shiny.rstudio.com/gallery/


#-----------------------ui.R----------------------------------------

require(shiny)
require(ggplot2)
require(leaflet)

#-----------------------body----------------------------------------

shinyUI(fluidPage( 
#big header
headerPanel(" bleaching of corals in AUS"),
#leafletmap after that
leafletOutput("worldmap"),
#sidebar
sidebarLayout(
sidebarPanel(
#list-input data validation for corals
selectInput("type_coral", " choose coral:", 
c("blue corals", "hard corals","sea fans","sea pens","soft corals")
),
#list-input data validation for smoothers
selectInput("type_smoother", " choose smoother:", 
c(
"polynomial linear smooth"= "polyls",
"loess" ="loess"))
),
#plot with header 
mainPanel(
h2(textOutput("bleaching Values")),
plotOutput("plot_of_bleaching")
)
)
))

#-----------------------end----------------------------------------
