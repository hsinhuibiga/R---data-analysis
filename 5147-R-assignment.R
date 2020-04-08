# student:HsinHui Lin 28464176 Mon 18:00 tutorial 

require(ggplot2)
library(ggmap)
library(datasets)
library(leaflet)
#----Task 1----------------------------------------------------------------#
#read the csv file in to the data
data = read.csv("assignment-02-data-formated.csv")
#print the data
data

#---Task 2 ----------------------------------------------------------------#
#to create static visualisations  in R using ggplot2
#put the value in the more readable type and use numeric 
data$value <- as.numeric(sub("%", "", data$value))

#consider to run the library
#myGraph <- ggplot(data, aes(variable for x axis, variable for y axis))

data.soft <- data[data$coralType == "soft corals", ]
data.hard <- data[data$coralType == "hard corals", ]
data.seaP <- data[data$coralType == "sea pens", ]
data.seaF <- data[data$coralType == "sea fans", ]
data.blue <- data[data$coralType == "blue corals", ]


graph <-ggplot(data = data.soft, aes(year, value)) +geom_point() + facet_wrap((latitude~location),nrow = 1) + geom_smooth(aes(group =1),method = "lm",se = FALSE)
graph <-ggplot(data = data.soft, aes(year, value)) +geom_point() + facet_wrap((latitude~location),nrow = 1) + geom_smooth(aes(group =1),method = "lm",formula = y~ poly(x, 2),se = FALSE)
graph <-ggplot(data = data.hard, aes(year, value)) +geom_point() + facet_wrap((latitude~location),nrow = 1) + geom_smooth(aes(group =1),method = "lm",se = FALSE)
graph <-ggplot(data = data.hard, aes(year, value)) +geom_point() + facet_wrap((latitude~location),nrow = 1) + geom_smooth(aes(group =1),method = "lm",formula = y~ poly(x, 2),se = FALSE)
graph <-ggplot(data = data.seaP, aes(year, value)) +geom_point() + facet_wrap((latitude~location),nrow = 1) + geom_smooth(aes(group =1),method = "lm",se = FALSE)
graph <-ggplot(data = data.seaP, aes(year, value)) +geom_point() + facet_wrap((latitude~location),nrow = 1) + geom_smooth(aes(group =1),method = "lm",formula = y~ poly(x, 2),se = FALSE)
graph <-ggplot(data = data.seaF, aes(year, value)) +geom_point() + facet_wrap((latitude~location),nrow = 1) + geom_smooth(aes(group =1),method = "lm",se = FALSE)
graph <-ggplot(data = data.seaF, aes(year, value)) +geom_point() + facet_wrap((latitude~location),nrow = 1) + geom_smooth(aes(group =1),method = "lm",formula = y~ poly(x, 2),se = FALSE)
graph <-ggplot(data = data.blue, aes(year, value)) +geom_point() + facet_wrap((latitude~location),nrow = 1) + geom_smooth(aes(group =1),method = "lm",se = FALSE)
graph <-ggplot(data = data.blue, aes(year, value)) +geom_point() + facet_wrap((latitude~location),nrow = 1) + geom_smooth(aes(group =1),method = "lm",formula = y~ poly(x, 2),se = FALSE)
graph
#---Task 3 ------------------------------------------------------------------------#
#to fit curves to the data to identify trends
#use scatter plot with matrix to represent the trends

ggplot(data = data,aes(x=year, y=value, color=coralType)) +geom_point() + 
labs(x= 'year' ,y = 'value', title = 'Trend for the Great Barrier Reef')+
geom_smooth(aes(group =1),method = "lm",se = FALSE) + facet_wrap(~location,nrow = 1)

#----Task 4-----------------------------------------------------------------------#
#to create a data map in R with Leaflet 


map <- leaflet(data=data) %>% 
  addTiles() %>% #addCircles(lng = ~lon, lat = ~lat)
  addMarkers(lng=~longitude , lat=~latitude, popup=~location)
map      # Prints the map



#----Task 5---------------------------------------------------------------------#
# to create an interactive visualisation in R with Shiny
#UI
ui <-shinyUI(fluidPage( 
  # Apply title as header
  headerPanel("Bleaching of the Great Barrier Reef"),
  leafletOutput("worldmap"),
  # Sidebar with controls to select the variable to plot against
  # mpg and to specify whether outliers should be included
  sidebarLayout(
    sidebarPanel(
      selectInput("Coral_Type", "Choosing Type:", 
                  c("blue corals", "hard corals","sea fans","sea pens","soft corals")
      ),
      selectInput("Smoother_Type", " Choosing:", 
                  c(
                    "polynomial linear smooth"= "polyls",
                    "loess" ="loess"))
    ),
    
    mainPanel(
      h2(textOutput("Value")),
      plotOutput("bogbr")
    )
  )
))

#server
names(data) = c("location", "coralType","longitude","latitude","year","value")

server <-shinyServer(function(input, output) {

  output$worldmap = renderLeaflet({leaflet(data = data) %>% addTiles() %>%addMarkers(~longitude, ~latitude, popup = ~as.character(location))})
  output$bogbr = renderPlot({
    a=data[data$coralType==input$Coral_Type,]
    b=ggplot(data = a, aes(x =year, y= value)) +geom_point()+facet_wrap((location~latitude),nrow = 1)
    b=b+scale_x_continuous("year")+scale_y_continuous("Value")
    
    if (input$Smoother_Type =="polyls"){
      picture=b+geom_smooth(aes(group =1),method = "lm",formula = y~ poly(x, 2),se = FALSE)
      }
    else{picture=b+geom_smooth(aes(group= 1),method = "lm",se  =FALSE)}
    print(picture)
  })
})

shinyApp(ui = ui,server = server)
