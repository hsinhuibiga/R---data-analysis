# student:HsinHui Lin 28464176 Mon 18:00 tutorial 

require(ggplot2)
library(ggmap)
library(datasets)
library(leaflet)
#----Task 1----------------------------------------------------------------#
#read the csv file in to the data
data = read.csv("wblnew.csv")
#print the data
data

#---Task 2 ----------------------------------------------------------------#
#to create static visualisations  in R using ggplot2
#put the value in the more readable type and use numeric 
#data$value <- as.numeric(sub("%", "", data$value))

#consider to run the library
#myGraph <- ggplot(data, aes(variable for x axis, variable for y axis))

data.aus <- data[data$country == "Australia", ]
data.ukr <- data[data$country == "Ukraine", ]
data.zam <- data[data$country == "Zambia", ]
data.tur <- data[data$country == "Turkey", ]
data.swe <- data[data$country == "Sweden", ]


graph <-ggplot(data = data.aus, aes(date, wbl_index)) +geom_point() + facet_wrap((latitude~industry),nrow = 1) + geom_smooth(aes(group =1),method = "lm",se = FALSE)
graph <-ggplot(data = data.aus, aes(date, wbl_index)) +geom_point() + facet_wrap((latitude~industry),nrow = 1) + geom_smooth(aes(group =1),method = "lm",formula = y~ poly(x, 2),se = FALSE)
graph <-ggplot(data = data.ukr, aes(date, wbl_index)) +geom_point() + facet_wrap((latitude~industry),nrow = 1) + geom_smooth(aes(group =1),method = "lm",se = FALSE)
graph <-ggplot(data = data.ukr, aes(date, wbl_index)) +geom_point() + facet_wrap((latitude~industry),nrow = 1) + geom_smooth(aes(group =1),method = "lm",formula = y~ poly(x, 2),se = FALSE)
graph <-ggplot(data = data.zam, aes(date, wbl_index)) +geom_point() + facet_wrap((latitude~industry),nrow = 1) + geom_smooth(aes(group =1),method = "lm",se = FALSE)
graph <-ggplot(data = data.zam, aes(date, wbl_index)) +geom_point() + facet_wrap((latitude~industry),nrow = 1) + geom_smooth(aes(group =1),method = "lm",formula = y~ poly(x, 2),se = FALSE)
graph <-ggplot(data = data.tur, aes(date, wbl_index)) +geom_point() + facet_wrap((latitude~industry),nrow = 1) + geom_smooth(aes(group =1),method = "lm",se = FALSE)
graph <-ggplot(data = data.tur, aes(date, wbl_index)) +geom_point() + facet_wrap((latitude~industry),nrow = 1) + geom_smooth(aes(group =1),method = "lm",formula = y~ poly(x, 2),se = FALSE)
graph <-ggplot(data = data.swe, aes(date, wbl_index)) +geom_point() + facet_wrap((latitude~industry),nrow = 1) + geom_smooth(aes(group =1),method = "lm",se = FALSE)
graph <-ggplot(data = data.swe, aes(date, wbl_index)) +geom_point() + facet_wrap((latitude~industry),nrow = 1) + geom_smooth(aes(group =1),method = "lm",formula = y~ poly(x, 2),se = FALSE)
graph
#---Task 3 ------------------------------------------------------------------------#
#to fit curves to the data to identify trends
#use scatter plot with matrix to represent the trends

ggplot(data = data,aes(x=date, y=wbl_index, color=country)) +geom_point() + 
  labs(x= 'date' ,y = 'wbl_index', title = 'WBL of the Countries')+
  geom_smooth(aes(group =1),method = "lm",se = FALSE) + facet_wrap(~industry,nrow = 1)

#----Task 4-----------------------------------------------------------------------#
#to create a data map in R with Leaflet 


map <- leaflet(data=data) %>% 
  addTiles() %>% #addCircles(lng = ~lon, lat = ~lat)
  addMarkers(lng=~longitude , lat=~latitude, popup=~industry)
map      # Prints the map



#----Task 5---------------------------------------------------------------------#
# to create an interactive visualisation in R with Shiny
#UI
ui <-shinyUI(fluidPage( 
  # Apply title as header
  headerPanel("WBL of the countries"),
  leafletOutput("worldmap"),
  # Sidebar with controls to select the variable to plot against
  # mpg and to specify whether outliers should be included
  sidebarLayout(
    sidebarPanel(
      selectInput("Country", "Choosing:", 
                  c("Sweden", "Ukrain","Turkey","Zambia","Australia")
      ),
      selectInput("Smoother_Type", " Choosing:", 
                  c(
                    "gdp"= "gdp",
                    "wbl_index" ="wbl_index"))
    ),
    
    mainPanel(
      h2(textOutput("Value")),
      plotOutput("bogbr")
    )
  )
))

#server
names(data) = c("industry", "country","longitude","latitude","date","wbl_index")

server <-shinyServer(function(input, output) {
  
  output$worldmap = renderLeaflet({leaflet(data = data) %>% addTiles() %>%addMarkers(~longitude, ~latitude, popup = ~as.character(industry))})
  output$bogbr = renderPlot({
    a=data[data$country==input$Country,]
    b=ggplot(data = a, aes(x =date, y= wbl_index)) +geom_point()+facet_wrap((industry~latitude),nrow = 1)
    b=b+scale_x_continuous("date")+scale_y_continuous("wbl_index")
    
    if (input$Smoother_Type =="gdp"){
      picture=b+geom_smooth(aes(group =1),method = "lm",formula = y~ poly(x, 2),se = FALSE)
    }
    else{picture=b+geom_smooth(aes(group= 1),method = "lm",se  =FALSE)}
    print(picture)
  })
})

shinyApp(ui = ui,server = server)
