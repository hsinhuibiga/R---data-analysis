# rference: https://shiny.rstudio.com/gallery/
# rference: https://stackoverflow.com/questions/11776287/remove-pattern-from-string-with-gsub
# rference: https://ggplot2.tidyverse.org/reference/geom_smooth.html

#--------------------- server.R-----------------------------------

require(shiny)
require(ggplot2)
require(leaflet)

#--------------------------body---------------------------------------


#------read-----------------------------------------------------------#
raw_data = read.csv("assignment-02-data-formated.csv")
names(raw_data) = c("location", "coralType","longitude","latitude","year","value")
#------convert values into %-------------------------------------------#
raw_data$value = as.numeric(gsub("%", "", raw_data$value))

#-----Shiny server-------------------------------------------------#
shinyServer(function(input, output) {
#-------------------list-input data validation for corals
var11 = reactive({smoothtype <- switch(input$type_smoother)})
#--------------------------the world map
output$worldmap = renderLeaflet({leaflet(data = raw_data) %>% addTiles() %>%addMarkers(~longitude, ~latitude, popup = ~as.character(location))})
#----- main plot---------------------------------------
output$plot_of_bleaching = renderPlot({
var1=raw_data[raw_data$coralType==input$type_coral,]
var2=ggplot(data = var1, aes(x =year, y= value)) +geom_point()+facet_wrap((location~latitude),nrow = 1)
var2=var2+scale_x_continuous("year")+scale_y_continuous("bleaching in percentages")
  
if (input$type_smoother =="polyls"){plot_main=var2+geom_smooth(aes(group =1),method = "lm",formula = y~ poly(x, 2),se = FALSE)}
else{plot_main=var2+geom_smooth(aes(group= 1),method = "lm",se  =FALSE)}

print(plot_main)
      
  })
  
})


#-----------------------end----------------------------------------