library(shiny)
library(ggplot2)
library(ggmap)

shinyServer(function(input, output) {
  
  load('/Users/linggeli/Downloads/CAPD/parks.Rda')
  
  location <- reactiveValues(x=-120, y=37.5)
  
  observeEvent(input$plot_dblclick, {
    current <- input$plot_dblclick
    location$x <- current$x
    location$y <- current$y
  })
   
  output$distPlot <- renderPlot({
    cali <- get_googlemap(center=c(lon=location$x, lat=location$y), maptype='hybrid', zoom=input$res)
    ggmap(cali) +
      geom_polygon(data=parks, aes(x=long, y=lat, group=group), colour='NA', fill=input$color, alpha=0.8)
  })
  
})
