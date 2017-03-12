library(shiny)

shinyUI(fluidPage(
  fluidRow(align='center',
           h3('Califonia National Parks')
  ),
  fluidRow(
    column(3, offset=1,
           selectInput('color', label='Color', 
                       choices=list('green', 'red', 'blue'), 
                       selected='green')),
    column(4,
           sliderInput('res', label='Zoom', min=6, max=9, value=6))
  ),
  fluidRow(align='center',
           plotOutput('distPlot', height='640px', dblclick='plot_dblclick')
  )
))
