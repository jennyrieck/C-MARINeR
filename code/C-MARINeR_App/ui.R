library(shiny)

# Define UI for application that draws a histogram
shinyUI(
  
  fluidPage(
    
    
    # Application title
    titlePanel("c-MARINeR"),
    
    sidebarLayout(
      sidebarPanel(
        h2('Analysis Options'),
        selectInput('cubeIn', 'Select Connectivity Cube', 
                    choices=arrayList),
        actionButton('go', 'Run Analysis'),
        h2('Plotting Options'),
        selectInput('rowColDesign', 'Select Row/Column Design', 
                    choices=designList),
        numericInput('fp_xaxis', 'X Axis Component #', value=1,min=1,max=10,
                     step=1),
        numericInput('fp_yaxis', 'Y Axis Component #', value=2,min=1,max=10,
                     step=1),
        sliderInput('heatmap_lvs', 'Rebuilt HeatMap Components', min=1,max=10,
                    value=c(1,2)),
        checkboxInput('fp_labels', 'ROI Labels', value=F),
        actionButton('replot', 'Plot Results'),
        br(),
        plotOutput('scree1'),
        plotOutput('scree2')
        #actionbutton('run','Run Analysis')
      ),
      
      mainPanel(plotOutput('compromise_factor_map'),
                br(),
                plotOutput('compromise_heat_map')
                
      )
      
    )
  )
)

