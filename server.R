# Solicitamos las librerias que necesitaremos
library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(patchwork)
library(DT)
library(mathjaxr)
library(plotly)

#Data
dataRandF <- data.frame(
  Months = c(0, 1, 3, 6, 9, 12),
  Probability = c(0, 0.004, 0.024, 0.1067, 0.2547, 0.4236, 1, 0.9960, 0.9760, 0.8933, 0.7453, 0.5764),
  Indicator = c("Non-Reliability", "Non-Reliability", "Non-Reliability", "Non-Reliability", "Non-Reliability", "Non-Reliability",
                "Reliability", "Reliability", "Reliability", "Reliability", "Reliability", "Reliability")
)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$FRawData <- DT::renderDataTable(
    DT::datatable({
      FailureRate
    },
    
    filter = "top",
    selection = 'multiple',
    style = 'bootstrap',
    class = 'cell-border stripe',
    rownames = FALSE,
    colnames = c("Component","Operational State","Failure Rate [failure/hour]","Reference")
    ))
  
  output$RRawData <- DT::renderDataTable(
    DT::datatable({
      RepairRate
    },
    
    filter = "top",
    selection = 'multiple',
    style = 'bootstrap',
    class = 'cell-border stripe',
    rownames = FALSE,
    colnames = c("Component","Operational State","Min Repair Time [hour]","Max Repair Time [hour]","Reference")
    ))
  
  output$Prueba <- renderPlotly({
    p <- ggplot(
      dataRandF,
      aes(x = Months, y = Probability, group = Indicator, color = Indicator)
    ) +
      geom_line(size=0.5) +
      scale_color_manual(values = c("#353436", "#1b98e0")) +
      labs(title = "Reliability down, Non-Reliability up",x = "Months", y = "Probability") +
      labs(caption = "(based on FT quantification)") +
      theme(legend.position = "top")
    ggplotly(p)
    
  })
  
  output$ImportanceM <- DT::renderDataTable(
    DT::datatable({
      ImportanceMeasures
    },
    
    filter = "top",
    selection = 'multiple',
    style = 'bootstrap',
    class = 'cell-border stripe',
    rownames = FALSE,
    colnames = c("Component","Birnbaum","Criticality","RRW", "RAW", "FV")
    ))
  
  output$AvailabilityTime <- DT::renderDataTable(
    DT::datatable({
      AvailabilityT
    },
    
    filter = "top",
    selection = 'multiple',
    style = 'bootstrap',
    class = 'cell-border stripe',
    rownames = FALSE,
    colnames = c("Time [months]","Number of Simulations","Uptime [hours]","Downtime [hours]", "Availability  [%]")
    ))

   

})
