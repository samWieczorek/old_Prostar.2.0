library(shiny)
library(R6)
library(tibble)
library(Magellan)
library(MSPipelines)
library(highcharter)

options(shiny.fullstacktrace = T)

rv <- reactiveValues()

process <- Protein_Normalization$new('Norm')

ui = fluidPage(
  tagList(
    process$ui()
  )
)




server = function(session, input, output){
  # Get a QFeatures dataset for example
  
  rv <- reactiveValues(
    res = NULL
  )
  utils::data(Exp1_R25_prot, package='DAPARdata2')
  
  #rv$res <- process$server(dataIn = reactive({rv$dataIn}))
  rv$res <- process$server(dataIn = reactive({Exp1_R25_prot}))
  

  
}
shiny::shinyApp(ui, server)
