library(shiny)
library(R6)
library(tibble)
library(MSPipelines)
library(Magellan)
options(shiny.fullstacktrace = T)


utils::data(Exp1_R25_prot, package='DAPARdata2')

pipe <- Magellan$new('App', name='Protein_Normalization')

ui = fluidPage(
  tagList(
    pipe$ui()
  )
)


server = function(input, output){
  # Get a QFeatures dataset for example
  rv <- reactiveValues(
    dataIn = NULL,
    res = NULL
  )
  #pipe$Launch_Pipeline('toto')
  rv$res <- pipe$server(dataIn = reactive({Exp1_R25_prot}))
  
  # observeEvent(req(rv$res()), {
  #   print(rv$res()$trigger)
  # })
}
shiny::shinyApp(ui, server)