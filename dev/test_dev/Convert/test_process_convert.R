library(shiny)
library(shinyjs)
library(QFeatures)
library(MSPipelines)
library(Magellan)

options(shiny.fullstacktrace = T)
source(file.path('.', 'mod_convert.R'), local=FALSE)$value

verbose <- F
redBtnClass <- "btn-danger"
PrevNextBtnClass <- "btn-info"
btn_success_color <- "btn-success"
optionsBtnClass <- "info"
btn_style <- "display:inline-block; vertical-align: middle; padding: 7px"


mod_test_process_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns('UI'))
  )
}


mod_test_process_server <- function(id){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
    rv <- reactiveValues(
      dataIn = Exp1_R25_prot,
      dataOut = NULL
    )
    

    observe({
      rv$dataOut <- mod_nav_process_server(id = 'Convert')
      observeEvent(rv$dataOut$dataOut()$trigger, {
        print('totototo')
        print(names(rv$dataOut$dataOut()$value))
        #browser()
      })
      
    }, priority=1000)
    
    
    
    
    
    output$UI <- renderUI({
      mod_nav_process_ui(ns('Convert'))
    })
    
  })
}



#----------------------------------------------------------------------
ui <- fluidPage(
  mod_test_process_ui('test_mod_process')
)


#----------------------------------------------------------------------
server <- function(input, output){
  mod_test_process_server('test_mod_process')
}


shinyApp(ui, server)
