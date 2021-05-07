library(shiny)
library(shinyjs)
# library(tibble)
library(Magellan)
#library(DT)
#library(rhandsontable)
options(shiny.fullstacktrace = T)

#------------------------ Class TimelineDraw --------------------------------------
#source(file.path('../../../R', 'mod_popover_for_help.R'), local=TRUE)$value
#source(file.path('../../../R', 'mod_format_DT.R'), local=TRUE)$value
#source(file.path('../../../R', 'global.R'), local=TRUE)$value
#source(file.path('../../../R', 'mod_build_design_example.R'), local=TRUE)$value
source(file.path('.', 'mod_convert.R'), local=TRUE)$value




mod_test_process_ui <- function(id){
  ns <- NS(id)
  uiOutput(ns('UI'))
}


mod_test_process_server <- function(id){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    rv <- reactiveValues(
      dataIn = NULL,
      dataOut = NULL,
      remoteReset = FALSE,
      remoteSkipped = FALSE,
      remoteEnabled = TRUE
    )
    
    
    observe({
      rv$dataOut <- mod_nav_process_server(id = 'Convert',
                                           dataIn = reactive({rv$dataIn}),
                                           is.enabled = reactive({rv$remoteEnabled}),
                                           remoteReset = reactive({rv$remoteReset}),
                                           is.skipped = reactive({rv$remoteSkipped})
      )
      
      observeEvent(rv$dataOut$dataOut()$trigger, {
        print('totototo')
        print(names(rv$dataOut$dataOut()$value))
      })
      
    }, priority=1000)
    
    
    output$UI <- renderUI({ mod_nav_process_ui(ns('Convert')) })
    
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