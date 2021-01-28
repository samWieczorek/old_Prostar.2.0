library(shiny)
library(R6)
library(tibble)
library(Magellan)
library(DT)
library(rhandsontable)
options(shiny.fullstacktrace = T)

#------------------------ Class TimelineDraw --------------------------------------
source(file.path('../../../R', 'class_process_convert.R'), local=TRUE)$value
source(file.path('../../../R', 'mod_popover_for_help.R'), local=TRUE)$value
source(file.path('../../../R', 'mod_format_DT.R'), local=TRUE)$value
source(file.path('../../../R', 'global.R'), local=TRUE)$value
source(file.path('../../../R', 'mod_build_design_example.R'), local=TRUE)$value


ui = fluidPage(
  tagList(
    shinyjs::useShinyjs(),
    actionButton('send', 'Send dataset'),
    uiOutput('show_pipe')
  )
)


server = function(input, output){
  utils::data(Exp1_R25_prot, package='DAPARdata2')
  
  rv <-reactiveValues(
    dataIn = NULL,
    convert = NULL,
    result = NULL
  )
  #conv$server(dataIn = reactive({rv$dataIn}))
  
  rv$convert <- Convert$new('App2')

  observe({
    rv$result <- rv$convert$server(dataIn = reactive({NA}))
    print(names(rv$result()$value))
  },
  priority = 1000)

  #shinyjs::delay(1000, rv$dataIn <- NA)
  
  output$show_pipe <- renderUI({
    req(rv$convert)
    rv$convert$ui()
  })

  
  observeEvent(input$send,{
    if (input$send%%2 != 0)
      rv$dataIn <- NA
    else
      rv$dataIn <- NULL
  })
  
}
shiny::shinyApp(ui, server)