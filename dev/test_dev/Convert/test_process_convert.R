library(shiny)
library(R6)
library(tibble)
library(Magellan)
library(DT)
options(shiny.fullstacktrace = T)

#------------------------ Class TimelineDraw --------------------------------------
source(file.path('.', 'class_process_convert.R'), local=TRUE)$value
source(file.path('../../../R', 'mod_popover_for_help.R'), local=TRUE)$value
source(file.path('../../../R', 'mod_format_DT.R'), local=TRUE)$value
source(file.path('../../../R', 'global.R'), local=TRUE)$value



#Pipeline <- Pipeline$new('App')
#conv <- Convert$new('App')

ui = fluidPage(
  tagList(
    shinyjs::useShinyjs(),
    actionButton('send', 'Send dataset'),
    #shinyjs::disabled(Pipeline$ui())
    uiOutput('show_pipe')
  )
)


server = function(input, output){
  utils::data(Exp1_R25_prot, package='DAPARdata2')
  
  rv <-reactiveValues(
    convert = NULL
  )
  #conv$server(dataIn = reactive({rv$dataIn}))
  
  rv$convert <- Convert$new('App2')
  
  observe({
    rv$convert$server(dataIn = reactive({rv$dataIn}))
  })

  
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