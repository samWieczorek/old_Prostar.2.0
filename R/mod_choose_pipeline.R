# Module UI
  
#' @title   mod_choose_pipeline_ui and mod_choose_pipeline_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#' @param dataType xxx
#' @param package xxx
#'
#' @rdname mod_choose_pipeline
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
#' @import sos
#' 
#' 
mod_choose_pipeline_ui <- function(id){
  ns <- NS(id)
  tagList(
    h3('mod_choose_pipeline'),
    div(
      style="display:inline-block; vertical-align: middle; padding-right: 20px;",
      selectInput(ns("dataType"), 'Data type', 
                  choices = c('None'='None', 
                              'protein'='protein', 
                              'peptide'='peptide'), 
                  width='150px')
    ),
    div(
      style="display:inline-block; vertical-align: middle; padding-right: 20px;",
      uiOutput(ns("selectWidgetPipeline"))
    ),
      uiOutput(ns('describePipeline'))
  )
}
    
# Module Server
    
#' @rdname mod_choose_pipeline
#' @export
#' @keywords internal
#' 
#' 
    
mod_choose_pipeline_server <- function(id, package = NULL){
  
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    output$selectWidgetPipeline <- renderUI({
      #browser()
      req(input$dataType != 'None')
      #if (dataType() == 'None') return(NULL)
      #print('inside selectWidgetPipeline')
      #print(paste0('dataType received = ', dataType()))
      library(package, character.only = TRUE)
      selectInput(ns("pipelineChoice"),
                     "Choose the pipeline",
                     choices = c('None',names(Pipelines()[grep(input$dataType, Pipelines())])), 
                     width='150px'
      )
    })
    
    output$describePipeline <- renderUI({
      print('inside selectWidgetPipeline')
      req(input$pipelineChoice)
      includeMarkdown(system.file('md', paste0(input$pipelineChoice, '.md'), package=package))
    })
    
    return( reactive({input$pipelineChoice}))
    
    
  })
  
}
    
## To be copied in the UI
# mod_choose_pipeline_ui("choose_pipeline_ui_1")
    
## To be copied in the server
# callModule(mod_choose_pipeline_server, "choose_pipeline_ui_1")
 
