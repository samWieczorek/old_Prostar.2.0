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
#' @rdname mod_convert
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
#' @import sos
#' 
#' 
mod_convert_ui <- function(id){
  ns <- NS(id)
  tagList(
    #uiOutput(ns('show'))
    actionButton(ns('send'), 'Send dataset'),
    process$ui()
  )
}

# Module Server

#' @rdname mod_convert
#' @export
#' @keywords internal
#' 
#' 


process <- Convert$new("App")

mod_convert_server <- function(id){
  
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    rv <- reactiveValues(
      dataIn = NA
      )
    
    #observe({
      process$server(dataIn = reactive({NA}))
   # })

  })
  
}

## To be copied in the UI
# mod_choose_pipeline_ui("choose_pipeline_ui_1")

## To be copied in the server
# callModule(mod_choose_pipeline_server, "choose_pipeline_ui_1")

