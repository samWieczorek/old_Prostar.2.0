#' launch_magellan UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_launch_magellan_ui <- function(id){
  ns <- NS(id)
  tagList(
    div(
      div(
        style="display:inline-block; vertical-align: middle; padding-right: 20px;",
        mod_choose_pipeline_ui(ns("pipe"))
      ),
      div(
        style="display:inline-block; vertical-align: middle; padding-right: 20px;",
        shinyjs::hidden(div(id=ns('div_demoDataset'),
                            mod_open_demoDataset_ui(ns('rl'))
        )
        )
      ),
      div(
        style="display:inline-block; vertical-align: middle; padding-right: 20px;",
        shinyjs::hidden(actionButton(ns('load_dataset_btn'), 'Load dataset', class=actionBtnClass))
      )
    )
    #uiOutput(ns('show'))
  )
}
    
#' launch_magellan Server Function
#'
#' @noRd 
mod_launch_magellan_server <- function(id){
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    rv <- reactiveValues(
      demoData = NULL,
      pipeline = NULL,
      pipeline.name = NULL,
      dataIn = NULL
    )
    
    rv$demoData <- mod_open_demoDataset_server("rl")
    
    rv$pipeline.name <- mod_choose_pipeline_server('pipe', 
                                                   package = 'MSPipelines')
    
    observe({
      shinyjs::toggle('div_demoDataset', condition = !is.null(rv$pipeline.name()) && rv$pipeline.name() != 'None')
      shinyjs::toggle('load_dataset_btn', condition = !is.null(rv$demoData()))
    })
    
    observeEvent(req(rv$pipeline.name() != 'None'), {
      print("Launch Magellan")
      obj <- base::get(rv$pipeline.name())
      rv$pipeline <- do.call(obj$new, list('App'))
      #rv$pipeline <- Protein$new('App')
      rv$dataOut <- rv$pipeline$server(dataIn = reactive({rv$dataIn}))
    })
    
    observeEvent(input$load_dataset_btn, {
      print(names(rv$demoData()))
      rv$dataIn <- rv$demoData()
    })
    
    output$show <- renderUI({
      req(rv$pipeline)
      rv$pipeline$ui()
    })
    
    list(server = reactive({rv$dataOut}),
         ui = reactive({
           req(rv$pipeline)
           rv$pipeline$ui()
           })
         )
    
  })
 
}
    
## To be copied in the UI
# mod_launch_magellan_ui("launch_magellan_ui_1")
    
## To be copied in the server
# callModule(mod_launch_magellan_server, "launch_magellan_ui_1")
 
