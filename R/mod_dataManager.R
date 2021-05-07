# Module UI

#' @title   mod_open_demo_dataset_ui and mod_open_demo_dataset_server
#' 
#' @description  A shiny Module.
#'
#' @param id shiny id
#' 
#' @param input internal
#' 
#' @param output internal
#' 
#' @param session internal
#' 
#' @param pipeline.def xxx
#' 
#' @return An object of class [`xxxx`]
#' 
#' @rdname mod_open_demo_dataset
#'
#' @keywords internal
#' 
#' @export 
#' 
#' @importFrom shiny NS tagList 
#' @import shinyjs
#' 
mod_dataManager_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    radioButtons(ns("dmUI"), "Choose one:",
                 choiceNames = list(
                   h4('Import file'),
                   HTML("<p style='color:red;'>Convert file</p>"),
                   "Open demo dataset"
                 ),
                 choiceValues = list(
                   "importFile", "convertFile", "openDemo"
                 )),
    
    shinyjs::hidden(
      div(id=ns('mod_demo'),
          mod_open_demoDataset_ui(ns('rl'))
          )
      ),
    shinyjs::hidden(
      div(id=ns('mod_convert'),
          uiOutput(ns('show_convert'))
          )
    ),
    shinyjs::hidden(
      div(id=ns('modChoosePipeline'),
          mod_choose_pipeline_ui(ns('pipe'))
      )
    ),
    actionButton(ns('send'), 'Send'),
  hr(),
    mod_infos_dataset_ui(ns("infos"))
    )
}

# Module Server

#' @rdname mod_open_demo_dataset
#' 
#' @export
#' 
#' @keywords internal
#' 
#' @import DAPARdata2
#' @importFrom BiocGenerics get
#' @importFrom utils data
#' @importFrom BiocManager install
#' @importFrom shinyjs info
#' @import QFeatures
#' 
mod_dataManager_server <- function(id){
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    
    rv.dm <- reactiveValues(
      dataType = NULL,
      pipeline = NULL,
      dataIn = NULL,
      dataOut = NULL
    )
    
    mod_infos_dataset_server('infos', 
                             obj = reactive({rv.dm$dataIn})
    )
    
    rv.dm$demoData <- mod_open_demoDataset_server("rl")
    convert <- Convert$new(ns('convData'))
    rv.dm$convertData <- convert$server(dataIn = reactive({NULL}))
    
    
    rv.dm$pipeline <- mod_choose_pipeline_server('pipe',
                                                 dataType = 'protein',
                                                 package = 'MSPipelines')

    output$show_convert <- renderUI({
      req(convert)
      convert$ui()
    })
    
    
    observeEvent(rv.dm$demoData(), {rv.dm$dataIn <- rv.dm$demoData()})
    observeEvent(rv.dm$convertData()$trigger, {rv.dm$dataIn <- rv.dm$convertData()$value})
    
    observeEvent(req(rv.dm$dataIn), {
     # shinyjs::toggle('modChoosePipeline', condition = !is.null(metadata(rv.dm$dataIn)$originalTypeOfData))
      shinyjs::toggle('modChoosePipeline', condition = T)
    })
    
    observeEvent(input$send, {
      rv.dm$dataOut <- list(dataset = rv.dm$dataIn,
                                  pipeline = rv.dm$pipeline())
    })

    observeEvent(input$dmUI,{
      print(input$dmUI)
      shinyjs::toggle('mod_demo', condition = input$dmUI == 'openDemo')
      shinyjs::toggle('mod_convert', condition = input$dmUI == 'convertFile')
      shinyjs::toggle('mod_import', condition = input$dmUI == 'importFile')
    })
    
    reactive({rv.dm$dataOut })
  })
  
}

## To be copied in the UI
# mod_open_demo_dataset_ui("open_demo_dataset_ui_1")

## To be copied in the server
# callModule(mod_open_demo_dataset_server, "open_demo_dataset_ui_1")