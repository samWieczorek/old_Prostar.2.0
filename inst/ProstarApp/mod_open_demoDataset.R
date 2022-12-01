# Module UI

#' @title   mod_open_demo_dataset_ui and mod_open_demo_dataset_server
#' 
#' @description  A shiny Module.
#' 
#' @name mod_open_demo_dataset
#'
#' @keywords internal
#' 
#' @examples 
#' if (interactive()){
#' ui <- fluidPage(
#' tagList(
#'   mod_open_demoDataset_ui("demo"),
#'   textOutput('res')
#' )
#' )
#' 
#' server <- function(input, output, session) {
#'   rv <- reactiveValues(
#'     obj = NULL
#'   )
#'   rv$obj <- mod_open_demoDataset_server("demo")
#'   
#'   output$res <- renderText({
#'     rv$obj()
#'     paste0('Names of the datasets: ', names(rv$obj()))
#'   })
  #' }
#' 
#' shinyApp(ui = ui, server = server)
#' }
#' 
NULL



#' @param id xxx
#' @export 
#' @rdname mod_open_demo_dataset
#' @importFrom shiny NS tagList 
#' @import shinyjs
#' 
mod_open_demoDataset_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    div(
      div(
        style="display:inline-block; vertical-align: middle; padding-right: 20px;",
        div(id='div_demoDataset',
                            tagList(
                              uiOutput(ns("chooseDemoDataset")),
                              uiOutput(ns("linktoDemoPdf"))
                            )
        )
        ),
      div(
        style="display:inline-block; vertical-align: middle; padding-right: 20px;",
        shinyjs::disabled(
          actionButton(
            ns('load_dataset_btn'), 
            'Load dataset', 
            class=actionBtnClass)
          )
      )
      )
  )
}

# Module Server

#' @rdname mod_open_demo_dataset
#' 
#' @export
#' 
#' @param id xxx
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
mod_open_demoDataset_server <- function(id){
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    .package <- 'DaparToolshedData'
    
    rv.openDemo <- reactiveValues(
      dataRead = NULL,
      dataOut = NULL
    )

    
    ### function for demo mode
    output$chooseDemoDataset <- renderUI({
      
      if (!requireNamespace(.package, quietly = TRUE)) {
        stop("Please install ", .package, ": 
            BiocManager::install('", .package, "')")
      }
      
      
      selectInput(ns("demoDataset"),
                  "Demo dataset",
                  choices = c('None', utils::data(package=.package)$results[,"Item"]),
                  selected = character(0),
                  width='200px')
    })
    
    
    
    observeEvent(req(input$demoDataset != 'None'), {
      nSteps <- 1
      withProgress(message = '',detail = '', value = 0, {
        incProgress(1/nSteps, detail = 'Loading dataset')
        utils::data(list=input$demoDataset, package=.package)
        rv.openDemo$dataRead <- BiocGenerics::get(input$demoDataset)
        if (class(rv.openDemo$dataRead)!="QFeatures") {
          shinyjs::info("Warning : this file is not a QFeatures file ! 
                      Please choose another one.")
          return(NULL)
        }
        shinyjs::toggleState('load_dataset_btn', condition = !is.null(rv.openDemo$dataRead))
      }) # End withProgress
    }) # End observeEvent

    
    observeEvent(input$load_dataset_btn, {
      rv.openDemo$dataOut <- rv.openDemo$dataRead
    })
    
    output$linktoDemoPdf <- renderUI({
      req(input$demoDataset)
      
      # file<- paste(system.file(package = "DAPARdata2"),"/doc/",
      #              input$demoDataset,".pdf", sep="")
      # cmd <- paste("cp ",file," www", sep="")
      # system(cmd)
      # filename <-paste0(input$demoDataset,".pdf", sep="")
      # p("Dataset documentation ",a(href=filename, target='_blank', "(pdf)"))
      # 
    })

    reactive({rv.openDemo$dataOut })
  })
  
}

## To be copied in the UI
# mod_open_demo_dataset_ui("open_demo_dataset_ui_1")

## To be copied in the server
# callModule(mod_open_demo_dataset_server, "open_demo_dataset_ui_1")