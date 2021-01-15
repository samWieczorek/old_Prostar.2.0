library(shiny)
library(MultiAssayExperiment)
library(tibble)


options(shiny.fullstacktrace = TRUE)


source(file.path('../../../R', 'mod_choose_pipeline.R'), local=TRUE)$value
source(file.path('../../../R', 'mod_infos_dataset.R'), local=TRUE)$value
source(file.path('../../../R', 'mod_format_DT.R'), local=TRUE)$value
source(file.path('../../../R', 'global.R'), local=TRUE)$value
source(file.path('../../../R', 'mod_open_demoDataset.R'), local=TRUE)$value

actionBtnClass <- "btn-primary"

ui <- fluidPage(
  tagList(
    div(
      style="display:inline-block; vertical-align: middle; padding-right: 20px;",
      mod_choose_pipeline_ui("pipe")
    ),
    
    shinyjs::hidden(div(id='div_demoDataset',
                        mod_open_demoDataset_ui('rl')
                        )
                    ),
    shinyjs::hidden(actionButton('send', 'Load dataset')),
    
    
    uiOutput('show_pipeline')
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  
  pipeline <- NULL
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
   # browser()
    shinyjs::toggle('div_demoDataset', condition = !is.null(rv$pipeline.name()) && rv$pipeline.name() != 'None')
    shinyjs::toggle('send', condition = !is.null(rv$demoData))
  })
  
  observeEvent(req(rv$pipeline.name() != 'None'), {
    
    print("Launch Magellan")
    rv$pipeline <- Protein_Normalization$new('App')
    rv$pipeline$server(dataIn = reactive({rv$dataIn}))
    #rv$dataIn <- rv$demoData()$dataset
  })
  
  observeEvent(input$send, {
    print(names(rv$demoData()))
    rv$dataIn <- rv$demoData()
    })

  
  output$show_pipeline <- renderUI({
    req(rv$pipeline)
    print('show_ui')
    rv$pipeline$ui()
  })
  
  observeEvent(rv$demoData(),{
    print('demo dataset loaded')

    print(rv$demoData()$dataset)
    print(rv$demoData()$pipeline)
  })
  
}


shinyApp(ui, server)
