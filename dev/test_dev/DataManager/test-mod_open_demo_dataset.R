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
    mod_open_demoDataset_ui('rl'),
    
    div(
      style="display:inline-block; vertical-align: middle; padding-right: 20px;",
      mod_choose_pipeline_ui("pipe")
    ),
    uiOutput('show_pipeline')
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  
  
  rv <- reactiveValues(
    demoData = NULL,
    pipeline = NULL
  )
  
  rv$demoData <- mod_open_demoDataset_server("rl")
  
  rv$pipe <- mod_choose_pipeline_server('pipe', 
                                                 dataType = reactive({input$dataType}), 
                                                 package = 'MSPipelines')

  
  output$show_pipeline <- renderUI({
    req(pipeline)
    pipeline$ui()
  })
  
  observeEvent(rv$demoData(),{
    print('demo dataset loaded')

    print(rv$demoData()$dataset)
    print(rv$demoData()$dataType)
  })
  
}


shinyApp(ui, server)
