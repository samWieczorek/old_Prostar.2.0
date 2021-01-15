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
      div(
      style="display:inline-block; vertical-align: middle; padding-right: 20px;",
      mod_choose_pipeline_ui("pipe")
    ),
    div(
      style="display:inline-block; vertical-align: middle; padding-right: 20px;",
      shinyjs::hidden(div(id='div_demoDataset',
                          mod_open_demoDataset_ui('rl')
      )
      )
    ),
    div(
      style="display:inline-block; vertical-align: middle; padding-right: 20px;",
      shinyjs::hidden(actionButton('load_dataset_btn', 'Load dataset', class=actionBtnClass))
    )
    ),
    
    uiOutput('show_pipeline')
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  

  rv.core <- reactiveValues(
    demoData = NULL,
    pipeline = NULL,
    pipeline.name = NULL,
    dataIn = NULL
  )
  
  rv.core$demoData <- mod_open_demoDataset_server("rl")
  
  rv.core$pipeline.name <- mod_choose_pipeline_server('pipe', 
                                        package = 'MSPipelines')

  observe({
    shinyjs::toggle('div_demoDataset', condition = !is.null(rv.core$pipeline.name()) && rv.core$pipeline.name() != 'None')
    shinyjs::toggle('load_dataset_btn', condition = !is.null(rv.core$demoData()))
  })
  
  observeEvent(req(rv.core$pipeline.name() != 'None'), {
    print("Launch Magellan")
    obj <- base::get(rv.core$pipeline.name())
    rv.core$pipeline <- do.call(obj$new, list('App'))
    #rv$pipeline <- Protein$new('App')
    rv.core$pipeline$server(dataIn = reactive({rv.core$dataIn}))
  })
  
  observeEvent(input$load_dataset_btn, {
    print(names(rv.core$demoData()))
    rv.core$dataIn <- rv.core$demoData()
    })

  
  output$show_pipeline <- renderUI({
    req(rv.core$pipeline)
    print('show_ui')
    rv.core$pipeline$ui()
  })
  
}


shinyApp(ui, server)
