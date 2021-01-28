library(shiny)
library(MultiAssayExperiment)
library(tibble)
library(R6)
library(rhandsontable)

options(shiny.fullstacktrace = TRUE)


source(file.path('../../../R', 'mod_infos_dataset.R'), local=TRUE)$value
source(file.path('../../../R', 'mod_format_DT.R'), local=TRUE)$value
source(file.path('../../../R', 'global.R'), local=TRUE)$value
source(file.path('../../../R', 'mod_open_demoDataset.R'), local=TRUE)$value
source(file.path('../../../R', 'mod_choose_pipeline.R'), local=TRUE)$value
source(file.path('../../../R', 'mod_dataManager.R'), local=TRUE)$value
source(file.path('../../../R', 'class_process_convert.R'), local=TRUE)$value
source(file.path('../../../R', 'mod_build_design_example.R'), local=TRUE)$value
actionBtnClass <- "btn-primary"
btn_success_color <- "btn-success"


ui <- fluidPage(
  tagList(
    mod_dataManager_ui('dm'),
    uiOutput('show_pipeline')
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  
  
  rv.core <- reactiveValues(
    dm = NULL,
    dataIn = NULL,
    pipeline = NULL
  )
  
  rv.core$dm <- mod_dataManager_server('dm')
  
  # the necessary variables to launch a pipeline
  observeEvent(rv.core$dm(), {
    print(rv.core$dm()$dataset)
    print(rv.core$dm()$pipeline)
    
    obj <- base::get(rv.core$dm()$pipeline)
    rv.core$pipeline <- do.call(obj$new, list('App'))
    rv.core$pipeline$server(dataIn = reactive({rv.core$dm()$dataset}))
  })
  
  output$show_pipeline <- renderUI({
    req(rv.core$pipeline)
    rv.core$pipeline$ui()
  })
  
}


shinyApp(ui, server)
