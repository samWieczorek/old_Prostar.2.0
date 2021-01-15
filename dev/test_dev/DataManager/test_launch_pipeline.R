library(shiny)
library(MultiAssayExperiment)
library(tibble)
library(Magellan)
library(MSPipelines)

options(shiny.fullstacktrace = TRUE)


source(file.path('../../../R', 'mod_choose_pipeline.R'), local=TRUE)$value
source(file.path('../../../R', 'mod_infos_dataset.R'), local=TRUE)$value
source(file.path('../../../R', 'mod_format_DT.R'), local=TRUE)$value
source(file.path('../../../R', 'global.R'), local=TRUE)$value
source(file.path('../../../R', 'mod_open_demoDataset.R'), local=TRUE)$value

actionBtnClass <- "btn-primary"

#pipeline <- Protein_Normalization$new('Protein', verbose = TRUE)
ui <- fluidPage(
  tagList(
    mod_open_demoDataset_ui('rl'),
    uiOutput('show_pipeline')
    #pipeline$ui()
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  
  rv <- reactiveValues(
    demoData = NULL,
    pipeline = NULL,
    res = NULL
  )
  
  rv$demoData <- mod_open_demoDataset_server("rl")
  utils::data(Exp1_R25_prot, package='DAPARdata2')
  
  observeEvent(req(rv$demoData()$dataset), {
    #browser()
    rv$pipeline <- Protein_Normalization$new('App')
    #rv$res <- rv$pipeline$server(dataIn = reactive({rv$demoData()$dataset}))
    rv$res <- rv$pipeline$server(dataIn = reactive({Exp1_R25_prot}))
  })
  
  
  output$show_pipeline <- renderUI({
    req(rv$pipeline)
    rv$pipeline$ui()
  })
  
  observeEvent(rv$demoData(),{
    print('demo dataset loaded')
    print(rv$demoData()$dataset)
    print(rv$demoData()$pipeline.name)
  })
  
}


shinyApp(ui, server)
