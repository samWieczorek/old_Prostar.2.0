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
    mod_launch_magellan_ui('app'),
    uiOutput('show_pipeline')
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  
  rv <- reactiveValues(
    pipe = NULL)
  
  rv$pipe <- mod_launch_magellan_server('app')
 
  
  output$show_pipeline <- renderUI({
    req(rv$pipe)
    print('show_ui')
    rv$pipe$ui()
  })
  
}


shinyApp(ui, server)
