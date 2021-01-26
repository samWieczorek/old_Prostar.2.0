library(shiny)
library(R6)
library(tibble)
library(MSPipelines)
options(shiny.fullstacktrace = T)

#------------------------ Class TimelineDraw --------------------------------------
source(file.path('../../R', 'class_TimelineDraw.R'), local=TRUE)$value
source(file.path('../../R', 'global.R'), local=TRUE)$value
source(file.path('../../R', 'class_ScreenManager.R'), local=TRUE)$value
source(file.path('../../R', 'class_Process.R'), local=TRUE)$value
source(file.path("../../R", "mod_popover_for_help.R"), local = TRUE)$value
source(file.path("../../R", "mod_format_DT.R"), local = TRUE)$value
source(file.path("../../R", "mod_bsmodal.R"), local=TRUE)$value

source(file.path('.', 'Example_ProcessA.R'), local=TRUE)$value
source(file.path('.', 'Example_ProcessB.R'), local=TRUE)$value
source(file.path('.', 'Example_Description.R'), local=TRUE)$value



rv <- reactiveValues()
proc <- Example_ProcessA$new('App', orientation='h')

ui = fluidPage(
  tagList(
     proc$ui()
  )
)




server = function(input, output){
  
  proc$server(dataIn = reactive({NULL}))
  
  utils::data(Exp1_R25_prot, package='DAPARdata2')

  
}
shiny::shinyApp(ui, server)