library(shiny)
library(R6)
library(tibble)
library(Magellan)
library(DT)
library(rhandsontable)
options(shiny.fullstacktrace = T)

#------------------------ Class TimelineDraw --------------------------------------
source(file.path('.', 'class_process_convert.R'), local=TRUE)$value
source(file.path('../../../R', 'mod_popover_for_help.R'), local=TRUE)$value
source(file.path('../../../R', 'mod_format_DT.R'), local=TRUE)$value
source(file.path('../../../R', 'global.R'), local=TRUE)$value
source(file.path('../../../R', 'mod_build_design_example.R'), local=TRUE)$value
source(file.path('.', 'mod_convert.R'), local=TRUE)$value

ui = fluidPage(
  mod_convert_ui('convert')
)


server = function(input, output){
  
  mod_convert_server('convert')

}
shiny::shinyApp(ui, server)