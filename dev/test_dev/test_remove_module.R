## Sources
# https://www.r-bloggers.com/shiny-add-removing-modules-dynamically/
# https://shiny.rstudio.com/articles/modules.html#migrating-from-callmodule-to-moduleserver
# https://appsilon.com/how-to-safely-remove-a-dynamic-shiny-module/

unsuspendAll <- function(session = getDefaultReactiveDomain()) {
  observe({
    pattern <- "^output_(.*?)_hidden$"
    
    ids <- names(session$clientData) %>%
      grep(pattern, ., value = TRUE) %>%
      sub(pattern, "\\1", .)
    for (id in ids) {
      print(id)
      outputOptions(session$output, id, suspendWhenHidden = FALSE)
    }
  })
} 




library(shiny)
data(mtcars)
cols <- sort(unique(names(mtcars)[names(mtcars) != 'mpg']))


##############
remove_shiny_inputs <- function(id, .input) {
  
  invisible(
    lapply(grep(id, names(.input), value = TRUE), function(i) {
      .subset2(.input, "impl")$.values$remove(i)
    })
  )
}


remove_module_observers <- function(session, id){
  cnames <- names(session$userData)
  ind <- grep(id, names(session$userData))
  session$userData[[cnames[ind]]]$destroy()
} 

mod_test_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("lmModel"))
}


mod_test_server <- function(id, param, val){
  moduleServer(
    id,
    function(input, output, session) {
      
      
      session$userData[[paste0('module_',id,'_observer_1')]] <- observeEvent(val(), {
        print(paste0('In module ', id(), ', new value = ', val()))
      })
      
      
      output[['lmModel']] <- renderUI({
        ns <- session$ns
        tags$div(id = environment(ns)[['namespace']],
                 tagList(
                   wellPanel(
                     tags$div( style="display:inline-block;",
                               tags$p(paste0('Module ', param(), ' created')),
                               actionButton(ns('deleteButton'),  '', icon = shiny::icon('times'))
                     )
                   )
                 )
        )
      })
    }
  )
}
  
  
 #-----------------------------------


ui <- fluidPage(
  actionButton('addButton', '', icon = icon('plus')),
  selectInput('test', '', choices=1:15, width='70px')
)


server <- function(input, output, session) {
  
  rv <- reactiveValues(
    mod = NULL)
  
  observeEvent(input$addButton, {
    id <- paste0('mod_', input$addButton)

    insertUI(
      selector = '#addButton',
      where = "beforeBegin",
      ui = mod_test_ui(id)
    )
    
    mod_test_server(id, param=reactive(id), val = reactive(input$test))
    
    observeEvent(input[[paste0(id, '-deleteButton')]], {
      removeUI(selector = sprintf('#%s', id))
      remove_shiny_inputs(id, input)
      remove_module_observers(session, id)
    })
  })
}
shinyApp(ui = ui, server = server)