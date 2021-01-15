options(shiny.maxRequestSize=300*1024^2) 
options(encoding = "UTF-8")
options(shiny.fullstacktrace = T)
require(compiler)
enableJIT(3)


#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinyjs
#' 
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here
  mod_test_server('tutu')
  
  
  rv.core <- reactiveValues(
    
    pipeline = NULL,
    
    
    # Current QFeatures object in Prostar
    current.obj = NULL,
    
    # pipeline choosen by the user for its dataset
    current.pipeline = NULL,
    
    
    # objects returned by demode, openmode and convertmode
    tmp_dataManager = list(convert = NULL,
                           openFile = NULL,
                           openDemo = NULL)
    #   
    #   # return value of the settings module
    #   settings = NULL,
    #   
    #   #
    #   tempplot = NULL,
    #   
    #   #
    #   loadData = NULL
  )
  
  
  ## Get the return values of modules in charge of loading datasets
  rv.core$tmp_dataManager <- list(
    #openFile = mod_open_dataset_server('moduleOpenDataset'),
    #convert = mod_convert_ms_file_server('moduleProcess_Convert'),
    openDemo = mod_open_demoDataset_server('demo_data')
  )
  
  # observeEvent(rv.core$tmp_dataManager$openFile(),{
  #   rv.core$current.obj <- rv.core$tmp_dataManager$openFile()$dataset
  #   rv.core$current.pipeline <- rv.core$tmp_dataManager$openFile()$pipeline
  # })
  # 
  # observeEvent(rv.core$tmp_dataManager$convert(),{
  #   rv.core$current.obj <- rv.core$tmp_dataManager$convert()
  #   rv.core$current.pipeline <- rv.core$tmp_dataManager$convert()$pipeline
  #   })
  
  
  observeEvent(input$browser,{browser()})
  
  observeEvent(rv.core$tmp_dataManager$openDemo(),{
    print('demo dataset loaded')
    browser()
    rv.core$temp.current.obj <- rv.core$tmp_dataManager$openDemo()$dataset
    rv.core$current.pipeline <- rv.core$tmp_dataManager$openDemo()$pipeline
  })
  
  
  #  #Once the type of pipeline is known (ie a dataset has been loaded),
  #  #call the server parts of the processing modules that belongs
  #  # to this pipeline
  observeEvent(req(rv.core$current.pipeline), {
    #browser()
    rv.core$pipeline <- Protein_Normalization$new('Pipeline')
    rv.core$pipeline$server(dataIn = reactive({rv.core$current.obj}))
    #shinyjs::show('div_pipeline')
  })
  
  observeEvent(input$ReloadProstar, { js$reset()})
  
  
  
  
  output$contenu_dashboardBody <- renderUI({
    
    # body content
     fluidPage(
       mod_test_ui('tutu'),
       theme = shinythemes::shinytheme("cerulean"),
       tabItems(
         tabItem(tabName = "ProstarHome", class="active",
                 mod_homepage_ui('home')),
         # tabItem(tabName = "openFile", h3("Open QFeature file"),
         #         mod_import_file_from_ui("open_file")),
         # tabItem(tabName = "convert", h3("Convert data"),
         #         mod_convert_ms_file_ui("convert_data")),
         tabItem(tabName = "demoData", h3("Load a demo dataset"),
                 mod_open_demoDataset_ui("demo_data")),
         tabItem(tabName = "export", h3("Export")), # export module not yet
         tabItem(tabName = "globalSettings", h3('Global settings'),
                 mod_settings_ui('global_settings')),
         tabItem(tabName = "releaseNotes", h3('Release notes'),
                 mod_release_notes_ui('rl')),
         tabItem(tabName = "checkUpdates", h3('Check for updates'),
                 mod_check_updates_ui('check_updates')),
         tabItem(tabName = "usefulLinks",
                 mod_insert_md_ui('links_MD')),
         tabItem(tabName = "faq",
                 mod_insert_md_ui('FAQ_MD')),
         tabItem(tabName = "bugReport", h3('Bug report'),
                 mod_bug_report_ui("bug_report")),
         tabItem(tabName = "pipeline", h3('Pipeline'),
                 uiOutput('show_pipeline')
                 )
         )
     )
    
  })
  
  output$show_pipeline <- renderUI({ 
    print('toto')
    #browser()
    req(rv.core$pipeline)
    print('toto2')
    
    rv.core$pipeline$ui()
  })
  
  
  # mimics loading data > body content and inactivation of import menus in sidebar
  observeEvent(rv.core$current.pipeline, ignoreNULL=FALSE, { #https://stackoverflow.com/questions/48278111/disable-enable-click-on-dashboard-sidebar-in-shiny

    if(is.null(rv.core$current.pipeline)){
      # show sidebar and button sidebar
      shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
      shinyjs::runjs("document.getElementsByClassName('sidebar-toggle')[0].style.visibility = 'visible';")

      # enable import menus
      shinyjs::removeCssClass(selector = "a[data-value='openFile']", class = "inactiveLink")
      shinyjs::removeCssClass(selector = "a[data-value='convert']", class = "inactiveLink")
      shinyjs::removeCssClass(selector = "a[data-value='demoData']", class = "inactiveLink")
    }
    else{ # "after data loaded"
      # hide sidebar/button sidebar
      shinyjs::addClass(selector = "body", class = "sidebar-collapse")
      shinyjs::runjs("document.getElementsByClassName('sidebar-toggle')[0].style.visibility = 'hidden';")

      # disable import menus
      shinyjs::addCssClass(selector = "a[data-value='openFile']", class = "inactiveLink")
      shinyjs::addCssClass(selector = "a[data-value='convert']", class = "inactiveLink")
      shinyjs::addCssClass(selector = "a[data-value='demoData']", class = "inactiveLink")
    }
  })
  
  
  #---------------------------Server modules calls---------------------------------------------------#
  mod_test_server('tutu')
  mod_homepage_server('home')
  #mod_settings_server("global_settings", obj = reactive({Exp1_R25_prot}))
  mod_release_notes_server("rl")
  mod_check_updates_server("check_updates")
  mod_insert_md_server("links_MD", URL_links)
  mod_insert_md_server("FAQ_MD", URL_FAQ)
  mod_bug_report_server("bug_report")
  
  
  
  # --------------------------------------------------------------
  # Once the server part is loaded, hide the loading page 
  # and show th main content
  shinyjs::hide(id = "loading_page", anim = FALSE)
  shinyjs::show("main_content", anim = TRUE, animType = "fade")
  
}
