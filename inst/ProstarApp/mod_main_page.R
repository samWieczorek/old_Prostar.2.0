# Module UI

#' @title   mod_main_page_ui and mod_loading_page_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_main_page
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList
#' @importFrom shinyjs inlineCSS
mod_main_page_ui <- function(id){
  ns <- NS(id)
  div(
      id = "header",
    
      shinydashboard::dashboardPage(
        #skin="blue",
        
        #theme = shinythemes::shinytheme("cerulean"),
        
        # https://stackoverflow.com/questions/31711307/how-to-change-color-in-shiny-dashboard
        # orangeProstar <- "#E97D5E"
        # gradient greenblue header
        # greenblue links <- #2fa4e7
        # darker greenblue hover links <- #157ab5
        # darker greenblue titles <- #317eac
        # small titles <- #9999
        # darkest greenblue button reset+next+selected menu
        # color background arrow : #88b7d5 (bleu gris clair)
        # lightgrey #dddd
        # grey #ccc
        # bleu ceruleen #2EA8B1
        # jaune clair 'mark' #FCF8E3
        # green #468847
        # darker green #356635
        
        ##
        ## Header
        ## 
        shinydashboard::dashboardHeader(
          # title on top right, shrinks when sidebar collapsed
          tags$li(class = "dropdown",
          #tags$style(".main-header {max-height: 20px}"),
          #tags$style(".main-header .logo {height: 20px;}"),
          #tags$style(".sidebar-toggle {height: 20px; padding-top: 1px !important;}"),
          #tags$style(".navbar {min-height:20px !important}"),
          tags$style(".skin-blue .main-header .navbar {background-color: green;}")
        ),
          ### changing logo
          title = dashboardthemes::shinyDashboardLogo(
            theme = "blue_gradient",
            boldText = "Prostar"
          ),
          # title = tagList(
          #   tags$span(
          #     class = "logo-mini", style =  "font-size : 14px","Prostar"),
          #   tags$span(
          #     class = "logo-lg", "Prostar")
          # ),
          # button to mimic data loaded
          tags$li(class="dropdown",
            actionButton('browser', 'Browser()')
          ),
          # links Prostar website and github
          tags$li(class="dropdown",
            a(href="http://www.prostar-proteomics.org/",
              img(src="logo.png",
                title="Prostar website",
                height="17px"))),
          tags$li(class="dropdown",
            a(href="https://github.com/samWieczorek/Prostar2",
              icon("github"),
              title="GitHub"))
        ),
        
        ##
        ## Sidebar
        ## 
        shinydashboard::dashboardSidebar(
          shinydashboard::sidebarMenu(id = "sb",
            # inactiveClass for import menus inactivation 
            tags$head(tags$style(".inactiveLink {
                           pointer-events: none;
                           background-color: grey;
                           }")),
            # Menus and submenus in sidebar
            br(),
            menuItem("Home", tabName = "ProstarHome", icon = icon("home"),selected = TRUE),
            hr(),
            menuItem("Data Manager", 
              icon = icon("folder"), 
              startExpanded = TRUE,
              menuSubItem("Open QFeature file", tabName = "openFile"),
              menuSubItem("Convert Data", tabName = "convert"),
              menuSubItem("Demo data", tabName = "demoData"),
              menuSubItem("Export Results", tabName = "export")
            ),
            hr(),
            menuItem("Pipeline", tabName = "pipeline", icon = icon("cogs")),
            hr(),
            menuItem("Dapar Viz", tabName = "daparviz", icon = icon("cogs")),
            hr(),
            menuItem("Help", 
              icon = icon("question-circle"),
              menuSubItem("Useful Links", tabName = "usefulLinks"),
              menuSubItem("FAQ", tabName = "faq"),
              menuSubItem("Bug Report", tabName = "bugReport"),
              menuSubItem("Global Settings", tabName = "globalSettings", icon = icon("cogs")),
              menuSubItem("Release Notes", tabName = "releaseNotes", icon = icon("clipboard")),
              menuSubItem("Check for Updates", tabName = "checkUpdates", icon = icon("wrench"))
            )
          )
        ),
        
        shinydashboard::dashboardBody(
          
          dashboardthemes::shinyDashboardThemes(
            theme = "blue_gradient"
          ),

          tagList(
            tags$head(
              tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
            ),
            
            useShinyjs(),
            
            # body content
            tabItems(
              tabItem(tabName = "ProstarHome", class="active",
                mod_homepage_ui('home')
              ),
              # tabItem(tabName = "openFile", h3("Open QFeature file"),
              #         mod_import_file_from_ui("open_file")),
              tabItem(tabName = "convert", 
                tagList(
                  h3("Convert datas"),
                  uiOutput('show_convert')
                )
              ),
              tabItem(tabName = "demoData", 
                tagList(
                  h3("Load a demo dataset"),
                  div(
                    div(
                      style="display:inline-block; vertical-align: middle; padding-right: 20px;",
                      mod_choose_pipeline_ui("pipe")
                    ),
                    div(
                      style="display:inline-block; vertical-align: middle; padding-right: 20px;",
                      #shinyjs::hidden(
                      # div(id='div_demoDataset',
                      mod_open_demoDataset_ui('demo_data')
                      # )
                      # )
                    ),
                    div(
                      style="display:inline-block; vertical-align: middle; padding-right: 20px;",
                      actionButton('load_dataset_btn', 'Load dataset', class=actionBtnClass)
                    )
                  )
                )
              ),
              
              tabItem(tabName = "daparviz", 
                tagList(
                  h3("Dapar viz"),
                  DaparViz::mod_all_ds_ui('daparviz')
                )
              ),
              
              
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
            # uiOutput('show_pipeline')
          )
        )
      )
      # mod_navbar_menu_ui('mainMenu')
      
      #                   fluidPage(
      #                     navbarPage("Prostar",
      #                     position = "fixed-top",
      #                     id = "navPage",
      #                     inverse = FALSE,
      # 
      #                     
      #                     
      #                     #modulePlotsUI('showPlots')
      #                     navbarMenu("Prostar",
      #                                tabPanel(title="Home",
      #                                         value="HomeTab",mod_homepage_ui("homepage")),
      #                                tabPanel(title="Global settings",
      #                                         value="GlobalSettingsTab", mod_settings_ui("modSettings")),
      #                                tabPanel("Release notes",
      #                                         value="ReleaseNotesTab",mod_release_notes_ui("modReleaseNotes")),
      #                                tabPanel("Check for updates",
      #                                         value="CheckUpdatesTab",mod_check_updates_ui("modCheckUpdates"))
      #                     ),
      #                     navbarMenu("Data manager",
      #                                tabPanel("Open MSnset",value = 'openMSnsetTab',
      #                                         mod_open_dataset_ui('moduleOpenDataset'),
      #                                         mod_infos_dataset_ui("infos_openFile")
      #                                         ),
      #                                tabPanel("Convert",value = "convertTab",
      #                                         mod_convert_ms_file_ui('moduleProcess_Convert')
      #                                         ),
      #                                tabPanel("Demo data",  value='demoTab', 
      #                                         mod_open_demo_dataset_ui('mod_OpenDemoDataset'),
      #                                         mod_infos_dataset_ui("infos_demoDataset")
      #                                         ),
      #                                tabPanel(title="ReloadProstar",
      #                                          value="ReloadTab",
      #                                          p("Due to some instability of cache memory when successively opening several datasets in a Prostar session, data management has been simplified.
      #                                           To work on another dataset than the current one, reloading Prostar first is now necessary (with the button above).  It will restart Prostar
      #                                           with a fresh R session where import menus are enabled 'Dataset manager' menu."),
      #                                          actionButton("ReloadProstar", "Reload Prostar",class = actionBtnClass)
      #                                           )
      #                     ),
      #                     # navbarMenu("Data mining",
      #                     #            tabPanel("Descriptive statistics", value='descriptiveStats', mod_all_plots_ui('modAllPlots'))
      #                     # ),
      #                     navbarMenu("Help",
      #                                tabPanel("Links",value="usefulLinksTab",  mod_insert_md_ui('links_MD')),
      #                                tabPanel("FAQ", value="faqTab",  mod_insert_md_ui('FAQ_MD')),
      #                                tabPanel("Bug report",value="bugReportTab",  mod_bug_report_ui('bugreport')
      # 
      #                     )
      #                     )
      #                   ) ## end navbarPage
      # )
    )

}

# Module Server

#' @rdname mod_mainpage
#' @export
#' @keywords internal

mod_main_page_server <- function(id){
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
  })
  
}
