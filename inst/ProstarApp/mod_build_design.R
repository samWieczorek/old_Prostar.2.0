#' build_design UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList
#' @importFrom shinyBS bsModal
#' @importFrom shinyjs useShinyjs
#' @import rhandsontable 
#' 
#' @export 
#' 
#' 
mod_build_design_ui <- function(id){
  ns <- NS(id)
  shinyjs::useShinyjs()
  tagList(
    tags$p("If you do not know how to fill the experimental design, you can click
           on the '?' next to each design in the list that appear once the conditions
           are checked or got to the ",
           actionLink(ns("linkToFaq1"), "FAQ", style="background-color: white"),
           " page."),
    
    fluidRow(
      column(width=6,tags$b("1 - Fill the \"Condition\" column to identify the conditions to compare.")),
      column(width=6,uiOutput(ns("UI_checkConditions"))  )
    ),
    fluidRow(
      column(width=6,uiOutput(ns("UI_hierarchicalExp"))),
      column(width=6,uiOutput(ns("checkDesign") ))
    ),
    hr(),
    div(style = "display:inline-block; vertical-align: middle; padding: 7px;",
        p('Order by conditions')),
    div(style = "display:inline-block; vertical-align: middle; padding: 7px;",
        selectInput(ns("convert_reorder"), "",
                choices=setNames(nm=c("No", "Yes")),
                width="80px"
                )
        ),
    h4("Design table"),
    tags$div(
      tags$div(style="display:inline-block; vertical-align: top;",
               rhandsontable::rHandsontableOutput(ns("hot"),width="100%"),
      ),
      tags$div(style="display:inline-block; vertical-align: top;",
               shinyjs::hidden(uiOutput(ns('showExamples')))
      )
    )
  )
}
    
#' build_design Server Functions
#'
#' @noRd 
#' 
#' @import rhandsontable
#' @importFrom DaparToolshed CheckDesign check.conditions
#' @import shinyjs
#' 
#' @export
#' 
mod_build_design_server <- function(id, sampleNames){
  
  widgets.default.values <- list(
    Design_chooseExpDesign = NULL
  )
  
  
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    rv.buildDesign <- reactiveValues(
      design_df = NULL,
      hot_table = NULL,
      designChecked = NULL,
      newOrder = NULL,
      conditionsChecked = NULL,
      designSaved = NULL,
      level = NULL,
      out = NULL
    )
    
    rv.widgets <- reactiveValues(
      Design_chooseExpDesign = widgets.default.values$Design_chooseExpDesign
    )
      
    mod_insert_md_server('FAQ', URL_FAQ)
    
    mod_build_design_example_server('designExamples')
    
    output$showExamples <- renderUI({
      mod_build_design_example_ui(ns('designExamples') )
    })
    
    
    #
    # Comment the function
    #
    color_renderer <- reactive({
      req(rv.buildDesign$design_df)
      conds <- rv.buildDesign$design_df$Condition
      if (length(which(conds==""))==0)
        uniqueConds <- unique(conds)
      else
        uniqueConds <- unique(conds[-which(conds=="")])
      pal <- DAPAR::ExtendPalette(length(uniqueConds))
      
      txt <- "function (instance, td, row, col, prop, value, cellProperties) {
                      Handsontable.renderers.TextRenderer.apply(this, arguments);"
      c <- 1
      for (i in 1:length(conds)){
        if (conds[i] != "")
          txt <- paste0(txt, "if(row==",(i-1)," && col==",c, ") {td.style.background = '",pal[which(conds[i] == unique(conds))],"';}")
      }
      txt <- paste0(txt,"}")
      
      return (txt)
    })
    
    
    
    
    
    #----------------------------------------------------------
    observeEvent(input$btn_checkConds,{
      input$convert_reorder
      
      req(length(grep("Bio.Rep", colnames(rv.buildDesign$design_df))) == 0)
      
      if (isTRUE(input$convert_reorder)) {
        rv.buildDesign$newOrder <- order(rv.buildDesign$design_df["Condition"])
        rv.buildDesign$design_df <- rv.buildDesign$design_df[rv.buildDesign$newOrder,]
      }
      
      rv.buildDesign$conditionsChecked <- DaparToolshed::check.conditions(rv.buildDesign$design_df$Condition)
      
    })
    
    observeEvent(input$Design_chooseExpDesign, {
      rv.widgets$Design_chooseExpDesign <- input$Design_chooseExpDesign
    })
    
    
    #----------------------------------------------------------
    observeEvent(req(sampleNames()),{
      rv.buildDesign$design_df  <- data.frame(Sample.name = as.character(sampleNames()),
                                              Condition = rep("",length(sampleNames())),
                                              stringsAsFactors = FALSE)
    })
    
    
    
    
    #-------------------------------------------------------------
    output$hot <- rhandsontable::renderRHandsontable({
      rv.buildDesign$design_df
      rv.widgets$Design_chooseExpDesign
      
      tmp <- rhandsontable::rhandsontable(rv.buildDesign$design_df,
                                          rowHeaders=NULL, 
                                          fillHandle = list(direction='vertical', 
                                                            autoInsertRow=FALSE,
                                                            maxRows=nrow(rv.buildDesign$design_df)
                                          )
      ) %>%
        rhandsontable::hot_rows(rowHeights = 30) %>%
        rhandsontable::hot_context_menu(allowRowEdit = TRUE, 
                                        allowColEdit = FALSE,
                                        allowInsertRow = FALSE,
                                        allowInsertColumn = FALSE,
                                        allowRemoveRow = TRUE,
                                        allowRemoveColumn = FALSE,
                                        autoInsertRow=FALSE) %>%
        rhandsontable:: hot_cols(renderer = color_renderer()) %>%
        rhandsontable::hot_col(col = "Sample.name", readOnly = TRUE)
      
      if (!is.null(rv.widgets$Design_chooseExpDesign)) {
        switch(rv.widgets$Design_chooseExpDesign,
               FlatDesign = {
                 if ("Bio.Rep" %in% colnames(rv.buildDesign$design_df))
                   tmp <- tmp %>% rhandsontable::hot_col(col = "Bio.Rep", readOnly = TRUE)
               },
               twoLevelsDesign = {
                 if ("Tech.Rep" %in% colnames(rv.buildDesign$design_df))
                   tmp <- tmp %>% rhandsontable::hot_col(col =  "Tech.Rep", readOnly = TRUE)
               } ,
               threeLevelsDesign = {
                 if ("Analyt.Rep" %in% colnames(rv.buildDesign$design_df))
                   tmp <- tmp %>% rhandsontable::hot_col(col = "Analyt.Rep", readOnly = TRUE)
               }
        )
      }
      
      tmp
      
    })
    
    
    # 
    # 
    # 
    # #----------------------------------------------------------
    output$UI_checkConditions  <- renderUI({
      
      req(rv.buildDesign$design_df)
      rv.buildDesign$conditionsChecked
      input$convert_reorder
      
      if ((sum(rv.buildDesign$design_df$Condition == "")==0) && (input$convert_reorder != "None")){
        tags$div(
          tags$div(style="display:inline-block;",
                   actionButton(ns("btn_checkConds"), "Check conditions", class = actionBtnClass)
          ),
          
          tags$div(style="display:inline-block;",
                   if(!is.null(rv.buildDesign$conditionsChecked)){
                     
                     if (isTRUE(rv.buildDesign$conditionsChecked$valid)){
                       img <- "Ok.png"
                       txt <- "Correct conditions"
                     }else {
                       img <- "Problem.png"
                       txt <- "Invalid conditions"
                     }
                     tagList(
                       tags$div(
                         tags$img(src = base64enc::dataURI(file=system.file('app/www/images', img, package="Prostar.2.0"), 
                                                           mime="image/png"),
                                  width='30px'),
                         tags$div(style="display:inline-block;",tags$p(txt))
                       ),
                       if(!isTRUE(rv.buildDesign$conditionsChecked$valid)){
                         tags$p(rv.buildDesign$conditionsChecked$warn)
                       }
                     )
                   }
          )
        )
      }
    })
    
    
    
    # #------------------------------------------------------------------------------
    output$UI_hierarchicalExp <- renderUI({
      #req(rv.buildDesign$conditionsChecked)
      req(rv.buildDesign$conditionsChecked$valid)
      
      
      tagList(
        div(
          div(
            # edit1
            style="display:inline-block; vertical-align: middle;",
            tags$b("2 - Choose the type of experimental design and complete it accordingly")
          ),
          div(
            # edit2
            style="display:inline-block; vertical-align: middle;",
            #tags$button(id=ns("btn_helpDesign"), tags$sup("[?]"), class="Prostar_tooltip")
            actionLink(ns("btn_helpDesign"), tags$sup("?"),style="background-color: white, color: blue")
          )
        ),
        
        radioButtons(ns("Design_chooseExpDesign"), "",
                     choices = c("Flat design (automatic)" = "FlatDesign" ,
                                 "2 levels design (complete Bio.Rep column)" = "twoLevelsDesign" ,
                                 "3 levels design (complete Bio.Rep and Tech.Rep columns)" = "threeLevelsDesign" ),
                     selected = rv.widgets$Design_chooseExpDesign)
      )
      
    })
    
    
    
    # observe({
    #   shinyjs::onclick('btn_helpDesign', {
    #     print('click on btn_helpDesign')
    #   })
    # })
    # 
    
    
    # #------------------------------------------------------------------------------
    observeEvent(input$btn_helpDesign,{
      #req(input$chooseExpDesign)
      shinyjs::toggle(id = "showExamples", anim = TRUE)
      
    })
    
    
    
    
    #------------------------------------------------------------------------------
    observeEvent(req(rv.widgets$Design_chooseExpDesign), {
      rv.buildDesign$design_df
      #rv.buildDesign$designChecked <- FALSE
      
      switch(rv.widgets$Design_chooseExpDesign,
             FlatDesign = {
               rv.buildDesign$design_df <- data.frame(rv.buildDesign$design_df[,1:2],
                                                      Bio.Rep = seq(1:nrow(rv.buildDesign$design_df)),
                                                      stringsAsFactors = FALSE)
               rv.buildDesign$level <- 1
             },
             twoLevelsDesign = {
               rv.buildDesign$design_df  <- data.frame(rv.buildDesign$design_df[,1:2],
                                                       Bio.Rep = rep("",nrow(rv.buildDesign$design_df)),
                                                       Tech.Rep = seq(1:nrow(rv.buildDesign$design_df)),
                                                       stringsAsFactors = FALSE)
               rv.buildDesign$level <- 2
             },
             threeLevelsDesign = {
               rv.buildDesign$design_df  <- data.frame(rv.buildDesign$design_df[,1:2],
                                                       Bio.Rep = rep("", nrow(rv.buildDesign$design_df)),
                                                       Tech.Rep = rep("", nrow(rv.buildDesign$design_df)),
                                                       Analyt.Rep = seq(1:nrow(rv.buildDesign$design_df)),
                                                       stringsAsFactors = FALSE)
               rv.buildDesign$level <- 3
             }
      )
    })
    
    
    #------------------------------------------------------------------------------
    observeEvent(input$hot,{
      rv.buildDesign$design_df <-  rhandsontable::hot_to_r(input$hot)
    })
    
    
    
    #------------------------------------------------------------------------------
    observeEvent(input$btn_checkDesign,{
      rv.buildDesign$designChecked <- DaparToolshed::CheckDesign(rv.buildDesign$design_df)
      if (isTRUE(rv.buildDesign$designChecked$valid))
        rv.buildDesign$out <- rv.buildDesign$design_df
    })
    
    
    #------------------------------------------------------------------------------
    output$checkDesign <- renderUI({
     req(rv.buildDesign$design_df)
      req(rv.widgets$Design_chooseExpDesign)
     
      n <- ncol(rv.buildDesign$design_df)
      replicateComplete <- length(which(rv.buildDesign$design_df[ ,3:n] == ''))==0
      if (replicateComplete) {
        tags$div(
          tags$div(
            style="display:inline-block;",
            actionButton(ns("btn_checkDesign"), "Check design", class = actionBtnClass)
          ),
          
          tags$div(
            style="display:inline-block;",
            if(!is.null(rv.buildDesign$designChecked)){
              
              if (isTRUE(rv.buildDesign$designChecked$valid)){
                img <- "Ok.png"
                txt <- "Correct design"
              }else {
                img <- "Problem.png"
                txt <- "Invalid design"
              }
              tagList(
                tags$div(
                  tags$img(src = base64enc::dataURI(file=system.file('app/www/images', img, package="Prostar.2.0"), 
                                                    mime="image/png"),
                           width='30px'),
                  tags$div(style="display:inline-block;",tags$p(txt))
                )
              )
            } 
          )
          
        )
      }
      
    })
    
    
    reactive({rv.buildDesign$out})
    
    
  })
  
}
    
## To be copied in the UI
# mod_build_design_ui("build_design_ui_1")
    
## To be copied in the server
# mod_build_design_server("build_design_ui_1")
