# Module UI


redBtnClass <- "btn-danger"
PrevNextBtnClass <- "btn-info"
btn_success_color <- "btn-success"
optionsBtnClass <- "info"


#' @title   mod_choose_pipeline_ui and mod_choose_pipeline_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#'
#' @rdname mod_convert
#'
#' @keywords internal
#' @export
#'
#' @importFrom shiny NS tagList
#' @import sos
#'
mod_Convert_ui <- function(id){
  # ns <- NS(id)
  # tagList()
}




#' Convert Server Function
#'
#' @param id xxx
#' @param dataIn xxx
#' @param steps.enabled xxx
#' @param remoteReset xxx
#'
#' @noRd
#'
#' @import DAPAR2
#' @import QFeatures
#' @importFrom shinyalert shinyalert
#'
#' @export
#'
mod_Convert_server <- function(id,
                               dataIn = NULL,
                               steps.enabled = reactive({NULL}),
                               remoteReset = reactive({FALSE})
){
  
  global <- list(
    VALIDATED = 1,
    UNDONE = 0,
    SKIPPED = -1
  )
  
  widgets.default.values <- list(
    selectFile_typeOfData = NULL,
    selectFile_checkDataLogged = 'no',
    selectFile_replaceAllZeros = TRUE,
    selectFile_software = NULL,
    DataID_colnameForID  = NULL,
    DataID_proteinId = NULL
  )
  
  ###-------------------------------------------------------------###
  ###                                                             ###
  ### ------------------- MODULE SERVER --------------------------###
  ###                                                             ###
  ###-------------------------------------------------------------###
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    rv <- reactiveValues(
      dataIn = NULL,
      dataOut = NULL,
      status = NULL,
      reset = NULL,
      steps.enabled = NULL
    )
    
    
    dataOut <- reactiveValues(
      trigger = NULL,
      value = NULL
    )
    
    config <- list(
      name = 'Convert',
      steps = c('Description','SelectFile','DataID','ExpFeatData','SamplesMetadata','Save'),
      mandatory = c(TRUE,TRUE,TRUE,TRUE,TRUE,TRUE)
    )
    
    rv.widgets <- reactiveValues(
      selectFile_typeOfData = widgets.default.values$selectFile_typeOfData,
      selectFile_checkDataLogged = widgets.default.values$selectFile_checkDataLogged,
      selectFile_replaceAllZeros = widgets.default.values$selectFile_replaceAllZeros,
      selectFile_software = widgets.default.values$selectFile_software,
      selectFile_XLSsheets = widgets.default.values$selectFile_XLSsheets,
      DataID_colnameForID  = widgets.default.values$DataID_colnameForID,
      DataID_proteinId = widgets.default.values$DataID_proteinId
    )
    
    #
    # Initialization of the module
    #
    observeEvent(steps.enabled(), ignoreNULL = TRUE, {
      if (is.null(steps.enabled()))
        rv$steps.enabled <- setNames(rep(FALSE, rv.process$length), rv.process$config$steps)
      else
        rv$steps.enabled <- steps.enabled()
    })
    
    
    observeEvent(remoteReset(), {
      lapply(names(rv.widgets), function(x){
        rv.widgets[[x]] <- widgets.default.values[[x]]
      })
    })
    
    
    ###### ------------------- Code for Description (step 0) -------------------------    #####
    output$Description <- renderUI({
      rv$steps.enabled
      #browser()
      wellPanel(
        tagList(
          includeMarkdown( system.file('app/md', paste0(config$name, '.md'), package='Prostar.2.0')),
          uiOutput(ns('datasetDescription')),
          if (isTRUE(rv$steps.enabled['Description'])  )
            actionButton(ns('btn_validate_Description'),
                         paste0('Start ', config$name),
                         class = btn_success_color)
          else
            shinyjs::disabled(
              actionButton(ns('btn_validate_Description'),
                           paste0('Start ', config$name),
                           class = btn_success_color)
            )
        )
      )
      
      
      
    })
    
    observeEvent(input$btn_validate_Description, ignoreInit = T, ignoreNULL=T, {
      rv$dataIn <- dataIn()
      dataOut$trigger <- Send_Result_to_Caller(rv$dataIn)$trigger
      dataOut$value <- Send_Result_to_Caller(rv$dataIn)$value
    })
    
    
    
    
    ###-------------------------------------------------------------###
    ### --------------- Code for step 1: selectFile ----------------###
    ###-------------------------------------------------------------###
    
    observeEvent(input$selectFile_typeOfData,{rv.widgets$selectFile_typeOfData <- input$selectFile_typeOfData})
    observeEvent(input$selectFile_checkDataLogged,{rv.widgets$selectFile_checkDataLogged <- input$selectFile_checkDataLogged})
    observeEvent(input$selectFile_replaceAllZeros,{rv.widgets$selectFile_replaceAllZeros <- input$selectFile_replaceAllZeros})
    observeEvent(input$selectFile_XLSsheets,{rv.widgets$selectFile_XLSsheets <- input$selectFile_XLSsheets})
    observeEvent(input$selectFile_software,{rv.widgets$selectFile_software <- input$selectFile_software})
    
    mod_popover_for_help_server("modulePopover_convertChooseDatafile", 
                                data = list(title = "Choose file to import", 
                                                     content="Select one (.txt, .csv, .tsv, .xls, .xlsx) file.")
                                )
    
    
    output$choose_file_to_import_ui <- renderUI({
  
      #req(rv.widgets$selectFile_software)
      fluidRow(
        column(width=2, mod_popover_for_help_ui(ns("modulePopover_convertChooseDatafile"))),
        column(width = 10, if (rv$steps.enabled['SelectFile'])
          fileInput(ns("selectFile_file2convert"), "",
                    multiple=FALSE,
                    accept = c(
                      "text/csv",
                      "text/comma-separated-values,text/plain",
                      ".csv",
                      ".xls", ".xlsx")
                    )
          else
            shinyjs::disabled(fileInput(ns("selectFile_file2convert"), "Choose file to import",
                                        multiple=FALSE,
                                        accept=c(".txt", ".tsv", ".csv",".xls", ".xlsx")))
        ))
    })
    
    output$Warning_BadFile_ui <- renderUI({
      req(!(tools::file_ext(input$selectFile_file2convert$datapath) %in% c("csv", 'txt', 'xlsx', 'xls')))
      #ext <- tools::file_ext(input$selectFile_file2convert$datapath)
      
      #validate(need(ext %in% c("csv", 'txt', 'xlsx', 'xls'), h3("Please upload a csv file")))
      h3('Please upload a correct file')
      
    })
    output$ConvertOptions_ui <- renderUI({
     # req(rv.widgets$selectFile_software)
     #browser()
      #req(input$file2convert$name)
      rv.widgets$selectFile_typeOfData
      rv.widgets$selectFile_checkDataLogged
      rv.widgets$selectFile_replaceAllZeros
      
      tagList(
        if (rv$steps.enabled['SelectFile'])
          radioButtons(ns("selectFile_typeOfData"),
                       "Is it a peptide or protein dataset ?",
                       choices=c("peptide dataset" = "peptide",
                                 "protein dataset" = "protein"),
                       selected = rv.widgets$selectFile_typeOfData
          )
        else
          shinyjs::disabled(radioButtons(ns("selectFile_typeOfData"),
                                         "Is it a peptide or protein dataset ?",
                                         choices=c("peptide dataset" = "peptide",
                                                   "protein dataset" = "protein"),
                                         selected = rv.widgets$selectFile_typeOfData
          )
          )
        
        , if (rv$steps.enabled['SelectFile'])
          radioButtons(ns("selectFile_checkDataLogged"),
                       "Are your data already log-transformed ?",
                       choices=c("yes (they stay unchanged)" = "yes",
                                 "no (they will be automatically transformed)"="no"),
                       selected = rv.widgets$selectFile_checkDataLogged)
        else
          shinyjs::disabled(radioButtons(ns("selectFile_checkDataLogged"),
                                         "Are your data already log-transformed ?",
                                         choices=c("yes (they stay unchanged)" = "yes",
                                                   "no (they will be automatically transformed)"="no"),
                                         selected = rv.widgets$selectFile_checkDataLogged)
          )
        ,br()
        ,if (rv$steps.enabled['SelectFile'])
          checkboxInput(ns("selectFile_replaceAllZeros"),
                        "Replace all 0 and NaN by NA",
                        value = rv.widgets$selectFile_replaceAllZeros)
        else
          shinyjs::disabled(checkboxInput(ns("selectFile_replaceAllZeros"),
                                          "Replace all 0 and NaN by NA",
                                          value = rv.widgets$selectFile_replaceAllZeros)
          )
      )
      
      
    })
    
    
    output$ManageXlsFiles_ui <- renderUI({
      req(rv.widgets$selectFile_software)
      req(tools::file_ext(input$selectFile_file2convert$datapath) %in% c('xlsx', 'xls'))
      
       sheets <- listSheets(input$selectFile_file2convert$datapath)
        if (rv$steps.enabled['SelectFile'])
          selectInput(ns("selectFile_XLSsheets"), "sheets",
                      choices = as.list(sheets),
                      selected = rv.widgets$selectFile_XLSsheets,
                      width='200px')
        else
          shinyjs::disabled(
            selectInput(ns("selectFile_XLSsheets"), "sheets",
                        choices = as.list(sheets),
                        selected = rv.widgets$selectFile_XLSsheets,
                        width='200px')
          )

      
      
    })

    ############ Read text file to be imported ######################
    observeEvent(req(input$selectFile_file2convert),{
      rv.widgets$selectFile_XLSsheets
      
      ext <- tools::file_ext(input$selectFile_file2convert$datapath)
      validate(need(ext %in% c("csv", 'txt', 'xlsx', 'xls'), "Please upload a csv file"))
      
        # result = tryCatch(
        #   {
        #ClearUI()
        #ClearMemory()
        dpath <- input$selectFile_file2convert$datapath
        # shinyjs::disable("selectFile_file2convert")
        switch(tools::file_ext(input$selectFile_file2convert$datapath),
               txt = { rv$data2convert <- read.csv(dpath,  header=TRUE, sep="\t", as.is=T)},
               csv = { rv$data2convert <- read.csv(dpath,  header=TRUE, sep=";", as.is=T)},
               tsv = { rv$data2convert <- read.csv(dpath,  header=TRUE, sep="\t", as.is=T)},
               xls = { rv$data2convert <- readExcel(dpath, ext, sheet=rv.widgets$selectFile_XLSsheets)},
               xlsx = {rv$data2convert <- readExcel(dpath, ext, sheet=rv.widgets$selectFile_XLSsheets)}
        )
        #   }
        #   , warning = function(w) {
        #     shinyjs::info(conditionMessage(w))
        #   }, error = function(e) {
        #     shinyjs::info(paste("Read text file to convert",":",
        #                         conditionMessage(e), 
        #                         sep=" "))
        #   }, finally = {
        #     #cleanup-code 
        #   })
      #shinyjs::disable('file1')
      
    })
    
    
    
    ##
    ## Main renderUI() function for the step SelectFile
    ##
    output$SelectFile <- renderUI({
      name <- 'SelectFile'
      tagList(
        div(
          div(style="display:inline-block; vertical-align: middle; padding-right: 40px;",
              uiOutput(ns('choose_file_to_import_ui'))),
          div(style="display:inline-block; vertical-align: middle; padding-right: 40px;",
              uiOutput(ns('Warning_BadFile_ui'))
              )
          ),
        div(style="display:inline-block; vertical-align: middle; padding-right: 40px;",
            if (rv$steps.enabled['SelectFile'])
              radioButtons(ns("selectFile_software"), "Software to import from",
                           choices = setNames(nm = c('maxquant', 'proline')),
                           selected = rv.widgets$selectFile_software
              )
            else
              shinyjs::disabled(radioButtons(ns("selectFile_software"), "Software to import from",
                                             choices = setNames(nm = c('maxquant', 'proline')),
                                             selected =  rv.widgets$selectFile_software
              )
              )
        ),
        uiOutput(ns("ManageXlsFiles_ui")),
        uiOutput(ns("ConvertOptions_ui")),
        div(
          if (rv$steps.enabled['SelectFile'])
            actionButton(ns('btn_validate_SelectFile'),
                         'Perform SelectFile',
                         class = btn_success_color)
          else
            shinyjs::disabled(
              actionButton(ns('btn_validate_SelectFile'),
                           'Perform SelectFile',
                           class = btn_success_color)
            )
        )
      )
    })
    
    
    
    observeEvent(input$btn_validate_SelectFile, ignoreInit = T, {
      # Add your stuff code here
      #browser()
      dataOut$trigger <- Send_Result_to_Caller(rv$dataIn)$trigger
      dataOut$value <- Send_Result_to_Caller(rv$dataIn)$value
    })
    
    ###### ------------------- Code for step 2: DataID -------------------------    #####
    
    observeEvent(input$DataID_colnameForID,{rv.widgets$DataID_colnameForID <- input$DataID_colnameForID})
    observeEvent(input$DataID_proteinId,{rv.widgets$DataID_proteinId <- input$DataID_proteinId})
     
    
    
    mod_popover_for_help_server("pop_convertIdType", 
               data = list(title = "ID definition", 
                                    content="If you choose the automatic ID, Prostar will build an index."))
    
    # Comment your function
    output$DataID_id_ui <- renderUI({
      req(rv$data2convert)
      
      .choices <- setNames(c("AutoID", colnames(rv$data2convert)), c("Auto ID", colnames(rv$data2convert)))
      
      tagList(
        mod_popover_for_help_ui(ns("pop_convertIdType")),
        div(
          if (rv$steps.enabled['DataID'])
            selectInput(ns("DataID_colnameForID"), label = "", choices = .choices)
          else
            shinyjs::disabled(selectInput(ns("DataID_colnameForID"), label = "", choices = .choices))
      )
      )
      
    })
    
    # Comment your function
    output$DataID_warningNonUniqueID_ui <- renderUI({
      req(rv.widgets$DataID_colnameForID != 'AutoID')
      req(rv$data2convert)
      
      t <- (length(as.data.frame(rv$data2convert)[, rv.widgets$DataID_colnameForID])
            == length(unique(as.data.frame(rv$data2convert)[, rv.widgets$DataID_colnameForID])))
      
      if (!t){
        text <- "<img src=\"images/Problem.png\" height=\"24\"></img><font color=\"red\">
        Warning ! Your ID contains duplicate data.
        Please choose another one."
        
      }
      else {
        text <- "<img src=\"images/Ok.png\" height=\"24\"></img>"
      }
      HTML(text)
    })
    
    mod_popover_for_help_server("pop_convertProteinID", 
               data = list(title = "Select protein IDs", 
                                    content="Select the column containing the parent protein IDs."))
    
    # Comment your function
    output$DataID_ChooseProteinID_ui <- renderUI({
      req(rv$data2convert)
      req(rv.widgets$selectFile_typeOfData != "protein")
      
       tagList(
        mod_popover_for_help_ui(ns("pop_convertProteinID")),
        selectInput(ns("DataID_proteinId"), 
                    "",
                    choices =  setNames(nm = c("",colnames(rv$data2convert))) ,
                    selected = rv.widgets$DataID_proteinId)
      )
    })
    
    
    
    output$DataID_helpTextDataID_ui <- renderUI({
      req(rv.widgets$selectFile_typeOfData)
      
      t <- ""
      switch(rv.widgets$selectFile_typeOfData,
             protein = {t <- "proteins"},
             peptide = {t <- "peptides"}
      )
      txt <- paste ("Please select among the columns of your data the one that 
                corresponds to a unique ID of the ", t, ".", sep=" ")
      helpText(txt)
      
    })
    
    
    
    
    
    
    datasetID_Ok <- reactive({
      req(rv.widgets$DataID_colnameForID)
      req(rv$data2convert)
      if (rv.widgets$DataID_colnameForID == "AutoID") 
        t <- TRUE
      else 
        t <- (length(as.data.frame(rv$data2convert)[, rv.widgets$DataID_colnameForID])
              == length(unique(as.data.frame(rv$data2convert)[, rv.widgets$DataID_colnameForID])))

      t
    })
    
    
    
    output$DataID_previewProteinID_ui <- renderUI({
      req(rv.widgets$DataID_proteinId != "")
      
      tagList(
        p(style="color: black;", 'Preview'),
        tableOutput(ns("DataID_previewProtID"))
      )
      
    })
    
    
    
    output$DataID_previewProtID <- renderTable(
       head(rv$data2convert[,rv.widgets$DataID_proteinId]),
      colnames = FALSE
    )
    
    
    
    
    
    
    output$DataID <- renderUI({
      name <- 'DataID'
      tagList(
        tags$div(
          tags$div( style="display:inline-block; vertical-align: top; padding-right: 100px;",
                    uiOutput(ns("DataID_id_ui")),
                    uiOutput(ns("DataID_warningNonUniqueID_ui"))
          ),
          tags$div( style="display:inline-block; vertical-align: top;",
                    uiOutput(ns("DataID_ChooseProteinID_ui")),
                    uiOutput(ns("DataID_previewProteinID_ui"))
        ),
        div(
          if (rv$steps.enabled['DataID'])
            actionButton(ns('btn_validate_DataID'),
                         'Perform DataID',
                         class = btn_success_color)
          else
            shinyjs::disabled(
              actionButton(ns('btn_validate_DataID'),
                           'Perform DataID',
                           class = btn_success_color)
            )
        )
      )
      )
    })
    
    observeEvent(input$btn_validate_DataID, ignoreInit = T, {
      # Add your stuff code here
      dataOut$trigger <- Send_Result_to_Caller(rv$dataIn)$trigger
      dataOut$value <- Send_Result_to_Caller(rv$dataIn)$value
    })
    
    ###### ------------------- Code for ExpFeatData -------------------------    #####
    
    output$ExpFeatData <- renderUI({
      name <- 'ExpFeatData'
      tagList(
        
        div(
          if (rv$steps.enabled['ExpFeatData'])
            actionButton(ns(paste0('btn_validate_', ExpFeatData)),
                         'Perform ExpFeatData',
                         class = btn_success_color)
          else
            shinyjs::disabled(
              actionButton(ns(paste0('btn_validate_', ExpFeatData)),
                           'Perform ExpFeatData',
                           class = btn_success_color)
            )
        )
      )
      
    })
    
    observeEvent(input$btn_validate_ExpFeatData, ignoreInit = T, {
      # Add your stuff code here
      dataOut$trigger <- Send_Result_to_Caller(rv$dataIn)$trigger
      dataOut$value <- Send_Result_to_Caller(rv$dataIn)$value
    })
    
    
    ###### ------------------- Code for SamplesMetadata -------------------------    #####
    
    output$SamplesMetadata <- renderUI({
      name <- 'SamplesMetadata'
      tagList(
        
        div(
          if (rv$steps.enabled['SamplesMetadata'])
            actionButton(ns(paste0('btn_validate_', SamplesMetadata)),
                         'Perform SamplesMetadata',
                         class = btn_success_color)
          else
            shinyjs::disabled(
              actionButton(ns(paste0('btn_validate_', SamplesMetadata)),
                           'Perform SamplesMetadata',
                           class = btn_success_color)
            )
        )
      )

    })
    
    
    observeEvent(input$btn_validate_SamplesMetadata, ignoreInit = T, {
      # Add your stuff code here
      dataOut$trigger <- Send_Result_to_Caller(rv$dataIn)$trigger
      dataOut$value <- Send_Result_to_Caller(rv$dataIn)$value
    })
    
    ###### ------------------- Code for Save -------------------------    #####
    
    output$Save <- renderUI({
      name <- 'Save'
      tagList(
        div(
          if (rv$steps.enabled['Save'])
            actionButton(ns(paste0('btn_validate_', Save)),
                         'Perform Save',
                         class = btn_success_color)
          else
            shinyjs::disabled(
              actionButton(ns(paste0('btn_validate_', Save)),
                           'Perform Save',
                           class = btn_success_color)
            )
        )
      )

    })
    
    
    observeEvent(input$btn_validate_Save, ignoreInit = T, {
      # Add your stuff code here
      rv$dataIn <- AddItemToDataset(rv$dataIn, config$name)
      dataOut$trigger <- Send_Result_to_Caller(rv$dataIn)$trigger
      dataOut$value <- Send_Result_to_Caller(rv$dataIn)$value
    })
    #------------- Code for validation step ---------------

    # Return value of module
    # DO NOT MODIFY THIS PART
    list(config = reactive({
      config$ll.UI <- setNames(lapply(config$steps,
                                      function(x){
                                        do.call('uiOutput', list(ns(x)))
                                      }),
                               paste0('screen_', config$steps)
      )
      config
    }),
    dataOut = reactive({dataOut})
    #status = reactive({rv$status})
    )
    
    
  })
}
