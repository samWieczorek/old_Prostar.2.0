# TODO Continue the adaptation of convert module but need to update DAPAR2 before

#' @author Samuel Wieczorek
#' 
#' @export
#' 
#' @import rhandsontable
#' 
Convert = R6Class(
  "Convert",
  inherit = Process,
  private = list(
    .config = list(name = 'Convert',
                   steps = c('Description', 'SelectFile', 'DataId', 'QuantiData', 'BuildDesign', 'Convert'),
                   mandatory = c(T, T, T, T, T, T)
    )
  ),

  
  public = list(

    Global_server = function(session, input, output){
      self$rv$value.test <- 3
      self$rv$choices <- 1:6
      
      
      
      callModule(modulePopover,"modulePopover_convertChooseDatafile", 
                 data = reactive(list(title = HTML(paste0("<strong><font size=\"4\">Data file</font></strong>")), 
                                      content="Select one (.txt, .csv, .tsv, .xls, .xlsx) file.")))
      
      callModule(modulePopover,"modulePopover_convertIdType", 
                 data = reactive(list(title = HTML(paste0("<strong><font size=\"4\">ID definition</font></strong>")), 
                                      content="If you choose the automatic ID, Prostar will build an index.")))
      
      
      
      callModule(modulePopover,"modulePopover_convertProteinID", 
                 data = reactive(list(title = HTML(paste0("<strong><font size=\"4\">Select protein IDs</font></strong>")), 
                                      content="Select the column containing the parent protein IDs.")))
      
      
      callModule(modulePopover,"modulePopover_convertDataQuanti", 
                 data = reactive(list(title = HTML(paste0("<strong><font size=\"4\">Quantitative data</font></strong>")), 
                                      content="Select the columns that are quantitation values by clicking in the field below.")))
      
      callModule(moduleStaticDataTable,"overview_convertData", table2show=reactive({GetDatasetOverview()}))
      
      
      
      
    },
    
    
    
    #------------------- Step 1: description ----------------------------------------
    Description_server = function(input, output){
      
      observeEvent(input$btn_validate_Description, ignoreInit = T, ignoreNULL=T, {
        cat(paste0(class(self)[1], "::observeEvent(input$btn_validate_Description from - ", self$id, '\n'))
        private$InitializeDataIn()
        self$ValidateCurrentPos()
      })
      
      output$datasetDescription <- renderUI({
        req(self$rv$temp.dataIn)
        tagList(
          p(paste0('Dataset description: ', paste0(names(self$rv$temp.dataIn), collapse=", "))),
          )
      })
    },
    
    
    Description_ui = function(){
      
      wellPanel(
        tagList(
          includeMarkdown( system.file("md", paste0(self$config$name, ".md"), package="Prostar.2.0")),
          uiOutput(self$ns('datasetDescription')),
          actionButton(self$ns('btn_validate_Description'),
                       paste0('Start ', self$config$name),
                       class = 'btn-success')
        )
      )
    },
    
    
    
    
    #------------------- Step 2: Select file ----------------------------------------
    
    SelectFile_server = function(input, output){
      
      
      output$ConvertOptions <- renderUI({
        req(input$file1)
        
        tagList(
          radioButtons(self$ns("typeOfData"), 
                       "Is it a peptide or protein dataset ?", 
                       choices=c("peptide dataset" = "peptide", 
                                 "protein dataset" = "protein")
          )
          
          ,radioButtons(self$ns("checkDataLogged"), 
                        "Are your data already log-transformed ?", 
                        choices=c("yes (they stay unchanged)" = "yes", 
                                  "no (they wil be automatically transformed)"="no"), 
                        selected="no")
          ,br()
          ,checkboxInput(self$ns("replaceAllZeros"), 
                         "Replace all 0 and NaN by NA", 
                         value= TRUE)
        )
      })
      
      
      
      
      
      ############ Read text file to be imported ######################
      observeEvent(c(input$file1,input$XLSsheets),{
        req(input$file1)
        req(!(GetExtension(input$file1$name) %in% c('xls', 'xlsx')))

        
        
        authorizedExts <- c("txt", "csv", "tsv","xls","xlsx")
        if( is.na(match(GetExtension(input$file1$name), authorizedExts))) {
          shinyjs::info("Warning : this file is not a text nor an Excel file ! 
                  Please choose another one.")
        }
        else {
          #ClearUI()
          #ClearMemory()
          ext <- GetExtension(input$file1$name)
          shinyjs::disable("file1")
          switch(ext,
                 txt = { rv$tab1 <- read.csv(input$file1$datapath,  header=TRUE, sep="\t", as.is=T)},
                 csv = { rv$tab1 <- read.csv(input$file1$datapath,  header=TRUE, sep=";", as.is=T)},
                 tsv = { rv$tab1 <- read.csv(input$file1$datapath,  header=TRUE, sep="\t", as.is=T)},
                 xls = { rv$tab1 <- readExcel(input$file1$datapath, ext, sheet=input$XLSsheets)},
                 xlsx = {rv$tab1 <- readExcel(input$file1$datapath, ext, sheet=input$XLSsheets)}
          )
        }
      })
      
      
      output$ManageXlsFiles <- renderUI({
        req(input$file1)
        req(GetExtension(input$file1$name) %in% c("xls","xlsx"))
        
       selectInput(self$ns("XLSsheets"), "sheets", 
                      choices = as.list(listSheets(input$file1$datapath),
                      width='200px')
       )
      })
    },
    
    
    SelectFile_ui = function(){
      
      tagList(
        fluidRow(
          column(width=2, modulePopoverUI(self$ns("modulePopover_convertChooseDatafile"))),
          column(width = 10, fileInput(self$ns("file1"), "", 
                                       multiple=FALSE, 
                                       accept=c(".txt", ".tsv", ".csv",".xls", ".xlsx")))
        ),
        uiOutput(self$ns("ManageXlsFiles")),
        br(),
        uiOutput(self$ns("ConvertOptions"))
      )

    },
    
    #------------------- Step 2: Choose data ID ----------------------------------------
    
    DataId_server = function(input, output){
      
      # observeEvent(input$btn_validate_Step1, ignoreInit = T, {
      #   # Add your stuff code here
      #   self$ValidateCurrentPos()
      # })

      
      output$id <- renderUI({
        req(rv$tab1)
        
        tagList(
          modulePopoverUI(self$ns("modulePopover_convertIdType")),
          selectInput(self$ns("idBox"), 
                      label = "", 
                      choices = setNames(nm = c("AutoID",colnames(rv$tab1)))
                      )
        )
        
      })
      
      
      output$warningNonUniqueID <- renderUI({
        req(input$idBox != 'AutoID')
        req(rv$tab1)
        
        t <- (length(as.data.frame(rv$tab1)[, input$idBox])
              == length(unique(as.data.frame(rv$tab1)[, input$idBox])))
        
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
      
      
      output$convertChooseProteinID_UI <- renderUI({
        req(rv$tab1)
        req(input$typeOfData != "protein")
        
         tagList(
          modulePopoverUI(self$ns("modulePopover_convertProteinID")),
          selectInput(self$ns("convert_proteinId"), 
                      "",
                      choices =  setNames(nm=c("",colnames(rv$tab1))), 
                      selected = character(0)
                      )
        )
      })

      
      output$helpTextDataID <- renderUI({
        req(input$typeOfData)
        
        plural.type <- ""
        switch(input$typeOfData,
               protein = plural.type <- "proteins",
               peptide = plural.type <- "peptides"
        )
        txt <- paste ("Please select among the columns of your data the one that 
                corresponds to a unique ID of the ", plural.type, ".", sep=" ")
        helpText(txt)
        
      })
      
      
      
      datasetID_Ok <- reactive({
        req(input$idBox)
        req(rv$tab1)
        
        if (input$idBox == "AutoID") {
          isIsOK <- TRUE
          } else {
            isIsOK <- (length(as.data.frame(rv$tab1)[, input$idBox])
                == length(unique(as.data.frame(rv$tab1)[, input$idBox])))
        }
        isIsOK
      })
      
      
      
      output$previewProteinID_UI <- renderUI({
        req(input$convert_proteinId != "")
        
        tagList(
          p(style="color: black;", 'Preview'),
          tableOutput(self$ns("previewProtID"))
        )
        
      })
      
      
      
      output$previewProtID <- renderTable(
        # req(input$convert_proteinId),
        head(rv$tab1[ ,input$convert_proteinId]),
        colnames = FALSE
      )
        
    },
    
    DataId_ui = function(){
      tagList(
        br(), br(),
        #uiOutput("helpTextDataID"),
        
        tags$div(
          tags$div( style="display:inline-block; vertical-align: top; padding-right: 100px;",
                    uiOutput(self$ns("id")),
                    uiOutput(self$ns("warningNonUniqueID"))
          ),
          tags$div( style="display:inline-block; vertical-align: top;",
                    uiOutput(self$ns("convertChooseProteinID_UI")),
                    uiOutput(self$ns("previewProteinID_UI"))
          )
        )
      )
    },
    
    
    #------------------- Step 2: Choose quantitative data ----------------------------------------
    
    QuantiData_ui = function(){
      # name <- 'Step2'
      # wellPanel(
      #   tagList(
      #     div(id=self$ns(name),
      #         div(style="display:inline-block; vertical-align: middle;padding-right: 20px;",
      #             tags$h3(name)),
      #         div(style="display:inline-block; vertical-align: middle;padding-right: 40px;",
      #             selectInput(self$ns('select2'), 'Select step 2',
      #                         choices = 1:5,
      #                         selected = 1,
      #                         width = '150px')),
      #         div(style="display:inline-block; vertical-align: middle;padding-right: 20px;",
      #             actionButton(self$ns(paste0('btn_validate_', name)), 
      #                          'Perform',
      #                          class = btn_success_color))
      #     )
      #   )
      # )
      tagList(
        shinyjs::useShinyjs(),
        fluidRow(
          column(width=4, shinyjs::hidden(
            checkboxInput(self$ns("selectIdent"), 
                          "Select columns for identification method", 
                          value = FALSE))
          ),
          column(width=4,uiOutput(self$ns("checkIdentificationTab"))),
          column(width = 4, shinyjs::hidden(
            div(id = self$ns('warning_neg_values'),
                p("Warning : Your original dataset may contain negative values",
                  "so that they cannot be logged. Please check back the dataset or", 
                  "the log option in the first tab."))
          )
          )
        ),
        fluidRow(
          column(width=4, uiOutput(self$ns("eData"),width = "400px")),
          column(width=8, shinyjs::hidden(
            DT::dataTableOutput(self$ns("x1"), width='500px'))
          )
        ),
        tags$script(HTML("Shiny.addCustomMessageHandler('unbind-DT', function(id) {
                                   Shiny.unbindAll($('#'+id).find('table').DataTable().table().node());
                                   })"))
      )
    },
    
    QuantiData_server = function(input, output){
      
      observeEvent(input$btn_validate_Step3, ignoreInit = T, {
        self$rv$dataIn <- AddItemToDataset(self$rv$dataIn, self$config$name)
        self$ValidateCurrentPos()
      })
      

      observe({
        #browser()
        shinyjs::toggle('warning_neg_values', 
                        condition = !is.null(input$eData.box) && length(which(rv$tab1[,input$eData.box] < 0)) > 0)
        shinyjs::toggle('selectIdent', condition = !is.null(rv$tab1))
        shinyjs::toggle('x1', condition = isTRUE(input$selectIdent))
      })
      
      output$eData <- renderUI({
        input$file1
        req(rv$tab1)
        
        tagList(
          modulePopoverUI(self$ns("modulePopover_convertDataQuanti")),
          selectInput(self$ns("eData.box"),
                      label = "",
                      choices = setNames(nm=colnames(rv$tab1)),
                      multiple = TRUE, 
                      width='200px',
                      size = 20,
                      selectize = FALSE)
        )
      })
      
      
      
      output$checkIdentificationTab <- renderUI({
        req(input$selectIdent == TRUE)
        
        #shinyValue("colForOriginValue_", length(input$eData.box))
        temp <- shinyValue("colForOriginValue_",
                           length(input$eData.box))
        
        if ((length(which(temp == "None")) == length(temp)))
        {
          img <- "images/Ok.png"
          txt <- "Correct"
        }  else {
          if (length(which(temp == "None")) > 0)
          {
            img <- "images/Problem.png"
            txt <- "The identification method is not appropriately defined for each sample."
          } else {
            if(length(temp) != length(unique(temp))){
              img <- "images/Problem.png"
              txt <- "There are duplicates in identification columns."
            }else { 
              img <- "images/Ok.png"
              txt <- "Correct"
            }
          }
        }
        tags$div(
          tags$div(
            tags$div(style="display:inline-block;",tags$img(src = img, height=25)),
            tags$div(style="display:inline-block;",tags$p(txt))
          )
        )
        
      })
      
      
      
      # reactive dataset
      quantiDataTable <- reactive({
        req(rv$tab1)
        req(input$eData.box)
        input$selectIdent
        
        #browser()
        
        df <- NULL
        session$sendCustomMessage('unbind-DT', 'x1')
         
        if (isTRUE(input$selectIdent)) {
          
          df <- data.frame(as.data.frame(input$eData.box),
                           shinyInput(selectInput,
                                      self$ns("colForOriginValue_"),
                                      nrow(as.data.frame(input$eData.box)),
                                      choices = setNames(nm=c("None",colnames(rv$tab1)))
                                      )
                           )
          colnames(df) <- c("Sample", "Identification method")
        } else {
          df <- data.frame(Sample = as.data.frame(input$eData.box))
          colnames(df) <- c("Sample")
        }
        df
      })
      
      
      
      output$x1 <- renderDataTable(
        quantiDataTable(),
        escape=FALSE,
        rownames = FALSE,
        extensions = c('Scroller', 'Buttons'),
        server=FALSE,
        selection='none', 
        class = 'compact',
        options=list(
          preDrawCallback=JS(
            'function() {
            Shiny.unbindAll(this.api().table().node());}'),
          drawCallback= JS(
            'function(settings) {
            Shiny.bindAll(this.api().table().node());}'),
          # rowCallback = JS("function(r,d) {$(r).attr('height', '10px')}"),
          dom = 'Bfrtip',
          autoWidth=TRUE,
          deferRender = TRUE,
          bLengthChange = FALSE,
          scrollX = 200,
          scrollY = 500,
          scroller = TRUE,
          ajax = list(url = dataTableAjax(session, quantiDataTable()))
          
        )
        
      )
      
      
      # observeEvent(shinyValue("colForOriginValue_",
      #                         nrow(as.data.frame(quantiDataTable()))),{})
      
      
      checkIdentificationMethod_Ok <- reactive({
        #req(input$selectIdent)
        res <- TRUE
        tmp <- NULL
        if (isTRUE(input$selectIdent)) {
          tmp <- shinyValue("colForOriginValue_",nrow(quantiDataTable()))
          if ((length(grep("None", tmp)) > 0)  || (sum(is.na(tmp)) > 0))
            res <- FALSE
        } 
        res
        
      })
      
    },
    
    
    #---------------------------------------------------------------------------------
    #------------------- Step 5: Build design UI----------------------------------------
    #---------------------------------------------------------------------------------
    
    BuildDesign_ui = function(){
      tagList(
        tags$p("If you do not know how to fill the experimental design, you can click
                                  on the '?' next to each design in the list that appear once the conditions 
                                  are checked or got to the ", 
               actionLink("linkToFaq1", "FAQ",style="background-color: white"), 
               " page."),
        fluidRow(
          column(width=6,tags$b("1 - Fill the \"Condition\" column to identify the conditions to compare.")),
          column(width=6,uiOutput("UI_checkConditions")  )
        ),
        fluidRow(
          column(width=6,uiOutput("UI_hierarchicalExp")),
          column(width=6,uiOutput("checkDesign") )
        ),
        hr(),
        selectInput("convert_reorder", "Order by conditions ?",
                    choices=c("No"="No", "Yes"="Yes"),
                    width="100px"),
        tags$div(
          
          tags$div(style="display:inline-block; vertical-align: top;",
                   uiOutput("viewDesign",width="100%")
          ),
          tags$div(style="display:inline-block; vertical-align: top;",
                   shinyjs::hidden(div(id = "showExamples", uiOutput("designExamples") ))
          )
        )
        
      )
    },
    
    
    #---------------------------------------------------------------------------------
    #--------------------------- Step 5: Build design UI -----------------------------
    #---------------------------------------------------------------------------------
    
    BuildDesign_server = function(){
      
      observeEvent(req(input$linkToFaq1), {
        updateTabsetPanel(session, 'navPage', "faqTab")
      })
      
      
      
      color_renderer <- reactive({
        rv$hot$Condition
        
        if (length(which(rv$hot$Condition==""))==0)
          uniqueConds <- unique(rv$hot$Condition)
        else
          uniqueConds <- unique(rv$hot$Condition[-which(rv$hot$Condition=="")])
        
        nUniqueConds <- length(uniqueConds)
        pal <- DAPAR2::ExtendPalette(nUniqueConds)
        
        txt <- "function (instance, td, row, col, prop, value, cellProperties) {
  Handsontable.renderers.TextRenderer.apply(this, arguments);"
        c <- 1
        for (i in 1:length(rv$hot$Condition)){
          if (rv$hot$Condition[i] != "")
            
            txt <- paste0(txt, "if(row==",(i-1)," && col==",c, ") {td.style.background = '", 
                          pal[which(rv$hot$Condition[i] == uniqueConds)],"';}")
        }
        txt <- paste0(txt,"}")
        
        return (txt)
      })
      
      
      
      
      
      #----------------------------------------------------------
      observeEvent(input$btn_checkConds,{
        input$file1
        input$convert_reorder
        
        if (length(grep("Bio.Rep", colnames(rv$hot))) > 0)  { return(NULL)}
        
        if (input$convert_reorder == "Yes") {
          rv$newOrder <- order(rv$hot$Condition)
          rv$hot <- rv$hot[rv$newOrder,]
        }
        
        rv$conditionsChecked <- DAPAR2::check.conditions(rv$hot$Condition)
      })
      
      
      
      #----------------------------------------------------------
      observeEvent(input$eData.box,{
        rv$hot  <- data.frame(Sample.name = as.character(input$eData.box),
                              Condition = rep("", length(input$eData.box)),
                              stringsAsFactors = FALSE)
      })
      
      #-------------------------------------------------------------
      output$hot <- renderRHandsontable({
        rv$hot
        input$chooseExpDesign
        
        if (is.null(rv$hot)){
          rv$hot  <- data.frame(Sample.name = as.character(input$eData.box),
                                Condition = rep("",length(input$eData.box)),
                                stringsAsFactors = FALSE)
        }
        
        hot <- rhandsontable::rhandsontable(rv$hot,
                                            rowHeaders=NULL, 
                                            fillHandle = list(direction='vertical', 
                                                              autoInsertRow=FALSE,
                                                              maxRows=nrow(rv$hot))
                                            ) %>%
          rhandsontable::hot_rows(rowHeights = 30) %>%
          rhandsontable::hot_context_menu(allowRowEdit = TRUE, 
                                          allowColEdit = FALSE,
                                          allowInsertRow = FALSE,
                                          allowInsertColumn = FALSE,
                                          allowRemoveRow = TRUE,
                                          allowRemoveColumn = FALSE,
                                          autoInsertRow=FALSE     ) %>%
          rhandsontable:: hot_cols(renderer = color_renderer()) %>%
          rhandsontable::hot_col(col = "Sample.name", readOnly = TRUE)
        
        if (!is.null(input$chooseExpDesign)) {
          switch(input$chooseExpDesign,
                 FlatDesign = {
                   if ("Bio.Rep" %in% colnames(rv$hot))
                     hot <- hot %>% rhandsontable::hot_col(col = "Bio.Rep", readOnly = TRUE)
                 },
                 twoLevelsDesign = {
                   if ("Tech.Rep" %in% colnames(rv$hot))
                     hot <- hot %>% rhandsontable::hot_col(col =  "Tech.Rep", readOnly = TRUE)
                 } ,
                 threeLevelsDesign = {
                   if ("Analyt.Rep" %in% colnames(rv$hot))
                     hot <- hot %>% rhandsontable::hot_col(col = "Analyt.Rep", readOnly = TRUE)
                 }
          )
        }
        hot
      })
      
      
      #----------------------------------------------------------
      output$UI_checkConditions  <- renderUI({
        
        req(rv$hot)
        rv$conditionsChecked
        input$convert_reorder
        
        if ((sum(rv$hot$Condition == "")==0) && (input$convert_reorder != "None")){
          tags$div(
            tags$div(style="display:inline-block;",
                     actionButton(self$ns("btn_checkConds"),
                                  "Check conditions",
                                  class = actionBtnClass)
            ),
            
            tags$div(style="display:inline-block;",
                     if(!is.null(rv$conditionsChecked)){
                       
                       if (isTRUE(rv$conditionsChecked$valid)){
                         img <- "images/Ok.png"
                         txt <- "Correct conditions"
                       }else {
                         img <- "images/Problem.png"
                         txt <- "Invalid conditions"
                       }
                       tagList(
                         tags$div(
                           tags$div(style="display:inline-block;",tags$img(src = img, height=25)),
                           tags$div(style="display:inline-block;",tags$p(txt))
                         ),
                         if(!isTRUE(rv$conditionsChecked$valid)){
                           tags$p(rv$conditionsChecked$warn)
                         }
                       )
                     }
            )
          )
        } else {
          tagList(
            br(),
            br(),
            br(),
            br()
          )
          
        }
      })
      
      
      
      #------------------------------------------------------------------------------
      output$UI_hierarchicalExp <- renderUI({
        req(rv$conditionsChecked)
        req(isTRUE(rv$conditionsChecked$valid))
        
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
              )
            ),
          radioButtons(self$ns("chooseExpDesign"), 
                       "",
                       choices = c("Flat design (automatic)" = "FlatDesign" ,
                                   "Two levels design (complete Bio.Rep column)" = "twoLevelsDesign" ,
                                     "Three levels design (complete Bio.Rep and Tech.Rep columns)" = "threeLevelsDesign" ),
                       selected=character(0))
          )
        })
      
      output$viewDesign <- renderUI({
        req(!isTRUE(rv$designSaved))
        
        tagList(
          h4("Design"),
          rHandsontableOutput(self$ns("hot"))
        )
      })
      
      
      callModule(moduleDesignExample,"buildDesignExampleThree", 3)
      callModule(moduleDesignExample,"buildDesignExampleTwo", 2)
      
      
      #------------------------------------------------------------------------------
      output$designExamples <- renderUI({
        input$chooseExpDesign
        
        switch(input$chooseExpDesign,
               FlatDesign = {
                 tags$p("There is nothing to do for the flat design: the 'Bio.Rep' column is already filled.")
                 },
               twoLevelsDesign =  {
                 tagList(
                   h4("Example for a 2-levels design"),
                   moduleDesignExampleUI(self$ns("buildDesignExampleTwo"))
                   )
                 },
               threeLevelsDesign =  {
                 tagList(
                   h4("Example for a 3-levels design"),
                   moduleDesignExampleUI(self$ns("buildDesignExampleThree"))
                 )
               }
              )
      })
      
      
      #------------------------------------------------------------------------------
      observe({
        shinyjs::onclick("btn_helpDesign",{
          shinyjs::toggle(id = "showExamples", anim = TRUE)}
        )
      })
      
      #------------------------------------------------------------------------------
      observeEvent(input$chooseExpDesign, {
        rv$hot
        rv$designChecked <- NULL
        
        switch(input$chooseExpDesign,
               FlatDesign = {
                 rv$hot  <- data.frame(rv$hot[,1:2],
                                       Bio.Rep = seq(1:nrow(rv$hot)),
                                       stringsAsFactors = FALSE)
               },
               twoLevelsDesign = {
                 rv$hot  <- data.frame(rv$hot[,1:2],Bio.Rep = rep("",nrow(rv$hot)),
                                       Tech.Rep = seq(1:nrow(rv$hot)),
                                       stringsAsFactors = FALSE)
               },
               threeLevelsDesign = {
                 rv$hot  <- data.frame(rv$hot[,1:2],
                                       Bio.Rep = rep("",nrow(rv$hot)),
                                       Tech.Rep = rep("",nrow(rv$hot)),
                                       Analyt.Rep = seq(1:nrow(rv$hot)),
                                       stringsAsFactors = FALSE)
               }
        )
      })
      
      
      
      
      #------------------------------------------------------------------------------
      observeEvent(input$hot,{ 
        rv$hot <-  hot_to_r(input$hot)
        })
      
      
      
      #------------------------------------------------------------------------------
      observeEvent(input$btn_checkDesign,{ 
        rv$designChecked <- DAPAR2::check.design(rv$hot)
        })
      
      #------------------------------------------------------------------------------
      output$checkDesign <- renderUI({
        req(input$chooseExpDesign)
        rv$designChecked
        req(rv$conditionsChecked)
        req(isTRUE(rv$conditionsChecked$valid))
        
        switch(isolate({input$chooseExpDesign}),
               FlatDesign = {},
               twoLevelsDesign = { 
                 if (sum(rv$hot$Bio.Rep == "") > 0) 
                   return(NULL)
                 },
               threeLevelsDesign = {
                 if ((sum(rv$hot$Bio.Rep == "")+sum(rv$hot$Tech.Rep == "")) > 0)
                   return(NULL)
                 }
        )
        
        
        tags$div(
          tags$div(
            style="display:inline-block;",
            actionButton(self$ns("btn_checkDesign"),
                         "Check design", 
                         class = actionBtnClass)
          ),
          
          tags$div(
            style="display:inline-block;",
            if(!is.null(rv$designChecked)){
              
              if (isTRUE(rv$designChecked$valid)){
                shinyjs::enable("createMSnsetButton")
                img <- "images/Ok.png"
                txt <- "Correct design"
              }else {
                img <- "images/Problem.png"
                txt <- "Invalid design"}
              tagList(
                tags$div(
                  tags$div(style="display:inline-block;",tags$img(src = img, height=25)),
                  tags$div(style="display:inline-block;",tags$p(txt))
                ),
                if(!isTRUE(rv$designChecked$valid)){
                  shinyjs::disable("createMSnsetButton")
                  tags$p(rv$designChecked$warn)
                } else {
                  shinyjs::enable("createMSnsetButton")
                }
              )
            } else {
              shinyjs::disable("createMSnsetButton")
            }
            )
          )
        })
      },
    
    #---------------------------------------------------------------------------------
    #----------------------------- Step 6: Convert UI --------------------------------
    #---------------------------------------------------------------------------------
    Convert_ui = function(){
      tagList(
        br(), br(),
        moduleStaticDataTableUI(self$ns("overview_convertData")),
        uiOutput(self$ns("conversionDone")),
        p("Once the 'Load' button (above) clicked, you will be automatically redirected to Prostar home page. 
        The dataset will be accessible within Prostar interface and processing menus will be enabled. 
        However, all importing functions ('Open MSnset', 'Demo data' and 'Convert data') will be disabled 
        (because successive dataset loading can make Prostar unstable). To work on another dataset, use first 
        the 'Reload Prostar' functionality from the 'Dataset manager' menu: it will make Prostar restart 
          with a fresh R session where import functions are enabled.")
        )
    }
    
    
    #---------------------------------------------------------------------------------
    #--------------------------- Step 6: Convert server ------------------------------
    #---------------------------------------------------------------------------------
    
    Convert_server = function(){
      
      
      output$convertFinalStep <- renderUI({
        req(rv$designChecked)
        req(rv$designChecked$valid)
      
        tagList(
          uiOutput(self$ns("checkAll_convert"), width="50"),
          htmlOutput(self$ns("msgAlertCreateMSnset")),
          hr(),
          textInput(self$ns("filenameToCreate"),"Enter the name of the study"),
          actionButton(self$ns("createMSnsetButton"),"Convert data", class = actionBtnClass),
          uiOutput(self$ns("warningCreateMSnset"))
        )
      })
      
      
      output$conversionDone <- renderUI({
        req(rv$current.obj)
         
        h4("The conversion is done. Your dataset has been automatically loaded 
        in memory. Now, you can switch to the Descriptive statistics panel to 
           vizualize your data.")
        
      })
      
      output$warningCreateMSnset <- renderUI({
        if (isTRUE(input$selectIdent)){
          colNamesForOriginofValues <- shinyValue("colForOriginValue_",
                                                  nrow(quantiDataTable()))
          
          if (length(which(colNamesForOriginofValues == "None")) >0){
            text <- "<font color=\"red\"> Warning: The MSnset cannot be created because the identification 
            method are not fully filled.  <br>"
            HTML(text)
          }
        }
      })

      #######################################
      observeEvent(input$createMSnsetButton, ignoreInit = TRUE,{

        colNamesForOriginofValues <- NULL
        if (isTRUE(input$selectIdent)) {
          colNamesForOriginofValues <- shinyValue("colForOriginValue_", 
                                                  nrow(quantiDataTable())
                                                  )
          if (length(which(colNamesForOriginofValues == "None")) >0){ return (NULL)   }
        } 
        
        isolate({
          result = tryCatch(
            {
              ext <- GetExtension(input$file1$name)
              txtTab <-  paste("tab1 <- read.csv(\"", input$file1$name,
                               "\",header=TRUE, sep=\"\t\", as.is=T)",  sep="")
              txtXls <-  paste("tab1 <- read.xlsx(",input$file1$name,
                               ",sheet=", input$XLSsheets,")",sep="")
              switch(ext,
                     txt = writeToCommandLogFile(txtTab),
                     csv = writeToCommandLogFile(txtTab),
                     tsv = writeToCommandLogFile(txtTab),
                     xls= writeToCommandLogFile(txtXls),
                     xlsx = writeToCommandLogFile(txtXls)
              )
              
              input$filenameToCreate
              rv$tab1
              
              tmp.eData.box <- input$eData.box
              indexForEData <- match(tmp.eData.box, colnames(rv$tab1))
              if (!is.null(rv$newOrder)){
                tmp.eData.box <- tmp.eData.box[rv$newOrder]
                indexForEData <- indexForEData[rv$newOrder]
              }
              
              indexForFData <- seq(1, ncol(rv$tab1))[-indexForEData]
              
              indexForIDBox <- NULL
              if (input$idBox !="AutoID") {
                indexForIDBox <- match(input$idBox, colnames(rv$tab1))
              }
              
              
              metadata <- hot_to_r(input$hot)
              logData <- (input$checkDataLogged == "no")
              
              
              indexForOriginOfValue <- NULL
              if (!is.null(colNamesForOriginofValues) && (length(grep("None", colNamesForOriginofValues))==0)  && (sum(is.na(colNamesForOriginofValues)) == 0)){
                for (i in 1:length(tmp.eData.box)){
                  indexForOriginOfValue <- c(indexForOriginOfValue, which(colnames(rv$tab1) == input[[paste0("colForOriginValue_", i)]]))
                }
              }
              
              
              versions <- list(Prostar_Version = 
                                 installed.packages(lib.loc = Prostar.loc)["Prostar","Version"],
                               DAPAR_Version = 
                                 installed.packages(lib.loc = DAPAR.loc)["DAPAR","Version"]
              )
              options(digits=15)
              
              tmp <- DAPAR::createMSnset(file = rv$tab1, 
                                         metadata = metadata, 
                                         indExpData = indexForEData, 
                                         indFData = indexForFData, 
                                         indiceID = indexForIDBox,
                                         indexForOriginOfValue = indexForOriginOfValue,
                                         logData = logData, 
                                         replaceZeros = input$replaceAllZeros,
                                         pep_prot_data = input$typeOfData,
                                         proteinId =  gsub(".", "_", input$convert_proteinId, fixed=TRUE),
                                         versions = versions
              )
              #ClearUI()
             # ClearMemory()
              rv$current.obj <- tmp
              
              rv$current.obj.name <- input$filenameToCreate
              rv$indexNA <- which(is.na(exprs(rv$current.obj)))
              
              l.params <- list(filename = input$filenameToCreate)
              
              loadObjectInMemoryFromConverter()
              
              updateTabsetPanel(session, "tabImport", selected = "Convert")
            },
            # warning = function(w) {
            #   if (conditionMessage(w) %in% c("NaNs produced", "production de NaN")){
            #     shinyjs::info(paste("Warning : Your original dataset may contain negative values",
            #                         "so that they cannot be logged. Please check back the dataset or", 
            #                         "the log option in the first tab.",
            #                         sep=" "))
            #   } else {
            #     shinyjs::info(paste("Warning in CreateMSnSet",":",
            #                         conditionMessage(w), 
            #                         sep=" "))
            #   }
            # }, 
            error = function(e) {
              shinyjs::info(paste("Error :","CreateMSnSet",":",
                                  conditionMessage(e), 
                                  sep=" "))
            }, finally = {
              #cleanup-code 
            })
          
          
          
        })
    })
      
    }
  )
)