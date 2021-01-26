# TODO Continue the adaptation of convert module but need to update DAPAR2 before

#' @author Samuel Wieczorek
#' 
#' @export
#' 
#' @import rhandsontable
#' @import DT
#' 
Convert = R6Class(
  "Convert",
  inherit = Magellan::Process,
  private = list(
    .config = list(name = 'Convert',
                   steps = c('Description', 'SelectFile', 'QuantiData', 'Design', 'Convert'),
                   mandatory = c(T, T, T, T, T)
    )
  ),

  
  public = list(

    Global_server = function(session, input){
      
      mod_popover_for_help_server("modulePopover_convertChooseDatafile", 
                 data = list(title = h4('Data file'), 
                                      content="Select one (.txt, .csv, .tsv, .xls, .xlsx) file."))
      
      mod_popover_for_help_server("modulePopover_convertIdType", 
                 data = list(title = h4('ID definition'), 
                                      content="If you choose the automatic ID, Prostar will build an index."))
      
      
      
      mod_popover_for_help_server("modulePopover_convertProteinID", 
                 data = list(title = HTML(paste0("<strong><font size=\"4\">Select protein IDs</font></strong>")), 
                                      content="Select the column containing the parent protein IDs."))
      
      
      mod_popover_for_help_server("modulePopover_convertDataQuanti", 
                 data = list(title = h4('Quantitative data'), 
                                      content="Select the columns that are quantitation values by clicking in the field below."))
      
      mod_format_DT_server("overview_convertData", table2show=reactive({GetDatasetOverview()}))
      
      GetDatasetOverview <- reactive({
        req(self$self$process.var$dataIn)
        
        
        columns <- c("Number of samples",
                     "Number of conditions",
                     "Number of lines", 
                     "Number of missing values", 
                     "% of missing values", 
                     "Number of empty lines")
        
        do <- data.frame(Definition = columns,
                         Value = rep(0,length(columns)))
        last.se <- self$self$process.var$dataIn[[ength(self$self$process.var$dataIn)]]
        intensities.tab <- assay(last.se)
        NA.count <- length(which(is.na(intensities.tab)==TRUE))
        pourcentage <- 100 * round(NA.count/(ncol(intensities.tab)*nrow(intensities.tab)), digits=4)
        nb.empty.lines <- sum(apply(
          is.na(as.matrix(intensities.tab)), 1, all))
        
        
        val <- c(ncol((intensities.tab),
                 length(colData(last.se)$Condition)),
                 nrow(intensities.tab),
                 NA.count,
                 pourcentage,
                 nb.empty.lines)
        do$Value <- val
        
        do
      })
      
      
    },
    
    GetExtension = function(filename){ 
      if (is.null(filename))
        ""
      else {
        vec <- unlist(strsplit(filename, split ='.', fixed=T))
        vec[length(vec)]
      }
      },
    
    #------------------- Step 1: description ----------------------------------------
    Description_server = function(session, input, output){
      
      observeEvent(input$btn_validate_Description, ignoreInit = T, ignoreNULL=T, {
        cat(paste0(class(self)[1], "::observeEvent(input$btn_validate_Description from - ", self$id, '\n'))
        private$InitializeDataIn()
        self$ValidateCurrentPos()
      })
      
    },
    
    
    Description_ui = function(){
      req(self$config$name)
     # browser()
      wellPanel(
        tagList(
          includeMarkdown( system.file("app/md", paste0(self$config$name, ".md"), package="Prostar.2.0")),
          #includeMarkdown( paste0("../../../inst/app/md", self$config$name, ".md")),
          shinyjs::disabled(actionButton(self$ns('btn_validate_Description'),
                       paste0('Start ', self$config$name),
                       class = 'btn-success')
          )
        )
      )
    },
    
    


    #------------------- Step 2: Select file ----------------------------------------

    SelectFile_server = function(session, input, output){


      # output$ConvertOptions <- renderUI({
      #   req(self$process.var$file2convert)
      # 
      #   tagList(
      #     radioButtons(self$ns("typeOfData"),
      #                  "Is it a peptide or protein dataset ?",
      #                  choices=c("peptide dataset" = "peptide",
      #                            "protein dataset" = "protein")
      #     )
      # 
      #     ,radioButtons(self$ns("checkDataLogged"),
      #                   "Are your data already log-transformed ?",
      #                   choices=c("yes (they stay unchanged)" = "yes",
      #                             "no (they wil be automatically transformed)"="no"),
      #                   selected="no")
      #     ,br()
      #     ,checkboxInput(self$ns("replaceAllZeros"),
      #                    "Replace all 0 and NaN by NA",
      #                    value= TRUE)
      #   )
      # })
      
      observe({
        shinyjs::toggle('btn_validate_SelectFile', condition = !is.null(self$process.var$rawData2convert))
        })

      
      observeEvent(input$btn_validate_SelectFile, ignoreInit = T, {
        self$ValidateCurrentPos()
      })
      
      
      observeEvent(input$file2convert, { 
        self$process.var$file2convert <- input$file2convert}, 
        priority = 1000)
      

      ############ Read text file to be imported ######################
      observeEvent(req(self$process.var$file2convert), {
        input$XLSsheets
        authorizedExts <- c("txt", "csv", "tsv","xls","xlsx")
        if( is.na(match(self$GetExtension(self$process.var$file2convert$name), authorizedExts))) {
          shinyjs::info("Warning : this file is not a text nor an Excel file !
                  Please choose another one.")
          shinyjs::reset('file2convert')
        }
        else {
          #ClearUI()
          #ClearMemory()
          shinyjs::toggle('Div_ConvertOptions', condition = !is.null(self$process.var$file2convert))
          ext <- self$GetExtension(self$process.var$file2convert$name)
          shinyjs::disable("file2convert")
          switch(ext,
                 txt = { self$process.var$rawData2convert <- read.csv(self$process.var$file2convert$datapath,  header=TRUE, sep="\t", as.is=T)},
                 csv = { self$process.var$rawData2convert <- read.csv(self$process.var$file2convert$datapath,  header=TRUE, sep=";", as.is=T)},
                 tsv = { self$process.var$rawData2convert <- read.csv(self$process.var$file2convert$datapath,  header=TRUE, sep="\t", as.is=T)},
                 xls = { self$process.var$rawData2convert <- readExcel(self$process.var$file2convert$datapath, ext, sheet=input$XLSsheets)},
                 xlsx = {self$process.var$rawData2convert <- readExcel(self$process.var$file2convert$datapath, ext, sheet=input$XLSsheets)}
          )
        }
      })


     


      
      output$warningNonUniqueID <- renderUI({
        #req(input$choose_global_id_ui != 'AutoID')
        #req(self$self$process.var$rawData2convert)
        
        req(input$global_id)
        req(self$process.var$rawData2convert)
        
        if (input$global_id == "AutoID")
          idIsOk <- TRUE
        else {
          colid <- as.data.frame(self$process.var$rawData2convert)[ ,input$global_id]
          idIsOk <- length(colid) == length(unique(colid))
        }
        #browser()
        # if (!idIsOk)
        #   text <- "<img src='images/Problem.png' height='24'></img><font color='red'>
        # Warning ! Your ID contains duplicate data.
        # Please choose another one."
        #   else
        #     text <- "<img src='images/Ok.png' height='24'></img>"
        # HTML(text)
        if (!idIsOk) {
          div(
            tags$div( style="display:inline-block; vertical-align: top;",
                      img(src=base64enc::dataURI(file=system.file('app/www/images', 
                                                                  'Problem.png', package='Prostar.2.0'), 
                                                 mime="image/png")
                      )
            ),
            tags$div( style="display:inline-block; vertical-align: top;",
                      p(style="font color='red'",
                        "Warning ! The ID you have choosen contains duplicate data.
                        Please choose another one.")
            )
          )
        } else {
          div(
            tags$div( style="display:inline-block; vertical-align: top;",
                      img(src=base64enc::dataURI(file=system.file('app/www/images', 
                                                                  'Ok.png', package='Prostar.2.0'), 
                                                 mime="image/png")
                      )
            ),
            tags$div( style="display:inline-block; vertical-align: top;",
                      p(style="font color='black'",
                        "ID ok")
            )
          )
        }
      })
      
      output$ManageXlsFiles <- renderUI({
        req(self$process.var$file2convert)
        req(self$GetExtension(self$process.var$file2convert$name) %in% c("xls","xlsx"))
        
        selectInput(self$ns("XLSsheets"), "sheets",
                    choices = as.list(listSheets(self$process.var$file2convert$datapath),
                                      width='200px'))
      })
      
      observe({
        self$process.var$rawData2convert
        input$typeOfData
        self$process.var$file2convert
        
        if (!is.null(self$process.var$rawData2convert)){
          updateSelectInput(session, 'global_id', choices = setNames(nm=c("AutoID",colnames(self$process.var$rawData2convert))))
          updateSelectInput(session, 'protein_id', choices = setNames(nm=c("",colnames(self$process.var$rawData2convert))))
        }
        shinyjs::toggle('test', condition = input$typeOfData == 'peptide')
        
        shinyjs::toggle('XLSsheets', condition = self$GetExtension(self$process.var$file2convert$name) %in% c("xls","xlsx"))
        
        # Show/hide xls manager UI
        if (self$GetExtension(self$process.var$file2convert$name) %in% c("xls","xlsx")){
          updateSelectInput(session, 'XLSsheets', choices = as.list(listSheets(self$process.var$file2convert$datapath)))
        }
          
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
      
      
      output$preview_protein_id_ui <- renderUI({
        req(input$protein_id != "")
        tagList(
          p(style="color: black;", 'Preview'),
          tableOutput(self$ns("preview_protein_id"))
        )
      })
      
      output$preview_protein_id <- renderTable(
        head(self$process.var$rawData2convert[ ,input$protein_id]),
        colnames = FALSE
      )
    },


    SelectFile_ui = function(){

      tagList(
        fluidRow(
          column(width=2, 
                 mod_popover_for_help_ui(self$ns("modulePopover_convertChooseDatafile"))),
          column(width = 10, 
                 fileInput(self$ns("file2convert"), "",
                           multiple=FALSE,
                           accept=c(".txt", ".tsv", ".csv",".xls", ".xlsx")
                           )
                 )
        ),
        shinyjs::hidden(
          selectInput(self$ns("XLSsheets"), "sheets",
                    choices = NULL,
                    width='200px')
          ),

        #shinyjs::hidden(
            div(id = self$ns('Div_ConvertOptions'),
                radioButtons(self$ns("typeOfData"),
                             "Is it a peptide or protein dataset ?",
                             choices=c("protein dataset" = "protein",
                                       "peptide dataset" = "peptide"
                                       )
                             ),
                radioButtons(self$ns("checkDataLogged"),
                             "Are your data already log-transformed ?",
                             choices=c("yes (they stay unchanged)" = "yes",
                                       "no (they wil be automatically transformed)"="no"),
                             selected="no"
                             ),
                checkboxInput(self$ns("replaceAllZeros"),
                              "Replace all 0 and NaN by NA",
                              value= TRUE)
               # )
            ),

        tags$div(
          tags$div( style="display:inline-block; vertical-align: top; padding-right: 100px;",
                    mod_popover_for_help_ui(self$ns("modulePopover_convertIdType")),
                    selectInput(self$ns("global_id"),
                                label = "",
                                choices = NULL
                                #choices = self$process.var$choices_global_id_ui
                    )
          ),
          tags$div( style="display:inline-block; vertical-align: top; padding-right: 100px;",
                    uiOutput(self$ns("warningNonUniqueID"))
          ),
          shinyjs::hidden(
            tags$div( id = self$ns('test'),
                    style="display:inline-block; vertical-align: top;",
                    tagList(
                      mod_popover_for_help_ui(self$ns("modulePopover_convertProteinID")),
                      selectInput(self$ns("protein_id"),
                                  "",
                                  choices = NULL,
                                  #choices =  self$process.var$choices_protein_id_ui,
                                  selected = character(0)
                      ),
                      uiOutput(self$ns("preview_protein_id_ui"))
                    )
          )
          )
      ),
          shinyjs::disabled(actionButton(self$ns('btn_validate_SelectFile'), 
                                         'Validate',
                                         class = btn_success_color)
          )
        )
    },

    #------------------- Step 3: Choose quantitative data ----------------------------------------

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
          column(width=4,
                 uiOutput(self$ns("checkIdentificationTab"))),
          column(width = 4, 
                 shinyjs::hidden(
                   div(id = self$ns('warning_neg_values'),
                       p("Warning : Your original dataset may contain negative values",
                         "so that they cannot be logged. Please check back the dataset or",
                         "the log option in the first tab.")
                       )
                   )
                 )
          ),
        fluidRow(
          column(width=4, 
                 uiOutput(self$ns("choose_quanti_data_ui"),
                          width = "400px"
                          )
                 ),
          column(width=8, 
                 #shinyjs::hidden(
                    DT::DTOutput(self$ns("selectIdOriginDT"), 
                                        width='500px')
                 # DT::DTOutput(self$ns("toto"), 
                 #              width='500px')
                   )
         # )
        ),
        tags$script(HTML("Shiny.addCustomMessageHandler('unbind-DT', function(id) {
        Shiny.unbindAll($('#'+id).find('table').DataTable().table().node());
                         })")),
        shinyjs::disabled(
          actionButton(self$ns('btn_validate_QuantiData'),
                       'Validate',
                       class = btn_success_color
                       )
          )
      )
    },

    
    # --------------------------------------------------------------------------
    # --------------------------- Quanti data server ---------------------------
    # --------------------------------------------------------------------------
    QuantiData_server = function(session, input, output){

      shinyOutput <- function(FUN,id,num,...) {
        inputs <- character(num)
        for (i in seq_len(num)) {
          inputs[i] <- as.character(FUN(paste0(id,i),label=NULL,...))
        }
        inputs
      }
      
      
      # function for dynamic inputs in DT
      shinyInput <- function(FUN, id, num,...) {
        inputs <- character(num)
        for (i in seq_len(num)) {
          inputs[i] <- as.character(FUN(paste0(id,i),label=NULL,...))
        }
        inputs
      }
      
      
      # function to read DT inputs
      shinyValue <- function(id, num) {
        unlist(lapply(seq_len(num),function(i) {
          value <- input[[paste0(id,i)]]
          if (is.null(value)) NA else value
        }))
      }
      
      
      observeEvent(input$btn_validate_QuantiData, ignoreInit = T, {
        self$ValidateCurrentPos()
      })


      observe({
        #browser()
        shinyjs::toggle('warning_neg_values',
                        condition = !is.null(input$quanti_data) && length(which(self$process.var$rawData2convert[,input$quanti_data] < 0)) > 0)
        shinyjs::toggle('selectIdent', condition = !is.null(self$process.var$rawData2convert))
        shinyjs::toggle('selectIdOriginDT', condition = isTRUE(input$selectIdent))
      })

      output$choose_quanti_data_ui <- renderUI({
        self$process.var$file2convert
        req(self$process.var$rawData2convert)

        tagList(
          mod_popover_for_help_ui(self$ns("modulePopover_convertDataQuanti")),
          selectInput(self$ns("quanti_data"),
                      label = "",
                      choices = setNames(nm=colnames(self$process.var$rawData2convert)),
                      multiple = TRUE,
                      width='200px',
                      size = 20,
                      selectize = FALSE)
        )
      })



      output$checkIdentificationTab <- renderUI({
        req(input$selectIdent == TRUE)

        #shinyValue("colForOriginValue_", length(input$quanti_data))
        temp <- shinyValue("colForOriginValue_",
                           length(input$quanti_data))

        if ((length(which(temp == "None")) == length(temp)))
        {
          imageName <- 'Ok.png'
          txt <- "Correct"
        }  else {
          if (length(which(temp == "None")) > 0)
          {
            imageName <- 'Problem.png'
            txt <- "The identification method is not appropriately defined for each sample."
          } else {
            if(length(temp) != length(unique(temp))){
              imageName <- 'Problem.png'
              txt <- "There are duplicates in identification columns."
            }else {
              imageName <- 'Ok.png'
              txt <- "Correct"
            }
          }
        }
          tags$div(
            tags$div(style="display:inline-block;",
                     img(src=base64enc::dataURI(file=system.file('app/www/images',
                                                                                imageName,
                                                                                package='Prostar.2.0'), 
                                                               mime="image/png"),
                              height=25)),
            tags$div(style="display:inline-block;",tags$p(txt))
          )

      })



      # reactive dataset
      quantiDataTable <- reactive({
        req(self$process.var$rawData2convert)
        req(input$quanti_data)
        input$selectIdent
        df <- NULL
        session$sendCustomMessage('unbind-DT', 'selectIdOriginDT')

        if (isTRUE(input$selectIdent)) {

          df <- data.frame(as.data.frame(input$quanti_data),
                           shinyInput(selectInput,
                                      self$ns("colForOriginValue_"),
                                      nrow(as.data.frame(input$quanti_data)),
                                      choices = setNames(nm=c("None",colnames(self$process.var$rawData2convert)))
                                      )
                           )
          colnames(df) <- c("Sample", "Identification method")
        } else {
          df <- data.frame(Sample = as.data.frame(input$quanti_data))
          colnames(df) <- c("Sample")
        }
        df
      })

      output$selectIdOriginDT <- DT::renderDT(
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
          dom = 'Brtip',
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
          tmp <- shinyValue("colForOriginValue_", nrow(quantiDataTable()))
          if ((length(grep("None", tmp)) > 0)  || (sum(is.na(tmp)) > 0))
            res <- FALSE
        }
        res

      })

    },


    #---------------------------------------------------------------------------------
    #------------------- Step 5: Build design UI----------------------------------------
    #---------------------------------------------------------------------------------

    Design_ui = function(){
      tagList(
        shinyjs::hidden(
          actionButton(self$ns('btn_validate_Design'),
                       'Validate',
                       class = btn_success_color
                       )
          ),
        tags$p("If you do not know how to fill the experimental design, you can click
                on the '?' next to each design in the list that appear once the conditions
                are checked or go to the ",
               actionLink("linkToFaq1", "FAQ",style="background-color: white"),
               " page."),
        fluidRow(
          column(width=6,
                 tags$b("1 - Fill the \"Condition\" column to identify the conditions to compare.")
                 ),
          column(width=6,
                 tags$div(
                   tags$div(style="display:inline-block;",
                          shinyjs::hidden(
                            actionButton(self$ns("btn_checkConds"),
                                         "Check conditions",
                                         class = actionBtnClass)
                          )
                 ),
                 tags$div(style="display:inline-block;",
                          uiOutput(self$ns("UI_checkConditions"))
                          
                 )
                 )
        )
        ),
        fluidRow(
          column(width=6,
                 shinyjs::hidden(
                   div(id = self$ns('UI_hierarchicalExp'),
                       div(
                         style="display:inline-block; vertical-align: middle;",
                         tags$b("2 - Choose the type of experimental design and complete it accordingly"),
                         actionLink(self$ns("btn_helpDesign"), tags$sup("?"),style="background-color: white, color: blue")
                         ),
                       radioButtons(self$ns("chooseExpDesign"),
                                    "",
                                    choices = c("Flat design (automatic)" = "FlatDesign" ,
                                                "Two levels design (complete Bio.Rep column)" = "twoLevelsDesign" ,
                                                "Three levels design (complete Bio.Rep and Tech.Rep columns)" = "threeLevelsDesign"
                                                ),
                                    selected=character(0),
                                    width = 'auto'
                                    )
                       )
                   )
                 ),
          column(width=6,
                 tags$div(id = self$ns('checkDesign_ui'),
                          tags$div(
                            style="display:inline-block;",
                            shinyjs::hidden(
                              actionButton(self$ns("btn_checkDesign"),
                                           "Check design",
                                           class = actionBtnClass
                                           )
                              )
                            ),
                          tags$div(
                            style="display:inline-block;",
                            shinyjs::hidden(
                              shinyjs::hidden(
                                uiOutput(self$ns('CheckingDesign_ui'))
                              )
                            )
                          )
                   )
                 ),
        hr(),
        selectInput(self$ns("convert_reorder"), "Order by conditions ?",
                    choices= setNames(nm = c("No", "Yes")),
                    width="100px"),
        tags$div(

          tags$div(style="display:inline-block; vertical-align: top;",
                   h4("Design"),
                   rHandsontableOutput(self$ns("designTableUI"))
          ),
          tags$div(style="display:inline-block; vertical-align: top;",
                   shinyjs::hidden(
                     div(id = self$ns("showExample"),
                         mod_build_design_example_ui(self$ns("designExamples"))
                         )
                     )
          )
        )

      )
      )
    },

    

    #---------------------------------------------------------------------------------
    #--------------------------- Step 5: Build design SERVER -----------------------------
    #---------------------------------------------------------------------------------

    Design_server = function(session, input, output){

      # observeEvent(req(input$linkToFaq1), {
      #   updateTabsetPanel(session, 'navPage', "faqTab")
      # })


      observeEvent(input$btn_validate_Design, ignoreInit = T, {
        self$ValidateCurrentPos()
      })
     

      #----------------------------------------------------------
      observeEvent(input$btn_checkConds,{
        self$process.var$file2convert
        input$convert_reorder

        if (length(grep("Bio.Rep", colnames(self$process.var$designTable))) > 0)  { return(NULL)}

        if (input$convert_reorder == "Yes") {
          self$process.var$newOrder <- order(self$process.var$designTable$Condition)
          self$process.var$designTable <- self$process.var$designTable[self$process.var$newOrder,]
        }

        self$process.var$conditionsChecked <- DAPAR2::check.conditions(self$process.var$designTable$Condition)
      })



      #----------------------------------------------------------
      observeEvent(input$quanti_data,{
        self$process.var$designTable  <- data.frame(Sample.name = as.character(input$quanti_data),
                              Condition = rep("", length(input$quanti_data)),
                              stringsAsFactors = FALSE)
      })

      
      
      color_renderer <- reactive({
        #self$process.var$designTable$Condition
        
        allConds <- self$process.var$designTable$Condition
        if (length(which(allConds==""))==0)
          uniqueConds <- unique(allConds)
        else
          uniqueConds <- unique(allConds[-which(allConds=="")])
        
        nUniqueConds <- length(uniqueConds)
        pal <- DAPAR2::ExtendPalette(nUniqueConds)
        
        codeForColors <- "function (instance, td, row, col, prop, value, cellProperties) {
  Handsontable.renderers.TextRenderer.apply(this, arguments);"
        c <- 1
        #browser()
        for (i in 1:length(allConds)){
          if (allConds[i] != "")
            
            codeForColors <- paste0(codeForColors, "if(row==",(i-1)," && col==",c, ") {td.style.background = '",
                          pal[which(allConds[i] == uniqueConds)],"';}")
        }
        codeForColors <- paste0(codeForColors,"}")
        
        codeForColors
      })
      
      
      #-------------------------------------------------------------
      output$designTableUI <- renderRHandsontable({
        self$process.var$designTable
        input$chooseExpDesign

        if (is.null(self$process.var$designTable)){
          self$process.var$designTable  <- data.frame(Sample.name = as.character(input$quanti_data),
                                Condition = rep("", length(input$quanti_data)),
                                stringsAsFactors = FALSE)
        }

        hot <- rhandsontable::rhandsontable(self$process.var$designTable,
                                            rowHeaders=NULL,
                                            fillHandle = list(direction='vertical',
                                                              autoInsertRow=FALSE,
                                                              maxRows=nrow(self$process.var$designTable)),
                                            readOnly=input$btn_validate_Design
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
                   if ("Bio.Rep" %in% colnames(self$process.var$designTable))
                     hot <- hot %>% rhandsontable::hot_col(col = "Bio.Rep", readOnly = TRUE)
                 },
                 twoLevelsDesign = {
                   if ("Tech.Rep" %in% colnames(self$process.var$designTable))
                     hot <- hot %>% rhandsontable::hot_col(col =  "Tech.Rep", readOnly = TRUE)
                 } ,
                 threeLevelsDesign = {
                   if ("Analyt.Rep" %in% colnames(self$process.var$designTable))
                     hot <- hot %>% rhandsontable::hot_col(col = "Analyt.Rep", readOnly = TRUE)
                 }
          )
        }
        hot
      })


      
      observe({
        req(self$process.var$designTable)
        input$convert_reorder
        
        shinyjs::toggle('btn_checkConds',
                        condition = (sum(self$process.var$designTable$Condition == "")==0) && (input$convert_reorder != "None"))
      })
      
      
      #----------------------------------------------------------
      output$UI_checkConditions  <- renderUI({

        req(self$process.var$designTable)
        req(self$process.var$conditionsChecked)
        input$convert_reorder

        if ((sum(self$process.var$designTable$Condition == "")==0) && (input$convert_reorder != "None")){
          if (isTRUE(self$process.var$conditionsChecked$valid)){
            img.file <- "Ok.png"
            txt <- "Correct conditions"
            } else {
              img.file <- "Problem.png"
              txt <- "Invalid conditions"
              }
          tagList(
            tags$div(
              tags$div(style="display:inline-block;",
                       tags$img(src=base64enc::dataURI(file=system.file('app/www/images', 
                                                                        img.file,
                                                                        package='Prostar.2.0'), 
                                                       mime="image/png"),
                                height=25)
                       ),
              tags$div(style="display:inline-block;",
                       tags$p(txt)
                       )
              ),
            if(!isTRUE(self$process.var$conditionsChecked$valid))
              tags$p(self$process.var$conditionsChecked$warn)
            )
            }
      })


observe({
  req(self$process.var$conditionsChecked)
  req(isTRUE(self$process.var$conditionsChecked$valid))
  
  shinyjs::toggle('UI_hierarchicalExp', condition = !is.null(self$process.var$conditionsChecked) && isTRUE(self$process.var$conditionsChecked$valid))
})
   

mod_build_design_example_server("designExamples")


      #------------------------------------------------------------------------------
      observe({
        shinyjs::onclick("btn_helpDesign",{
          shinyjs::toggle(id = "showExamples", anim = TRUE)}
        )
      })

      #------------------------------------------------------------------------------
      observeEvent(input$chooseExpDesign, {
        self$process.var$designTable
        self$process.var$designChecked <- NULL

        switch(input$chooseExpDesign,
               FlatDesign = {
                 self$process.var$designTable  <- data.frame(self$process.var$designTable[,1:2],
                                       Bio.Rep = seq(1:nrow(self$process.var$designTable)),
                                       stringsAsFactors = FALSE)
               },
               twoLevelsDesign = {
                 self$process.var$designTable  <- data.frame(self$process.var$designTable[,1:2],Bio.Rep = rep("",nrow(self$process.var$designTable)),
                                       Tech.Rep = seq(1:nrow(self$process.var$designTable)),
                                       stringsAsFactors = FALSE)
               },
               threeLevelsDesign = {
                 self$process.var$designTable  <- data.frame(self$process.var$designTable[,1:2],
                                       Bio.Rep = rep("",nrow(self$process.var$designTable)),
                                       Tech.Rep = rep("",nrow(self$process.var$designTable)),
                                       Analyt.Rep = seq(1:nrow(self$process.var$designTable)),
                                       stringsAsFactors = FALSE)
               }
        )
      })


      observeEvent(input$designTableUI,{
       # browser()
        self$process.var$designTable <-  hot_to_r(input$designTableUI)
        })

      #------------------------------------------------------------------------------
      observeEvent(input$btn_checkDesign,{
        self$process.var$designChecked <- DAPAR2::CheckDesign(self$process.var$designTable)
        shinyjs::toggle('btn_validate_Design', condition = isTRUE(self$process.var$conditionsChecked$valid))
        shinyjs::show('CheckingDesign_ui')
        })

      # Observe for checkDesign
      observe({
        req(input$chooseExpDesign)
        self$process.var$designChecked
        req(self$process.var$conditionsChecked)
        req(isTRUE(self$process.var$conditionsChecked$valid))
        
        cond <- !(sum(self$process.var$designTable$Bio.Rep == "") > 0) || !(((sum(self$process.var$designTable$Bio.Rep == "")+sum(self$process.var$designTable$Tech.Rep == "")) > 0))
        shinyjs::toggle('btn_checkDesign', condition = cond)
        
      })
      
      
      
      output$CheckingDesign_ui <- renderUI({
        #req(!is.null(self$process.var$designChecked$valid))
        
       if (isTRUE(self$process.var$designChecked$valid)){
         img.file <- "Ok.png"
         txt <- "Correct design"
         } else {
           img.file <- "Problem.png"
           txt <- "Invalid design"
         }
        
        tags$div(
          tags$div(style="display:inline-block;",
                   tags$img(src=base64enc::dataURI(file=system.file('app/www/images', 
                                                                    img.file,
                                                                    package='Prostar.2.0'), 
                                                   mime="image/png"),
                            height=25)
                             ),
          tags$div(style="display:inline-block;",
                   tags$p(txt)
                     )
            )
      })

      },

    #---------------------------------------------------------------------------------
    #----------------------------- Step 6: Convert UI --------------------------------
    #---------------------------------------------------------------------------------
    Convert_ui = function(){
      tagList(
        shinyjs::hidden(
          div(id = self$ns('FinalStep'),
              tagList(
                textInput(self$ns("filenameToCreate"), "Enter the name of the dataset"),
                actionButton(self$ns("btn_validate_convert"), 
                             "Convert data", 
                             class = actionBtnClass)
                )
              )
        ),
        
        mod_format_DT_ui(self$ns("overview_convertData")),
        
        shinyjs::hidden(
          div(id = self$ns('ConversionDone'),
              h4("The conversion is done. Your dataset has been automatically loaded 
              in memory. Now, you can switch to the Descriptive statistics panel to 
                 vizualize your data."),
              p("Once the 'Load' button (above) clicked, you will be automatically redirected to Prostar home page. 
              The dataset will be accessible within Prostar interface and processing menus will be enabled. 
              However, all importing functions ('Open MSnset', 'Demo data' and 'Convert data') will be disabled 
              (because successive dataset loading can make Prostar unstable). To work on another dataset, use first 
              the 'Reload Prostar' functionality from the 'Dataset manager' menu: it will make Prostar restart 
                with a fresh R session where import functions are enabled.")
          )
      )
      )
    },


    #---------------------------------------------------------------------------------
    #--------------------------- Step 6: Convert server ------------------------------
    #---------------------------------------------------------------------------------

    Convert_server = function(session, input, output){

observe({
  req(self$process.var$designChecked)
  self$rv$dataIn
  
  shinyjs::toggle('FinalStep', condition = isTRUE(self$process.var$designChecked$valid))
  shinyjs::toggle('ConversionDone', condition = !is.null(self$rv$dataIn) && !is.na(self$rv$dataIn))
})


      observeEvent(input$btn_validate_convert, ignoreInit = TRUE,{
        
        #  Prepare quantitative data indices
        tmp.eData.box <- input$quanti_data
        indexForQuantiData <- match(tmp.eData.box, colnames(self$process.var$rawData2convert))
        if (!is.null(self$process.var$newOrder)){
          tmp.eData.box <- tmp.eData.box[self$process.var$newOrder]
          indexForQuantiData <- indexForQuantiData[self$process.var$newOrder]
          }
        
        # Prepare colnames for Origin of values (type of identification)
        colNamesForOriginofValues <- NULL
        if (isTRUE(input$selectIdent)) {
          colNamesForOriginofValues <- shinyValue("colForOriginValue_",
                                                  nrow(quantiDataTable())
                                                  )
          #if (length(which(colNamesForOriginofValues == "None")) >0){ return (NULL)   }
        }
        #       indexForOriginOfValue <- NULL
        #       if (!is.null(colNamesForOriginofValues) && (length(grep("None", colNamesForOriginofValues))==0)  && (sum(is.na(colNamesForOriginofValues)) == 0)){
        #         for (i in 1:length(tmp.eData.box)){
        #           indexForOriginOfValue <- c(indexForOriginOfValue, which(colnames(self$self$process.var$rawData2convert) == input[[paste0("colForOriginValue_", i)]]))
        #         }
        #       }
        

        
        # Prepare versions info
        .versions <- list(Prostar_Version = installed.packages(lib.loc = Prostar.loc)["Prostar.2.0","Version"],
                          DAPAR_Version = installed.packages(lib.loc = DAPAR.loc)["DAPAR2","Version"]
                          )
        withProgress(message = 'Converting dataset',detail = '', value = 100, {
        self$rv$dataIn <- DAPAR2::createQFeatures(data = self$process.var$rawData2convert, 
                                                  sample = self$process.var$designTable, 
                                                  indExpData = indexForQuantiData, 
                                                  keyId = input$choose_global_id_ui, 
                                                  namesOrigin = colNamesForOriginofValues,
                                                  logTransform = input$checkDataLogged == "no", 
                                                  forceNA = input$replaceAllZeros,
                                                  typeOfData =  input$typeOfData,
                                                  parentProtId = gsub(".", "_", input$protein_id, fixed=TRUE)
                                                  )
        })
        self$ValidateCurrentPos()
    })
      
    }
  )
)