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
                   steps = c('Description', 'SelectFile', 'QuantiData'),
                   mandatory = c(T, T, T)
    )
    
    # .config = list(name = 'Convert',
    #                steps = c('Description', 'SelectFile', 'DataId', 'QuantiData', 'BuildDesign', 'Convert'),
    #                mandatory = c(T, T, T, T, T, T)
    # )
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
        req(self$rv$dataIn)
        
        
        columns <- c("Number of samples",
                     "Number of conditions",
                     "Number of lines", 
                     "Number of missing values", 
                     "% of missing values", 
                     "Number of empty lines")
        
        do <- data.frame(Definition = columns,
                         Value = rep(0,length(columns)))
        last.se <- self$rv$dataIn[[ength(self$rv$dataIn)]]
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
        #req(self$rv$rawData2convert)
        
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
                         })"))
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
      
      
      
      
      observeEvent(input$btn_validate_Step3, ignoreInit = T, {
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

    }


  #   #---------------------------------------------------------------------------------
  #   #------------------- Step 5: Build design UI----------------------------------------
  #   #---------------------------------------------------------------------------------
  #   
  #   BuildDesign_ui = function(){
  #     tagList(
  #       tags$p("If you do not know how to fill the experimental design, you can click
  #                                 on the '?' next to each design in the list that appear once the conditions 
  #                                 are checked or got to the ", 
  #              actionLink("linkToFaq1", "FAQ",style="background-color: white"), 
  #              " page."),
  #       fluidRow(
  #         column(width=6,tags$b("1 - Fill the \"Condition\" column to identify the conditions to compare.")),
  #         column(width=6,uiOutput("UI_checkConditions")  )
  #       ),
  #       fluidRow(
  #         column(width=6,uiOutput("UI_hierarchicalExp")),
  #         column(width=6,uiOutput("checkDesign") )
  #       ),
  #       hr(),
  #       selectInput("convert_reorder", "Order by conditions ?",
  #                   choices=c("No"="No", "Yes"="Yes"),
  #                   width="100px"),
  #       tags$div(
  #         
  #         tags$div(style="display:inline-block; vertical-align: top;",
  #                  uiOutput("viewDesign",width="100%")
  #         ),
  #         tags$div(style="display:inline-block; vertical-align: top;",
  #                  shinyjs::hidden(div(id = "showExamples", uiOutput("designExamples") ))
  #         )
  #       )
  #       
  #     )
  #   },
  #   
  #   
  #   #---------------------------------------------------------------------------------
  #   #--------------------------- Step 5: Build design UI -----------------------------
  #   #---------------------------------------------------------------------------------
  #   
  #   BuildDesign_server = function(){
  #     
  #     observeEvent(req(input$linkToFaq1), {
  #       updateTabsetPanel(session, 'navPage', "faqTab")
  #     })
  #     
  #     
  #     
  #     color_renderer <- reactive({
  #       rv$hot$Condition
  #       
  #       if (length(which(rv$hot$Condition==""))==0)
  #         uniqueConds <- unique(rv$hot$Condition)
  #       else
  #         uniqueConds <- unique(rv$hot$Condition[-which(rv$hot$Condition=="")])
  #       
  #       nUniqueConds <- length(uniqueConds)
  #       pal <- DAPAR2::ExtendPalette(nUniqueConds)
  #       
  #       txt <- "function (instance, td, row, col, prop, value, cellProperties) {
  # Handsontable.renderers.TextRenderer.apply(this, arguments);"
  #       c <- 1
  #       for (i in 1:length(rv$hot$Condition)){
  #         if (rv$hot$Condition[i] != "")
  #           
  #           txt <- paste0(txt, "if(row==",(i-1)," && col==",c, ") {td.style.background = '", 
  #                         pal[which(rv$hot$Condition[i] == uniqueConds)],"';}")
  #       }
  #       txt <- paste0(txt,"}")
  #       
  #       return (txt)
  #     })
  #     
  #     
  #     
  #     
  #     
  #     #----------------------------------------------------------
  #     observeEvent(input$btn_checkConds,{
  #       self$rv$file2convert
  #       input$convert_reorder
  #       
  #       if (length(grep("Bio.Rep", colnames(rv$hot))) > 0)  { return(NULL)}
  #       
  #       if (input$convert_reorder == "Yes") {
  #         rv$newOrder <- order(rv$hot$Condition)
  #         rv$hot <- rv$hot[rv$newOrder,]
  #       }
  #       
  #       rv$conditionsChecked <- DAPAR2::check.conditions(rv$hot$Condition)
  #     })
  #     
  #     
  #     
  #     #----------------------------------------------------------
  #     observeEvent(input$quanti_data,{
  #       rv$hot  <- data.frame(Sample.name = as.character(input$quanti_data),
  #                             Condition = rep("", length(input$quanti_data)),
  #                             stringsAsFactors = FALSE)
  #     })
  #     
  #     #-------------------------------------------------------------
  #     output$hot <- renderRHandsontable({
  #       rv$hot
  #       input$chooseExpDesign
  #       
  #       if (is.null(rv$hot)){
  #         rv$hot  <- data.frame(Sample.name = as.character(input$quanti_data),
  #                               Condition = rep("",length(input$quanti_data)),
  #                               stringsAsFactors = FALSE)
  #       }
  #       
  #       hot <- rhandsontable::rhandsontable(rv$hot,
  #                                           rowHeaders=NULL, 
  #                                           fillHandle = list(direction='vertical', 
  #                                                             autoInsertRow=FALSE,
  #                                                             maxRows=nrow(rv$hot))
  #                                           ) %>%
  #         rhandsontable::hot_rows(rowHeights = 30) %>%
  #         rhandsontable::hot_context_menu(allowRowEdit = TRUE, 
  #                                         allowColEdit = FALSE,
  #                                         allowInsertRow = FALSE,
  #                                         allowInsertColumn = FALSE,
  #                                         allowRemoveRow = TRUE,
  #                                         allowRemoveColumn = FALSE,
  #                                         autoInsertRow=FALSE     ) %>%
  #         rhandsontable:: hot_cols(renderer = color_renderer()) %>%
  #         rhandsontable::hot_col(col = "Sample.name", readOnly = TRUE)
  #       
  #       if (!is.null(input$chooseExpDesign)) {
  #         switch(input$chooseExpDesign,
  #                FlatDesign = {
  #                  if ("Bio.Rep" %in% colnames(rv$hot))
  #                    hot <- hot %>% rhandsontable::hot_col(col = "Bio.Rep", readOnly = TRUE)
  #                },
  #                twoLevelsDesign = {
  #                  if ("Tech.Rep" %in% colnames(rv$hot))
  #                    hot <- hot %>% rhandsontable::hot_col(col =  "Tech.Rep", readOnly = TRUE)
  #                } ,
  #                threeLevelsDesign = {
  #                  if ("Analyt.Rep" %in% colnames(rv$hot))
  #                    hot <- hot %>% rhandsontable::hot_col(col = "Analyt.Rep", readOnly = TRUE)
  #                }
  #         )
  #       }
  #       hot
  #     })
  #     
  #     
  #     #----------------------------------------------------------
  #     output$UI_checkConditions  <- renderUI({
  #       
  #       req(rv$hot)
  #       rv$conditionsChecked
  #       input$convert_reorder
  #       
  #       if ((sum(rv$hot$Condition == "")==0) && (input$convert_reorder != "None")){
  #         tags$div(
  #           tags$div(style="display:inline-block;",
  #                    actionButton(self$ns("btn_checkConds"),
  #                                 "Check conditions",
  #                                 class = actionBtnClass)
  #           ),
  #           
  #           tags$div(style="display:inline-block;",
  #                    if(!is.null(rv$conditionsChecked)){
  #                      
  #                      if (isTRUE(rv$conditionsChecked$valid)){
  #                        img <- "images/Ok.png"
  #                        txt <- "Correct conditions"
  #                      }else {
  #                        img <- "images/Problem.png"
  #                        txt <- "Invalid conditions"
  #                      }
  #                      tagList(
  #                        tags$div(
  #                          tags$div(style="display:inline-block;",tags$img(src = img, height=25)),
  #                          tags$div(style="display:inline-block;",tags$p(txt))
  #                        ),
  #                        if(!isTRUE(rv$conditionsChecked$valid)){
  #                          tags$p(rv$conditionsChecked$warn)
  #                        }
  #                      )
  #                    }
  #           )
  #         )
  #       } else {
  #         tagList(
  #           br(),
  #           br(),
  #           br(),
  #           br()
  #         )
  #         
  #       }
  #     })
  #     
  #     
  #     
  #     #------------------------------------------------------------------------------
  #     output$UI_hierarchicalExp <- renderUI({
  #       req(rv$conditionsChecked)
  #       req(isTRUE(rv$conditionsChecked$valid))
  #       
  #       tagList(
  #         div(
  #           div(
  #             # edit1
  #             style="display:inline-block; vertical-align: middle;",
  #             tags$b("2 - Choose the type of experimental design and complete it accordingly")
  #             ),
  #           div(
  #             # edit2
  #             style="display:inline-block; vertical-align: middle;",
  #             )
  #           ),
  #         radioButtons(self$ns("chooseExpDesign"), 
  #                      "",
  #                      choices = c("Flat design (automatic)" = "FlatDesign" ,
  #                                  "Two levels design (complete Bio.Rep column)" = "twoLevelsDesign" ,
  #                                    "Three levels design (complete Bio.Rep and Tech.Rep columns)" = "threeLevelsDesign" ),
  #                      selected=character(0))
  #         )
  #       })
  #     
  #     output$viewDesign <- renderUI({
  #       req(!isTRUE(rv$designSaved))
  #       
  #       tagList(
  #         h4("Design"),
  #         rHandsontableOutput(self$ns("hot"))
  #       )
  #     })
  #     
  #     
  #     callModule(moduleDesignExample,"buildDesignExampleThree", 3)
  #     callModule(moduleDesignExample,"buildDesignExampleTwo", 2)
  #     
  #     
  #     #------------------------------------------------------------------------------
  #     output$designExamples <- renderUI({
  #       input$chooseExpDesign
  #       
  #       switch(input$chooseExpDesign,
  #              FlatDesign = {
  #                tags$p("There is nothing to do for the flat design: the 'Bio.Rep' column is already filled.")
  #                },
  #              twoLevelsDesign =  {
  #                tagList(
  #                  h4("Example for a 2-levels design"),
  #                  moduleDesignExampleUI(self$ns("buildDesignExampleTwo"))
  #                  )
  #                },
  #              threeLevelsDesign =  {
  #                tagList(
  #                  h4("Example for a 3-levels design"),
  #                  moduleDesignExampleUI(self$ns("buildDesignExampleThree"))
  #                )
  #              }
  #             )
  #     })
  #     
  #     
  #     #------------------------------------------------------------------------------
  #     observe({
  #       shinyjs::onclick("btn_helpDesign",{
  #         shinyjs::toggle(id = "showExamples", anim = TRUE)}
  #       )
  #     })
  #     
  #     #------------------------------------------------------------------------------
  #     observeEvent(input$chooseExpDesign, {
  #       rv$hot
  #       rv$designChecked <- NULL
  #       
  #       switch(input$chooseExpDesign,
  #              FlatDesign = {
  #                rv$hot  <- data.frame(rv$hot[,1:2],
  #                                      Bio.Rep = seq(1:nrow(rv$hot)),
  #                                      stringsAsFactors = FALSE)
  #              },
  #              twoLevelsDesign = {
  #                rv$hot  <- data.frame(rv$hot[,1:2],Bio.Rep = rep("",nrow(rv$hot)),
  #                                      Tech.Rep = seq(1:nrow(rv$hot)),
  #                                      stringsAsFactors = FALSE)
  #              },
  #              threeLevelsDesign = {
  #                rv$hot  <- data.frame(rv$hot[,1:2],
  #                                      Bio.Rep = rep("",nrow(rv$hot)),
  #                                      Tech.Rep = rep("",nrow(rv$hot)),
  #                                      Analyt.Rep = seq(1:nrow(rv$hot)),
  #                                      stringsAsFactors = FALSE)
  #              }
  #       )
  #     })
  #     
  #     
  #     
  #     
  #     #------------------------------------------------------------------------------
  #     observeEvent(input$hot,{ 
  #       rv$hot <-  hot_to_r(input$hot)
  #       })
  #     
  #     
  #     
  #     #------------------------------------------------------------------------------
  #     observeEvent(input$btn_checkDesign,{ 
  #       rv$designChecked <- DAPAR2::check.design(rv$hot)
  #       })
  #     
  #     #------------------------------------------------------------------------------
  #     output$checkDesign <- renderUI({
  #       req(input$chooseExpDesign)
  #       rv$designChecked
  #       req(rv$conditionsChecked)
  #       req(isTRUE(rv$conditionsChecked$valid))
  #       
  #       switch(isolate({input$chooseExpDesign}),
  #              FlatDesign = {},
  #              twoLevelsDesign = { 
  #                if (sum(rv$hot$Bio.Rep == "") > 0) 
  #                  return(NULL)
  #                },
  #              threeLevelsDesign = {
  #                if ((sum(rv$hot$Bio.Rep == "")+sum(rv$hot$Tech.Rep == "")) > 0)
  #                  return(NULL)
  #                }
  #       )
  #       
  #       
  #       tags$div(
  #         tags$div(
  #           style="display:inline-block;",
  #           actionButton(self$ns("btn_checkDesign"),
  #                        "Check design", 
  #                        class = actionBtnClass)
  #         ),
  #         
  #         tags$div(
  #           style="display:inline-block;",
  #           if(!is.null(rv$designChecked)){
  #             
  #             if (isTRUE(rv$designChecked$valid)){
  #               shinyjs::enable("createMSnsetButton")
  #               img <- "images/Ok.png"
  #               txt <- "Correct design"
  #             }else {
  #               img <- "images/Problem.png"
  #               txt <- "Invalid design"}
  #             tagList(
  #               tags$div(
  #                 tags$div(style="display:inline-block;",tags$img(src = img, height=25)),
  #                 tags$div(style="display:inline-block;",tags$p(txt))
  #               ),
  #               if(!isTRUE(rv$designChecked$valid)){
  #                 shinyjs::disable("createMSnsetButton")
  #                 tags$p(rv$designChecked$warn)
  #               } else {
  #                 shinyjs::enable("createMSnsetButton")
  #               }
  #             )
  #           } else {
  #             shinyjs::disable("createMSnsetButton")
  #           }
  #           )
  #         )
  #       })
  #     },
  #   
  #   #---------------------------------------------------------------------------------
  #   #----------------------------- Step 6: Convert UI --------------------------------
  #   #---------------------------------------------------------------------------------
  #   Convert_ui = function(){
  #     tagList(
  #       br(), br(),
  #       mod_format_DT_ui(self$ns("overview_convertData")),
  #       uiOutput(self$ns("conversionDone")),
  #       p("Once the 'Load' button (above) clicked, you will be automatically redirected to Prostar home page. 
  #       The dataset will be accessible within Prostar interface and processing menus will be enabled. 
  #       However, all importing functions ('Open MSnset', 'Demo data' and 'Convert data') will be disabled 
  #       (because successive dataset loading can make Prostar unstable). To work on another dataset, use first 
  #       the 'Reload Prostar' functionality from the 'Dataset manager' menu: it will make Prostar restart 
  #         with a fresh R session where import functions are enabled.")
  #       )
  #   },
  #   
  #   
  #   #---------------------------------------------------------------------------------
  #   #--------------------------- Step 6: Convert server ------------------------------
  #   #---------------------------------------------------------------------------------
  #   
  #   Convert_server = function(){
  #     
  #     
  #     output$convertFinalStep <- renderUI({
  #       req(rv$designChecked)
  #       req(rv$designChecked$valid)
  #     
  #       tagList(
  #         uiOutput(self$ns("checkAll_convert"), width="50"),
  #         htmlOutput(self$ns("msgAlertCreateMSnset")),
  #         hr(),
  #         textInput(self$ns("filenameToCreate"),"Enter the name of the study"),
  #         actionButton(self$ns("createMSnsetButton"),"Convert data", class = actionBtnClass),
  #         uiOutput(self$ns("warningCreateMSnset"))
  #       )
  #     })
  #     
  #     
  #     output$conversionDone <- renderUI({
  #       req(rv$current.obj)
  #        
  #       h4("The conversion is done. Your dataset has been automatically loaded 
  #       in memory. Now, you can switch to the Descriptive statistics panel to 
  #          vizualize your data.")
  #       
  #     })
  #     
  #     output$warningCreateMSnset <- renderUI({
  #       if (isTRUE(input$selectIdent)){
  #         colNamesForOriginofValues <- shinyValue("colForOriginValue_",
  #                                                 nrow(quantiDataTable()))
  #         
  #         if (length(which(colNamesForOriginofValues == "None")) >0){
  #           text <- "<font color=\"red\"> Warning: The MSnset cannot be created because the identification 
  #           method are not fully filled.  <br>"
  #           HTML(text)
  #         }
  #       }
  #     })
  # 
  #     #######################################
  #     observeEvent(input$createMSnsetButton, ignoreInit = TRUE,{
  # 
  #       colNamesForOriginofValues <- NULL
  #       if (isTRUE(input$selectIdent)) {
  #         colNamesForOriginofValues <- shinyValue("colForOriginValue_", 
  #                                                 nrow(quantiDataTable())
  #                                                 )
  #         if (length(which(colNamesForOriginofValues == "None")) >0){ return (NULL)   }
  #       } 
  #       
  #       isolate({
  #         result = tryCatch(
  #           {
  #             ext <- GetExtension(self$rv$file2convert$name)
  #             txtTab <-  paste("tab1 <- read.csv(\"", self$rv$file2convert$name,
  #                              "\",header=TRUE, sep=\"\t\", as.is=T)",  sep="")
  #             txtXls <-  paste("tab1 <- read.xlsx(",self$rv$file2convert$name,
  #                              ",sheet=", input$XLSsheets,")",sep="")
  #             switch(ext,
  #                    txt = writeToCommandLogFile(txtTab),
  #                    csv = writeToCommandLogFile(txtTab),
  #                    tsv = writeToCommandLogFile(txtTab),
  #                    xls= writeToCommandLogFile(txtXls),
  #                    xlsx = writeToCommandLogFile(txtXls)
  #             )
  #             
  #             input$filenameToCreate
  #             self$rv$rawData2convert
  #             
  #             tmp.eData.box <- input$quanti_data
  #             indexForEData <- match(tmp.eData.box, colnames(self$rv$rawData2convert))
  #             if (!is.null(rv$newOrder)){
  #               tmp.eData.box <- tmp.eData.box[rv$newOrder]
  #               indexForEData <- indexForEData[rv$newOrder]
  #             }
  #             
  #             indexForFData <- seq(1, ncol(self$rv$rawData2convert))[-indexForEData]
  #             
  #             indexForchoose_global_id_ui <- NULL
  #             if (input$choose_global_id_ui !="AutoID") {
  #               indexForchoose_global_id_ui <- match(input$choose_global_id_ui, colnames(self$rv$rawData2convert))
  #             }
  #             
  #             
  #             metadata <- hot_to_r(input$hot)
  #             logData <- (input$checkDataLogged == "no")
  #             
  #             
  #             indexForOriginOfValue <- NULL
  #             if (!is.null(colNamesForOriginofValues) && (length(grep("None", colNamesForOriginofValues))==0)  && (sum(is.na(colNamesForOriginofValues)) == 0)){
  #               for (i in 1:length(tmp.eData.box)){
  #                 indexForOriginOfValue <- c(indexForOriginOfValue, which(colnames(self$rv$rawData2convert) == input[[paste0("colForOriginValue_", i)]]))
  #               }
  #             }
  #             
  #             
  #             versions <- list(Prostar_Version = 
  #                                installed.packages(lib.loc = Prostar.loc)["Prostar","Version"],
  #                              DAPAR_Version = 
  #                                installed.packages(lib.loc = DAPAR.loc)["DAPAR","Version"]
  #             )
  #             options(digits=15)
  #             
  #             tmp <- DAPAR::createMSnset(file = self$rv$rawData2convert, 
  #                                        metadata = metadata, 
  #                                        indExpData = indexForEData, 
  #                                        indFData = indexForFData, 
  #                                        indiceID = indexForchoose_global_id_ui,
  #                                        indexForOriginOfValue = indexForOriginOfValue,
  #                                        logData = logData, 
  #                                        replaceZeros = input$replaceAllZeros,
  #                                        pep_prot_data = input$typeOfData,
  #                                        proteinId =  gsub(".", "_", input$protein_id, fixed=TRUE),
  #                                        versions = versions
  #             )
  #             #ClearUI()
  #            # ClearMemory()
  #             rv$current.obj <- tmp
  #             
  #             rv$current.obj.name <- input$filenameToCreate
  #             rv$indexNA <- which(is.na(exprs(rv$current.obj)))
  #             
  #             l.params <- list(filename = input$filenameToCreate)
  #             
  #             loadObjectInMemoryFromConverter()
  #             
  #             updateTabsetPanel(session, "tabImport", selected = "Convert")
  #           },
  #           # warning = function(w) {
  #           #   if (conditionMessage(w) %in% c("NaNs produced", "production de NaN")){
  #           #     shinyjs::info(paste("Warning : Your original dataset may contain negative values",
  #           #                         "so that they cannot be logged. Please check back the dataset or", 
  #           #                         "the log option in the first tab.",
  #           #                         sep=" "))
  #           #   } else {
  #           #     shinyjs::info(paste("Warning in CreateMSnSet",":",
  #           #                         conditionMessage(w), 
  #           #                         sep=" "))
  #           #   }
  #           # }, 
  #           error = function(e) {
  #             shinyjs::info(paste("Error :","CreateMSnSet",":",
  #                                 conditionMessage(e), 
  #                                 sep=" "))
  #           }, finally = {
  #             #cleanup-code 
  #           })
  #         
  #         
  #         
  #       })
  #   })
      
  #  }
  )
)