type_id <- paste0("v1.0.20240923")


library("shiny")
library("shinycssloaders")
library("shinyjqui")
library("svglite")
library("ggplot2")
library("dplyr")
library("DT")
library("data.table")
library("readr")
library("nnet")
library("caret")
library("ggpubr")

options(shiny.maxRequestSize = 10000*1024^2)

# UI ---------------------------------------------------------------------------
ui <- navbarPage("{ shinyLogistic }",
                 # Data Preview -----------------------------------------------
                 tabPanel("Data Preview",
                          sidebarLayout(
                            sidebarPanel(
                              width = 3,
                              p(),
                              h3("Training Data"),
                              fileInput("MatFileInput", label = "Matrix Upload:"),
                              div(fileInput("MetFileInput", label = "Meta Upload:"), style = "margin-top:-10px"),
                              div(actionButton("LoadExpData","Load Example Data"), style = "margin-top:-10px"),
                              h4("Or"),
                              fileInput("ModelFileInput", label = "Model Upload:", accept = c(".rds",".RData")),
                              div(hr(), style = "margin-top:-10px"),
                              h3("Testing Data"),
                              radioButtons("ttOrsplit",NULL,choices = c("Upload Data","Split Loaded Data","Use Training Data"),
                                           inline = T, selected = "Use Training Data"),
                              conditionalPanel(condition = "input.ttOrsplit == 'Upload Data'",
                                               fileInput("TestMatFileInput", label = "Matrix Upload:"),
                                               div(fileInput("TestMetFileInput", label = "Meta Upload:"), style = "margin-top:-10px")
                              ),
                              conditionalPanel(condition = "input.ttOrsplit == 'Split Loaded Data'",
                                               radioButtons("splitPropOrNA",NULL,choices = c("Split by Proportion","Split by Value(s)"), inline = T),
                                               conditionalPanel(condition = "input.splitPropOrNA == 'Split by Proportion'",
                                                                h3("Split Input Data to Train and Test"),
                                                                numericInput("ttSplitProp","Proportion of samples to use in training data:",
                                                                             #value = 0.7, min = 0, max = 1, step = 0.1,
                                                                             value = NA, min = 0, max = 1, step = 0.1)
                                               ),
                                               conditionalPanel(condition = "input.splitPropOrNA == 'Split by Value(s)'",
                                                                selectizeInput("SplitColumn","Select Column to split:", choices = NULL, selected = 1),
                                                                selectizeInput("SplitColumnVal","Select Values to Train With:",
                                                                               choices = NULL, selected = 1, multiple = T)
                                               )
                              )
                            ),
                            
                            mainPanel(
                              p(),
                              verbatimTextOutput("FileCheckAlerts"),
                              p(),
                              uiOutput("rendPreviewTabs")
                              #tabsetPanel(
                              #  id = "tabs"
                              #)
                            )
                          )
                 ),
                 # Regularization -----------------------------------------------
                 tabPanel("Regularization",
                          sidebarLayout(
                            sidebarPanel(
                              width = 3,
                              p(),
                              h3("K-Means Parameters"),
                              fluidRow(
                                column(6,
                                       numericInput("regKmax","Max Number of Iterations:",value = 11, min = 1, step = 1)
                                ),
                                column(6,
                                       numericInput("regNstart","Initial Configurations:", value = 10, min = 1, step = 1)
                                )
                              )
                            ),
                            mainPanel(
                              plotOutput("RegClusterPlot", height = "500px", width = "100%")
                            )
                          )
                 ),
                 # Prediction -----------------------------------------------
                 tabPanel("Prediction",
                          sidebarLayout(
                            sidebarPanel(
                              width = 3,
                              
                              selectizeInput("FeatColumn","Select Feature to Predict:", choices = NULL, selected = 1),
                              span(textOutput("message"), style="color:red"),
                              selectizeInput("FeatRef","Select reference Feature:", choices = NULL, selected = 1),
                              #div(selectizeInput("FeatRef","Select reference Feature:", choices = NULL, selected = 1), style = "margin-top:-10px"),
                              div(radioButtons("PredClustOrAll","Generate model from:", choices = c("Clusters","All Features"), inline = T), style = "margin-top:-10px"),
                              conditionalPanel(condition = "input.PredClustOrAll == 'Clusters'",
                                               fluidRow(
                                                 column(6,
                                                        numericInput("predictK","Number of Custers:", value = 7, min = 1, step = 1)
                                                 ),
                                                 column(6,
                                                        numericInput("predNstart","Initial Configurations:", value = 10, min = 1, step = 1)
                                                 )
                                               )
                              ),
                              actionButton("RunShinyLR","Run Logistic Regression", width = "100%"),
                              p(),
                              downloadButton("dnldModel","Model"),
                              downloadButton("dnldTrainMat2","Training Matrix"),
                              downloadButton("dnldTrainMet2","Training Meta"),
                              p(),
                              conditionalPanel(condition = "input.PredictionData != '1'",
                                               selectizeInput("AddMetaCols","Add feature from meta data:", choices = NULL, selected = NULL, multiple = T)
                              ),
                              conditionalPanel(condition = "input.PredictionData == '2'",
                                               div(hr(), style = "margin-top:-10px;margin-bottom:-10px"),
                                               shiny::h3("Box Plot Parameters"),
                                               ## Limits the size of the select input box for group selection
                                               tags$head(
                                                 tags$style(HTML(
                                                   '.selectize-input {
                                                   max-height: 102px;
                                                   overflow-y: auto;
                                                   }'
                                                 )
                                                 )
                                               ),
                                               conditionalPanel(condition = "input.PredGroupCenters == '1'",
                                                                selectizeInput("BPgroupCriteriaTrain", label = "Grouping Criteria:", choices = NULL, selected = 1),
                                                                selectizeInput("BPgroupSelectionTrain", label = "Select Groups:", choices = NULL, multiple = T, selected = 1),
                                                                #fluidRow(
                                                                #  column(9, style = 'padding-right:2px;',
                                                                selectizeInput("BPFeatSelectionTrain", label = "Select Feature:", choices = NULL,
                                                                               multiple = F, selected = 1,width = "100%"),
                                                                #  ),
                                                                #  column(3, style = 'padding-left:2px;',
                                                                #         selectInput("BPlogOptTrain","Log:", choices = c("No Log","Log2","Log2+1","Log10","Log10+1"))
                                                                #  )
                                                                #),
                                                                fluidRow(
                                                                  column(5, style = 'padding-right:2px;',
                                                                         selectInput("BPplotstatCompTrain","Stat Test Method:",
                                                                                     choices = c("none","wilcox.test","t.test","kruskal.test","anova"))
                                                                  ),
                                                                  column(4, style = 'padding-right:4px;margin-top:10px',
                                                                         checkboxInput("BPplotsampledotsTrain","Include Dot Annotation", value = F)
                                                                  ),
                                                                  column(3, style = 'padding-left:4px;',
                                                                         numericInput("BPplotDotSizeTrain","Dot Size:", value = 2, step = 0.25)
                                                                  )
                                                                ),
                                                                fluidRow(
                                                                  column(5, #style = 'padding-right:2px;',
                                                                         selectInput("BPplotXaxOrderTrain","X-Axis Group Order",
                                                                                     choices = c("Ascending","Descending","Not Specificed"))
                                                                  ),
                                                                  column(3, style = 'margin-top:10px;',
                                                                         checkboxInput("BPflipBPTrain","Flip Axis", value = F)
                                                                         #checkboxInput("BPremoveSinglesTrain","Remove groups 1 or less samples", value = T)
                                                                  ),
                                                                  column(4, style = 'margin-top:20px;',
                                                                         radioButtons("BPorViolinTrain",NULL,choices = c("Box Plot","Violin Plot"), selected = "Box Plot")
                                                                  )
                                                                )
                                               ),
                                               conditionalPanel(condition = "input.PredGroupCenters == '2'",
                                                                selectizeInput("BPgroupCriteriaTest", label = "Grouping Criteria:", choices = NULL, selected = 1),
                                                                selectizeInput("BPgroupSelectionTest", label = "Select Groups:", choices = NULL, multiple = T, selected = 1),
                                                                #fluidRow(
                                                                #  column(9, style = 'padding-right:2px;',
                                                                selectizeInput("BPFeatSelectionTest", label = "Select Feature:", choices = NULL,
                                                                               multiple = F, selected = 1,width = "100%"),
                                                                #  ),
                                                                #  column(3, style = 'padding-left:2px;',
                                                                #         selectInput("BPlogOptTest","Log:", choices = c("No Log","Log2","Log2+1","Log10","Log10+1"))
                                                                #  )
                                                                #),
                                                                fluidRow(
                                                                  column(5, style = 'padding-right:2px;',
                                                                         selectInput("BPplotstatCompTest","Stat Test Method:",
                                                                                     choices = c("none","wilcox.test","t.test","kruskal.test","anova"))
                                                                  ),
                                                                  column(4, style = 'padding-right:4px;margin-top:10px',
                                                                         checkboxInput("BPplotsampledotsTest","Include Dot Annotation", value = F)
                                                                  ),
                                                                  column(3, style = 'padding-left:4px;',
                                                                         numericInput("BPplotDotSizeTest","Dot Size:", value = 2, step = 0.25)
                                                                  )
                                                                ),
                                                                fluidRow(
                                                                  column(5, #style = 'padding-right:2px;',
                                                                         selectInput("BPplotXaxOrderTest","X-Axis Group Order",
                                                                                     choices = c("Ascending","Descending","Not Specificed"))
                                                                  ),
                                                                  column(3, style = 'margin-top:10px;',
                                                                         checkboxInput("BPflipBPTest","Flip Axis", value = F)
                                                                         #checkboxInput("BPremoveSinglesTest","Remove groups 1 or less samples", value = T)
                                                                  ),
                                                                  column(4, style = 'margin-top:20px;',
                                                                         radioButtons("BPorViolinTest",NULL,choices = c("Box Plot","Violin Plot"), selected = "Box Plot")
                                                                  )
                                                                )
                                               ),
                                               conditionalPanel(condition = "input.PredGroupCenters == '3'",
                                                                selectizeInput("BPgroupCriteriaTT", label = "Grouping Criteria:", choices = NULL, selected = 1),
                                                                selectizeInput("BPgroupSelectionTT", label = "Select Groups:", choices = NULL, multiple = T, selected = 1),
                                                                #fluidRow(
                                                                #  column(9, style = 'padding-right:2px;',
                                                                selectizeInput("BPFeatSelectionTT", label = "Select Feature:", choices = NULL,
                                                                               multiple = F, selected = 1,width = "100%"),
                                                                #  ),
                                                                #  column(3, style = 'padding-left:2px;',
                                                                #         selectInput("BPlogOptTT","Log:", choices = c("No Log","Log2","Log2+1","Log10","Log10+1"))
                                                                #  )
                                                                #),
                                                                fluidRow(
                                                                  column(5, style = 'padding-right:2px;',
                                                                         selectInput("BPplotstatCompTT","Stat Test Method:",
                                                                                     choices = c("none","wilcox.test","t.test","kruskal.test","anova"))
                                                                  ),
                                                                  column(4, style = 'padding-right:4px;margin-top:10px',
                                                                         checkboxInput("BPplotsampledotsTT","Include Dot Annotation", value = F)
                                                                  ),
                                                                  column(3, style = 'padding-left:4px;',
                                                                         numericInput("BPplotDotSizeTT","Dot Size:", value = 2, step = 0.25)
                                                                  )
                                                                ),
                                                                fluidRow(
                                                                  column(5, #style = 'padding-right:2px;',
                                                                         selectInput("BPplotXaxOrderTT","X-Axis Group Order",
                                                                                     choices = c("Ascending","Descending","Not Specificed"))
                                                                  ),
                                                                  column(3, style = 'margin-top:10px;',
                                                                         checkboxInput("BPflipBPTT","Flip Axis", value = F)
                                                                         #checkboxInput("BPremoveSinglesTT","Remove groups 1 or less samples", value = T)
                                                                  ),
                                                                  column(4, style = 'margin-top:20px;',
                                                                         radioButtons("BPorViolinTT",NULL,choices = c("Box Plot","Violin Plot"), selected = "Box Plot")
                                                                  )
                                                                )
                                               )
                              )
                            ),
                            mainPanel(
                              uiOutput("rendPredictionData_tabs")
                            )
                          )
                 ),
                 # Prediction Analysis-----------------------------------------------
                 tabPanel("Prediction Analysis",
                          sidebarLayout(
                            sidebarPanel(
                              width = 3,
                              selectizeInput("SubsetCol","Subset Samples By:", choices = NULL, selected = 1),
                              uiOutput("rendSubsetFeat")
                            ),
                            mainPanel(
                              #tabsetPanel(id = "PredAnalysisTabs",
                              #tabPanel("Training",
                              p(),
                              fluidRow(
                                column(6,
                                       plotOutput("confMat_heat",height = "500px", width = "500px")
                                ),
                                column(6,
                                       h3("Overall Statistics"),
                                       tableOutput("confMat_summ")
                                ),
                              ),
                              h3("Feature Statistics"),
                              tableOutput("confMat_Featsumm")
                              #),
                              #tabPanel("Testing",
                              #         )
                              #)
                            )
                          )
                 )
)


# Server -----------------------------------------------------------------------

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  FileCheckAlerts_react1 <- reactiveVal(c())
  FileCheckAlerts_react2 <- reactiveVal(c())
  
  output$FileCheckAlerts <- renderPrint({
    req(FileCheckAlerts_react1())
    req(FileCheckAlerts_react2())
    text <- paste(c(FileCheckAlerts_react1(),FileCheckAlerts_react2()), collapse = "\n")
    cat(text)
  })
  #output$renddownload_notes <- renderUI({
  #  if (length(c(FileCheckAlerts_react1(),FileCheckAlerts_react2())) > 0) {
  #    downloadButton("download_notes","Download data processing notes")
  #  }
  #})
  #output$download_notes <- downloadHandler(
  #  filename = function() {
  #    paste("Merge_Data_Processing_Notes_", Sys.Date(), ".txt", sep = "")
  #  },
  #  content = function(file) {
  #    df <- c(FileCheckAlerts_react1(),FileCheckAlerts_react2())
  #    writeLines(df, file)
  #  }
  #)
  
  # Load in files --------------------------------------------------------------
  
  # File names
  model_react_file <- reactiveVal(NULL)
  mat_train_file <- reactiveVal(NULL)
  met_train_file <- reactiveVal(NULL)
  mat_test_file <-reactiveVal(NULL)
  met_test_file <-reactiveVal(NULL)
  # Raw data
  model_react <- reactiveVal(NULL)
  mat_train_raw <- reactiveVal(NULL)
  met_train_raw <- reactiveVal(NULL)
  mat_test_raw <- reactiveVal(NULL)
  met_test_raw <- reactiveVal(NULL)
  # Formatted Data
  mat_train <- reactiveVal(NULL)
  met_train <- reactiveVal(NULL)
  mat_test <- reactiveVal(NULL)
  met_test <- reactiveVal(NULL)
  
  observe({
    print(FileCheckAlerts_react1())
    print(FileCheckAlerts_react2())
  })
  
  observeEvent(input$ModelFileInput, {
    model_react_file(input$ModelFileInput$datapath)
  })
  observeEvent(input$MatFileInput, {
    mat_train_file(input$MatFileInput$datapath)
  })
  observeEvent(input$MetFileInput, {
    met_train_file(input$MetFileInput$datapath)
  })
  observeEvent(input$TestMatFileInput, {
    mat_test_file(input$TestMatFileInput$datapath)
  })
  observeEvent(input$TestMetFileInput, {
    met_test_file(input$TestMetFileInput$datapath)
  })
  
  output$rendPreviewTabs <- renderUI({
    training_mat_tab <- tabPanel("Training Matrix",
                                 p(),
                                 uiOutput("rendMatHead"),
                                 dataTableOutput("Training_Matrix_Preview"),
                                 downloadButton("dnldTrainMat","Download Table"),
                                 value = 1
    )
    training_meta_tab <- tabPanel("Training Meta",
                                  p(),
                                  uiOutput("rendMetHead"),
                                  dataTableOutput("Training_Meta_Preview"),
                                  downloadButton("dnldTrainMet","Download Table"),
                                  value = 2
    )
    testing_mat_tab <- tabPanel("Testing Matrix",
                                p(),
                                uiOutput("rendTestMatHead"),
                                dataTableOutput("Testing_Matrix_Preview"),
                                downloadButton("dnldTestMat","Download Table"),
                                value = 3
    )
    testing_meta_tab <- tabPanel("Testing Meta",
                                 p(),
                                 uiOutput("rendTestMetHead"),
                                 dataTableOutput("Testing_Meta_Preview"),
                                 downloadButton("dnldTestMet","Download Table"),
                                 value = 4
    )
    
    if (isTruthy(model_react())) {
      if (!isTruthy(mat_train_raw()) & !isTruthy(met_train_raw())) {
        tabsetPanel(id = "PreviewTabs",
                    testing_mat_tab,
                    testing_meta_tab
        )
      } else {
        tabsetPanel(id = "PreviewTabs",
                    training_mat_tab,
                    training_meta_tab,
                    testing_mat_tab,
                    testing_meta_tab
        )
      }
    } else {
      if (isTruthy(mat_train_raw()) & isTruthy(met_train_raw())) {
        tabsetPanel(id = "PreviewTabs",
                    training_mat_tab,
                    training_meta_tab,
                    testing_mat_tab,
                    testing_meta_tab
        )
      }
    }
    
  })
  
  observeEvent(model_react_file(), {
    FileCheckAlerts_list <- FileCheckAlerts_react1()
    FileCheckAlerts_react1(c(FileCheckAlerts_list,
                             paste0("Training Model Uploaded: ",input$ModelFileInput$name)))
    if (tolower(tools::file_ext(model_react_file())) == "rdata") {
      model <- loadRData(model_react_file())
      model_react(model)
    } else if (tolower(tools::file_ext(model_react_file())) == "rds") {
      model <- readRDS(model_react_file())
      model_react(model)
    }
  })
  
  observeEvent(mat_train_file(), {
    FileCheckAlerts_list <- FileCheckAlerts_react1()
    FileCheckAlerts_react1(c(FileCheckAlerts_list,
                             paste0("Training Matrix Uploaded: ",input$MatFileInput$name)))
    if (tolower(tools::file_ext(mat_train_file())) %in% c("zip","gz")) {
      mat_train_raw <- as.data.frame(read_delim(mat_train_file(),delim = '\t', col_names = T))
    } else {
      mat_train_raw <- as.data.frame(fread(mat_train_file()))
    }
    # Remove Duplicate features
    colnames(mat_train_raw)[1] <- "Feature"
    mat_train_raw_dup <- mat_train_raw[which(mat_train_raw[,1] %in% mat_train_raw[,1][duplicated(mat_train_raw[,1])]),]
    mat_train_raw_nondup <- mat_train_raw[which(!mat_train_raw[,1] %in% mat_train_raw[,1][duplicated(mat_train_raw[,1])]),]
    if (nrow(mat_train_raw_dup) > 0) {
      mat_train_raw_dup <- mat_train_raw_dup %>%
        group_by(Feature) %>%
        summarise_all(max)
    }
    mat_train_raw <- rbind(mat_train_raw_dup,mat_train_raw_nondup)
    if (nrow(mat_train_raw_dup) > 0) {
      message <- paste0(length(unique(mat_train_raw_dup[,1])), " duplicate features found in training matrix. Features reduced to those with maximum average value." )
      FileCheckAlerts_react1(FileCheckAlerts_list,message)
    }
    mat_train_raw(mat_train_raw)
  })
  
  observeEvent(met_train_file(), {
    FileCheckAlerts_list <- FileCheckAlerts_react1()
    FileCheckAlerts_react1(c(FileCheckAlerts_list,
                             paste0("Training Meta Uploaded: ",input$MetFileInput$name)))
    if (tolower(tools::file_ext(met_train_file())) %in% c("zip","gz")) {
      met_train_raw <- as.data.frame(read_delim(met_train_file(),delim = '\t', col_names = T))
      #met_train_raw(met_train_raw)
    } else {
      met_train_raw <- as.data.frame(fread(met_train_file()))
      #met_train_raw(met_train_raw)
    }
    char_cols <- names(which(unlist(lapply(met_train_raw, is.character))==TRUE))
    na_cols <- names(which(sapply(met_train_raw, function(x)all(is.na(x)))==TRUE))
    met_train_raw[,char_cols][is.na(met_train_raw[,char_cols])] <- "NA"
    met_train_raw[,na_cols][is.na(met_train_raw[,na_cols])] <- "NA"
    met_train_raw(met_train_raw)
    
    
    
  })
  
  observeEvent(mat_test_file(), {
    FileCheckAlerts_list <- FileCheckAlerts_react1()
    FileCheckAlerts_react1(c(FileCheckAlerts_list,
                             paste0("Testing Matrix Uploaded: ",input$TestMatFileInput$name)))
    if (tolower(tools::file_ext(mat_test_file())) %in% c("zip","gz")) {
      mat_test_raw <- as.data.frame(read_delim(mat_test_file(),delim = '\t', col_names = T))
    } else {
      mat_test_raw <- as.data.frame(fread(mat_test_file()))
    }
    # Remove Duplicate features
    colnames(mat_test_raw)[1] <- "Feature"
    mat_test_raw_dup <- mat_test_raw[which(mat_test_raw[,1] %in% mat_test_raw[,1][duplicated(mat_test_raw[,1])]),]
    mat_test_raw_nondup <- mat_test_raw[which(!mat_test_raw[,1] %in% mat_test_raw[,1][duplicated(mat_test_raw[,1])]),]
    if (nrow(mat_test_raw_dup) > 0) {
      mat_test_raw_dup <- mat_test_raw_dup %>%
        group_by(Feature) %>%
        summarise_all(max)
    }
    mat_test_raw <- rbind(mat_test_raw_dup,mat_test_raw_nondup)
    if (nrow(mat_test_raw_dup) > 0) {
      message <- paste0(length(unique(mat_test_raw_dup[,1])), " duplicate features found. Features reduced to those with maximum value." )
      FileCheckAlerts_react1(FileCheckAlerts_list,message)
    }
    mat_test_raw(mat_test_raw)
  })
  
  observeEvent(met_test_file(), {
    FileCheckAlerts_list <- FileCheckAlerts_react1()
    FileCheckAlerts_react1(c(FileCheckAlerts_list,
                             paste0("Testing Meta Uploaded: ",input$TestMetFileInput$name)))
    if (tolower(tools::file_ext(met_test_file())) %in% c("zip","gz")) {
      met_test_raw <- as.data.frame(read_delim(met_test_file(),delim = '\t', col_names = T))
      #met_test_raw(met_test_raw)
    } else {
      met_test_raw <- as.data.frame(fread(met_test_file()))
      #met_test_raw(met_test_raw)
    }
    char_cols <- names(which(unlist(lapply(met_test_raw, is.character))==TRUE))
    na_cols <- names(which(sapply(met_test_raw, function(x)all(is.na(x)))==TRUE))
    met_test_raw[,char_cols][is.na(met_test_raw[,char_cols])] <- "NA"
    met_test_raw[,na_cols][is.na(met_test_raw[,na_cols])] <- "NA"
    met_test_raw(met_test_raw)
  })
  
  observe({
    req(met_train_raw())
    if (input$ttOrsplit == "Split Loaded Data") {
      if (input$splitPropOrNA == "Split by Value(s)") {
        met_train_raw <- met_train_raw()
        featChoices <- colnames(met_train_raw)[-1]
        updateSelectizeInput(session,"SplitColumn",choices = featChoices, server = T)
      }
    }
  })
  observe({
    req(mat_train_raw())
    req(met_train_raw())
    req(input$SplitColumn)
    if (input$ttOrsplit == "Split Loaded Data") {
      if (input$splitPropOrNA == "Split by Value(s)") {
        mat_train_raw <- mat_train_raw()
        met_train_raw <- met_train_raw()
        splitCol <- input$SplitColumn
        valChoices <- unique(met_train_raw[,splitCol])
        updateSelectizeInput(session,"SplitColumnVal",choices = valChoices)
      }
    }
  })
  
  # Format Data ----------------------------------------------------------------
  observe({
    
    FileCheckAlerts_list <- c()
    if (is.null(model_react())) {
      req(mat_train_raw())
      req(met_train_raw())
      mat_test_raw <- NULL
      met_test_raw <- NULL
      mat_train_raw <- mat_train_raw()
      met_train_raw <- met_train_raw()
      FileCheckAlerts_list <- c()
      
      rownames(mat_train_raw) <- mat_train_raw[,1]
      mat_train_raw <- mat_train_raw[,-1]
      
      rownames(met_train_raw) <- met_train_raw[,1]
      
      sampSame <- intersect(colnames(mat_train_raw),rownames(met_train_raw))
      mat_train_raw <- mat_train_raw[,sampSame]
      met_train_raw <- met_train_raw[sampSame,]
      
      if (length(sampSame) != ncol(mat_train_raw) | length(sampSame) != nrow(met_train_raw)) {
        message <- paste0("Mistmatching or missing sample names found between training matrix and meta data. Reduced to only similar samples (N=",length(sampSame),")")
        FileCheckAlerts_list <- c(FileCheckAlerts_list,message)
      }
      
      if (input$ttOrsplit == "Upload Data") {
        req(mat_test_raw())
        mat_test_raw <- mat_test_raw()
        rownames(mat_test_raw) <- mat_test_raw[,1]
        mat_test_raw <- mat_test_raw[,-1]
        featSame <- intersect(rownames(mat_train_raw),rownames(mat_test_raw))
        mat_train_raw <- mat_train_raw[featSame,]
        mat_test_raw <- mat_test_raw[featSame,]
        if (length(featSame) != nrow(mat_train_raw) | length(featSame) != nrow(mat_test_raw)) {
          message <- paste0("Mistmatching features found between training and testing matrix. Reduced to only similar features (N=",length(featSame),")")
          FileCheckAlerts_list <- c(FileCheckAlerts_list,message)
        }
        if (!is.null(mat_test_raw) & !is.null(met_test_raw())) {
          met_test_raw <- met_test_raw()
          rownames(met_test_raw) <- met_test_raw[,1]
          sampSame_test <- intersect(colnames(mat_test_raw),rownames(met_test_raw))
          mat_test_raw <- mat_test_raw[,sampSame_test]
          met_test_raw <- met_test_raw[sampSame_test,]
          featSame <- intersect(rownames(mat_train_raw),rownames(mat_test_raw))
          mat_train_raw <- mat_train_raw[featSame,]
          mat_test_raw <- mat_test_raw[featSame,]
          if (length(featSame) != nrow(mat_train_raw) | length(featSame) != nrow(mat_test_raw)) {
            message <- paste0("Mistmatching features found between training and testing matrix. Reduced to only similar features (N=",length(featSame),")")
            FileCheckAlerts_list <- c(FileCheckAlerts_list,message)
          }
          if (length(sampSame_test) != ncol(mat_test_raw) | length(sampSame_test) != nrow(met_test_raw)) {
            message <- paste0("Mistmatching or missing sample names found between feature matrix and meta data. Reduced to only similar samples (N=",length(sampSame_test),")")
            FileCheckAlerts_list <- c(FileCheckAlerts_list,message)
          }
          FileCheckAlerts_list <- c(FileCheckAlerts_list,
                                    paste0("Number of samples in training matrix: ",ncol(mat_train_raw)),
                                    paste0("Number of samples in training meta: ",nrow(met_train_raw)),
                                    paste0("Number of samples in testing matrix: ",ncol(mat_test_raw)),
                                    paste0("Number of samples in testing meta: ",nrow(met_test_raw)))
        }
      }
      if (input$ttOrsplit == "Split Loaded Data") {
        if (input$splitPropOrNA == "Split by Proportion") {
          req(input$ttSplitProp)
          prop <- input$ttSplitProp
          train_num <- round(ncol(mat_train_raw) * prop)
          train_samp <- sample(colnames(mat_train_raw),train_num)
          test_samp <- setdiff(colnames(mat_train_raw),train_samp)
          mat_test_raw <- mat_train_raw[,test_samp]
          mat_train_raw <- mat_train_raw[,train_samp]
          met_test_raw <- met_train_raw[test_samp,]
          met_train_raw <- met_train_raw[train_samp,]
          FileCheckAlerts_list <- c(FileCheckAlerts_list,
                                    paste0("Training data split. Training with ",(1-prop)*100,"% of samples and testing on the remaining ",prop*100,"% of samples."))
          FileCheckAlerts_list <- c(FileCheckAlerts_list,
                                    paste0("Number of samples in training matrix: ",ncol(mat_train_raw)),
                                    paste0("Number of samples in training meta: ",nrow(met_train_raw)),
                                    paste0("Number of samples in testing matrix: ",ncol(mat_test_raw)),
                                    paste0("Number of samples in testing meta: ",nrow(met_test_raw)))
        } else if (input$splitPropOrNA == "Split by Value(s)") {
          req(input$SplitColumn)
          req(input$SplitColumnVal)
          splitCol <- input$SplitColumn
          splitVal <- input$SplitColumnVal
          splitVal[which(splitVal=="NA")] <- NA
          met_test_raw <- met_train_raw[which(!met_train_raw[,splitCol] %in% splitVal),]
          met_train_raw <- met_train_raw[which(met_train_raw[,splitCol] %in% splitVal),]
          mat_test_raw <- mat_train_raw[,met_test_raw[,1]]
          mat_train_raw <- mat_train_raw[,met_train_raw[,1]]
          splitVal[is.na(splitVal)] <- "NA"
          FileCheckAlerts_list <- c(FileCheckAlerts_list,
                                    paste0("Training data split by ",splitCol,". Training data identifiers: ",paste0(splitVal, collapse = ", ")))
          FileCheckAlerts_list <- c(FileCheckAlerts_list,
                                    paste0("Number of samples in training matrix: ",ncol(mat_train_raw)),
                                    paste0("Number of samples in training meta: ",nrow(met_train_raw)),
                                    paste0("Number of samples in testing matrix: ",ncol(mat_test_raw)),
                                    paste0("Number of samples in testing meta: ",nrow(met_test_raw)))
        }
      }
      if (input$ttOrsplit == "Use Training Data") {
        mat_test_raw <- mat_train_raw
        met_test_raw <- met_train_raw
        FileCheckAlerts_list <- c(FileCheckAlerts_list,
                                  paste0("Training data split will be used as testing data"))
        FileCheckAlerts_list <- c(FileCheckAlerts_list,
                                  paste0("Number of samples in training matrix: ",ncol(mat_train_raw)),
                                  paste0("Number of samples in training meta: ",nrow(met_train_raw)),
                                  paste0("Number of samples in testing matrix: ",ncol(mat_test_raw)),
                                  paste0("Number of samples in testing meta: ",nrow(met_test_raw)))
      }
      
      FileCheckAlerts_react2(FileCheckAlerts_list)
      mat_train(mat_train_raw)
      met_train(met_train_raw)
      mat_test(mat_test_raw)
      met_test(met_test_raw)
    } else {
      req(met_test_raw())
      req(mat_test_raw())
      met_test_raw <- met_test_raw()
      mat_test_raw <- mat_test_raw()
      rownames(met_test_raw) <- met_test_raw[,1]
      sampSame_test <- intersect(colnames(mat_test_raw),rownames(met_test_raw))
      mat_test_raw <- mat_test_raw[,sampSame_test]
      met_test_raw <- met_test_raw[sampSame_test,]
      if (length(sampSame_test) != ncol(mat_test_raw) | length(sampSame_test) != nrow(met_test_raw)) {
        message <- paste0("Mistmatching or missing sample names found between feature matrix and meta data. Reduced to only similar samples (N=",length(sampSame_test),")")
        FileCheckAlerts_list <- c(FileCheckAlerts_list,message)
      }
      FileCheckAlerts_list <- c(FileCheckAlerts_list,
                                paste0("Number of samples in testing matrix: ",ncol(mat_test_raw)),
                                paste0("Number of samples in testing meta: ",nrow(met_test_raw)))
      FileCheckAlerts_react2(FileCheckAlerts_list)
      mat_test(mat_test_raw)
      met_test(met_test_raw)
    }
    
    
  })
  
  # View Data ------------------------------------------------------------------
  
  output$rendMatHead <- renderUI({
    req(mat_train())
    if (ncol(mat_train()) > 301) {
      radioButtons("rendMatHead",NULL, choices = c("View table head","View entire table"), inline = T)
    } else {
      radioButtons("rendMatHead",NULL, choices = c("View table head","View entire table"), inline = T, selected = "View entire table")
    }
  })
  output$Training_Matrix_Preview <- renderDataTable({
    req(mat_train())
    req(input$rendMatHead)
    mat <- as.data.frame(mat_train())
    if (input$rendMatHead == "View table head") {
      mat <- head(mat,c(100,100))
    }
    datatable(mat,
              options = list(lengthMenu = c(5,10, 20, 100, 1000),
                             pageLength = 10,
                             scrollX = T))
  })
  
  output$rendMetHead <- renderUI({
    req(met_train())
    if (ncol(met_train()) > 301) {
      radioButtons("MetHead",NULL, choices = c("View table head","View entire table"), inline = T)
    } else {
      radioButtons("MetHead",NULL, choices = c("View table head","View entire table"), inline = T, selected = "View entire table")
    }
  })
  output$Training_Meta_Preview <- renderDataTable({
    req(met_train())
    req(input$MetHead)
    mat <- as.data.frame(met_train())
    if (input$MetHead == "View table head") {
      mat <- head(mat,c(100,100))
    }
    datatable(mat,
              options = list(lengthMenu = c(5,10, 20, 100, 1000),
                             pageLength = 10,
                             scrollX = T),
              rownames = F)
  })
  
  output$rendTestMatHead <- renderUI({
    req(mat_test())
    if (ncol(mat_test()) > 301) {
      radioButtons("TestMatHead",NULL, choices = c("View table head","View entire table"), inline = T)
    } else {
      radioButtons("TestMatHead",NULL, choices = c("View table head","View entire table"), inline = T, selected = "View entire table")
    }
  })
  output$Testing_Matrix_Preview <- renderDataTable({
    req(mat_test())
    req(input$TestMatHead)
    mat <- as.data.frame(mat_test())
    if (input$TestMatHead == "View table head") {
      mat <- head(mat,c(100,100))
    }
    datatable(mat,
              options = list(lengthMenu = c(5,10, 20, 100, 1000),
                             pageLength = 10,
                             scrollX = T))
  })
  
  output$rendTestMetHead <- renderUI({
    req(met_test())
    if (ncol(met_test()) > 301) {
      radioButtons("TestMetHead",NULL, choices = c("View table head","View entire table"), inline = T)
    } else {
      radioButtons("TestMetHead",NULL, choices = c("View table head","View entire table"), inline = T, selected = "View entire table")
    }
  })
  output$Testing_Meta_Preview <- renderDataTable({
    req(met_test())
    req(input$TestMetHead)
    mat <- as.data.frame(met_test())
    if (input$TestMetHead == "View table head") {
      mat <- head(mat,c(100,100))
    }
    datatable(mat,
              options = list(lengthMenu = c(5,10, 20, 100, 1000),
                             pageLength = 10,
                             scrollX = T),
              rownames = F)
  })
  
  # Regularization -------------------------------------------------------------
  
  RegClusterdf_react <- reactive({
    mat_train <- mat_train()
    kmax <- input$regKmax
    nstart <- input$regNstart
    wss <- sapply(1:kmax,function(k){kmeans(mat_train,k,nstart=nstart)$tot.withinss})
    wss
  })
  output$RegClusterPlot <- renderPlot({
    
    wss <- RegClusterdf_react()
    kmax <- input$regKmax
    plot(1:kmax,wss,typ="b",pch=19,frame=F,xlab="Number of clusters",
         ylab="Total with clusters sum of squares")
  })
  
  # Prediction -----------------------------------------------------------------
  
  ## Inputs --------------------------------------------------------------------
  observe({
    req(met_train())
    met_train <- met_train()
    updateSelectizeInput(session,"FeatColumn", choices = colnames(met_train)[-1], server = T)
  })
  observe({
    req(met_train())
    req(input$FeatColumn)
    met_train <- met_train()
    refChoices <- unique(met_train()[,input$FeatColumn])
    refChoices <- refChoices[order(is.na(refChoices))]
    updateSelectizeInput(session,"FeatRef",choices = refChoices)
  })
  observe({
    met_train <- met_train()
    met_test <- met_test()
    col_choices <- unique(c(colnames(met_train)[-1],colnames(met_test)[-1]))
    updateSelectizeInput(session,"AddMetaCols", choices = col_choices, selected = NULL, server = T)
  })
  
  ## UI ------------------------------------------------------------------------
  
  output$rendPredictionData_tabs <- renderUI({
    if (input$ttOrsplit == "Use Training Data") {
      tabsetPanel(id = "PredictionData",
                  tabPanel("Training Data Clusters",
                           p(),
                           h3("Model Coefficients"),
                           dataTableOutput("TrainDataClusterCoeffTab"),
                           downloadButton("dnldTrainDataClusterCoeffTab","Download Table"),
                           h3("Cluster Assignments"),
                           dataTableOutput("TrainDataClustersTab"),
                           downloadButton("dnldTrainDataClustersTab","Download Table"),
                           value = 1),
                  tabPanel("Group Centers",
                           tabsetPanel(id = "PredGroupCenters",
                                       tabPanel("Training",
                                                p(),
                                                #uiOutput("rendTrainGroupCentersBP"),
                                                jqui_resizable(plotOutput("TrainGroupCentersBP", height = "400px", width = "100%")),
                                                downloadButton("dnldTrainGroupCentersBP","SVG"),
                                                p(),
                                                radioButtons("TrainGroupCentersTabHead",NULL,
                                                             choices = c("View table head","View entire table"), inline = T),
                                                dataTableOutput("TrainGroupCentersTab"),
                                                downloadButton("dnldTrainGroupCentersTab","Download Table"),
                                                value = 1
                                       )
                                       
                           ),
                           value = 2
                  ),
                  tabPanel("Model Fitted",
                           p(),
                           radioButtons("ModelFittedTabHead",NULL,
                                        choices = c("View table head","View entire table"), inline = T),
                           dataTableOutput("ModelFittedTab"),
                           downloadButton("dnldModelFittedTab","Download Table"),
                           value = 3
                  ),
                  tabPanel("Predictions",
                           tabsetPanel(id = "PredResults",
                                       tabPanel("Training",
                                                p(),
                                                radioButtons("TrainPredictionsTabHead",NULL,
                                                             choices = c("View table head","View entire table"), inline = T),
                                                dataTableOutput("TrainPredictionsTab"),
                                                downloadButton("dnldTrainPredictionsTab","Download Table")
                                       )
                                       
                           ),
                           value = 4
                  )
      )
    } else {
      tabsetPanel(id = "PredictionData",
                  tabPanel("Training Data Clusters",
                           p(),
                           h3("Model Coefficients"),
                           dataTableOutput("TrainDataClusterCoeffTab"),
                           downloadButton("dnldTrainDataClusterCoeffTab","Download Table"),
                           h3("Cluster Assignments"),
                           dataTableOutput("TrainDataClustersTab"),
                           downloadButton("dnldTrainDataClustersTab","Download Table"),
                           value = 1),
                  tabPanel("Group Centers",
                           tabsetPanel(id = "PredGroupCenters",
                                       tabPanel("Training",
                                                p(),
                                                #uiOutput("rendTrainGroupCentersBP"),
                                                jqui_resizable(plotOutput("TrainGroupCentersBP", height = "400px", width = "100%")),
                                                downloadButton("dnldTrainGroupCentersBP","SVG"),
                                                p(),
                                                radioButtons("TrainGroupCentersTabHead",NULL,
                                                             choices = c("View table head","View entire table"), inline = T),
                                                dataTableOutput("TrainGroupCentersTab"),
                                                downloadButton("dnldTrainGroupCentersTab","Download Table"),
                                                value = 1
                                       ),
                                       tabPanel("Testing",
                                                p(),
                                                #uiOutput("rendTestGroupCentersBP"),
                                                jqui_resizable(plotOutput("TestGroupCentersBP", height = "400px", width = "100%")),
                                                downloadButton("dnldTestGroupCentersBP","SVG"),
                                                p(),
                                                radioButtons("TestGroupCentersTabHead",NULL,
                                                             choices = c("View table head","View entire table"), inline = T),
                                                dataTableOutput("TestGroupCentersTab"),
                                                downloadButton("dnldTestGroupCentersTab","Download Table"),
                                                value = 2
                                       ),
                                       tabPanel("Training and Testing",
                                                p(),
                                                #uiOutput("rendTrainTestGroupCentersBP"),
                                                jqui_resizable(plotOutput("TrainTestGroupCentersBP", height = "400px", width = "100%")),
                                                downloadButton("dnldTrainTestGroupCentersBP","SVG"),
                                                p(),
                                                radioButtons("TrainTestGroupCentersTabHead",NULL,
                                                             choices = c("View table head","View entire table"), inline = T),
                                                withSpinner(dataTableOutput("TrainTestGroupCentersTab"), type = 6),
                                                downloadButton("dnldTrainTestGroupCentersTab","Download Table"),
                                                value = 3
                                       )
                                       
                           ),
                           value = 2
                  ),
                  tabPanel("Model Fitted",
                           p(),
                           radioButtons("ModelFittedTabHead",NULL,
                                        choices = c("View table head","View entire table"), inline = T),
                           dataTableOutput("ModelFittedTab"),
                           downloadButton("dnldModelFittedTab","Download Table"),
                           value = 3
                  ),
                  tabPanel("Predictions",
                           tabsetPanel(id = "PredResults",
                                       tabPanel("Training",
                                                p(),
                                                radioButtons("TrainPredictionsTabHead",NULL,
                                                             choices = c("View table head","View entire table"), inline = T),
                                                dataTableOutput("TrainPredictionsTab"),
                                                downloadButton("dnldTrainPredictionsTab","Download Table")
                                       ),
                                       tabPanel("Testing",
                                                p(),
                                                radioButtons("TestPredictionsTabHead",NULL,
                                                             choices = c("View table head","View entire table"), inline = T),
                                                dataTableOutput("TestPredictionsTab"),
                                                downloadButton("dnldTestPredictionsTab","Download Table")
                                       ),
                                       tabPanel("Training and Testing",
                                                p(),
                                                radioButtons("TrainTestPredictionsTabHead",NULL,
                                                             choices = c("View table head","View entire table"), inline = T),
                                                withSpinner(dataTableOutput("TrainTestPredictionsTab"), type = 6),
                                                downloadButton("dnldTrainTestPredictionsTab","Download Table")
                                       )
                                       
                           ),
                           value = 4
                  )
      )
    }
    
  })
  
  
  ## Work ----------------------------------------------------------------------
  pred_obj <- eventReactive(input$RunShinyLR, {
    
    req(input$FeatColumn)
    req(input$FeatRef)
    req(input$predictK)
    req(input$predNstart)
    mat_train <- mat_train()
    met_train <- met_train()
    mat_test <- mat_test()
    met_test <- met_test()
    model_react <- model_react()
    feature <- input$FeatColumn
    ref <- input$FeatRef
    usek <- input$PredClustOrAll
    usek <- ifelse(usek == "Clusters",TRUE,FALSE)
    clusters <- input$predictK
    kmax <- input$regKmax
    nstart <- input$predNstart
    #save(list = ls(), file = "featPredenv.RData", envir = environment())
    if (isTruthy(model_react) & isTruthy(mat_test) & isTruthy(met_test)) {
      res <- lrFeaturePredict(model = model_react,
                              data_test = mat_test,
                              meta_test = met_test,
                              feature = feature)
      res
      
    } else if (isTruthy(mat_train) & isTruthy(met_train) & isTruthy(mat_test) & isTruthy(met_test)) {
      if (length(unique(met_train[,feature])) > 1 & (ref %in% unique(met_train[,feature]))) {
        res <- lrFeaturePredict(data_train = mat_train,
                                meta_train = met_train,
                                data_test = mat_test,
                                meta_test = met_test,
                                feature = feature,
                                reference = ref,
                                usek = usek,
                                k = clusters,
                                kmax = kmax,
                                nstart = nstart)
        res
      }
    }
    
  })
  
  output$message <- renderText({
    req(input$FeatColumn)
    req(met_train())
    met_train <- met_train()
    feature <- input$FeatColumn
    if (length(unique(met_train[,feature])) <= 1) {
      "Two or more classes are required to fit a multinomial model."
    }
  })
  
  
  ## Tables --------------------------------------------------------------------
  
  TrainDataClusterCoeffTab_react <- reactive({
    req(pred_obj())
    res <- pred_obj()
    feature <- input$FeatColumn
    res_model_summ <- summary(res$`Regression Model`)
    df <- as.data.frame(res_model_summ$coefficients)
    df <- cbind(type = rownames(df),
                df)
    colnames(df) <- c(feature,"Coefficients")
    df
  })
  output$TrainDataClusterCoeffTab <- renderDataTable({
    df <- TrainDataClusterCoeffTab_react()
    datatable(df,
              options = list(lengthMenu = c(5,10, 20, 100, 1000),
                             pageLength = 20,
                             scrollX = T),
              rownames = F) %>%
      formatRound(columns = colnames(df)[-1], digits = 5)
  })
  TrainDataClustersTab_react <- reactive({
    req(pred_obj())
    res <- pred_obj()
    df <- data.frame(Feature = names(res$clusters),
                     cluster = res$clusters)
    df <- df[order(df$cluster),]
    colnames(df)[2] <- "Model Feature Clusters"
    df
  })
  output$TrainDataClustersTab <- renderDataTable({
    df <- TrainDataClustersTab_react()
    datatable(df,
              options = list(lengthMenu = c(5,10, 20, 100, 1000),
                             pageLength = 20,
                             scrollX = T),
              rownames = F)
  })
  TrainGroupCentersTab_react <- reactive({
    req(pred_obj())
    res <- pred_obj()
    meta_cols <- input$AddMetaCols
    met_train <- met_train()
    feature <- input$FeatColumn
    df <- res$`Group Center Training`
    df <- cbind(SampleName = rownames(df),df)
    df <- df[order(as.character(df[,feature])),]
    colnames(df)[1] <- colnames(met_train)[1]
    if (any(meta_cols %in% colnames(met_train))) {
      df <- merge(met_train[,c(colnames(met_train)[1],meta_cols)],df, all.y = T)
      df <- df %>% relocate(any_of(meta_cols) , .after = !!sym(feature))
    }
    df
  })
  output$TrainGroupCentersTab <- renderDataTable({
    req(pred_obj())
    req(TrainGroupCentersTab_react())
    df <- TrainGroupCentersTab_react()
    res <- pred_obj()
    if (input$TrainGroupCentersTabHead == "View table head") {
      df <- head(df,c(100,100))
    }
    datatable(df,
              options = list(lengthMenu = c(5,10, 20, 100, 1000),
                             pageLength = 20,
                             scrollX = T),
              rownames = F) %>%
      formatRound(columns = res$`Regression Model`$coefnames[-1], digits = 5)
  })
  TestGroupCentersTab_react <- reactive({
    req(pred_obj())
    res <- pred_obj()
    meta_cols <- input$AddMetaCols
    met_test <- met_test()
    df <- res$`Group Center Testing`
    df <- cbind(SampleName = rownames(df),df)
    colnames(df)[1] <- colnames(met_test)[1]
    if (any(meta_cols %in% colnames(met_test))) {
      df <- merge(met_test[,c(colnames(met_test)[1],meta_cols)],df, all.y = T)
      df <- df %>% relocate(any_of(meta_cols) , .after = colnames(df)[1])
    }
    df
  })
  output$TestGroupCentersTab <- renderDataTable({
    req(pred_obj())
    req(TestGroupCentersTab_react())
    res <- pred_obj()
    df <- TestGroupCentersTab_react()
    if (input$TestGroupCentersTabHead == "View table head") {
      df <- head(df,c(100,100))
    }
    datatable(df,
              options = list(lengthMenu = c(5,10, 20, 100, 1000),
                             pageLength = 20,
                             scrollX = T),
              rownames = F) %>%
      formatRound(columns = res$`Regression Model`$coefnames[-1], digits = 5)
  })
  TrainTestGroupCentersTab_react <- reactive({
    req(pred_obj())
    res <- pred_obj()
    meta_cols <- input$AddMetaCols
    met_train <- met_train()
    met_test <- met_test()
    df <- res$`Group Mean Train and Test`
    df <- cbind(SampleName = rownames(df),df)
    colnames(df)[1] <- colnames(met_train)[1]
    colnames(met_test)[1] <- colnames(met_train)[1]
    meta_merge <- merge(met_train[,which(colnames(met_train) %in% c(colnames(met_train)[1],meta_cols))],
                        met_test[,which(colnames(met_test) %in% c(colnames(met_test)[1],meta_cols))], all = T)
    if (any(meta_cols %in% colnames(meta_merge))) {
      df <- merge(meta_merge,df, all.y = T)
      df <- df %>% relocate(any_of(meta_cols) , .after = Training_Testing)
    }
    df
  })
  output$TrainTestGroupCentersTab <- renderDataTable({
    req(pred_obj())
    req(TrainTestGroupCentersTab_react())
    res <- pred_obj()
    df <- TrainTestGroupCentersTab_react()
    if (input$TrainTestGroupCentersTabHead == "View table head") {
      df <- head(df,c(100,100))
    }
    datatable(df,
              options = list(lengthMenu = c(5,10, 20, 100, 1000),
                             pageLength = 20,
                             scrollX = T),
              rownames = F) %>%
      formatRound(columns = res$`Regression Model`$coefnames[-1], digits = 5)
  })
  ModelFittedTab_react <- reactive({
    req(pred_obj())
    res <- pred_obj()
    meta_cols <- input$AddMetaCols
    met_train <- met_train()
    df <- res$`Model Fitted`
    df <- cbind(SampleName = rownames(df),df)
    colnames(df)[1] <- colnames(met_train)[1]
    if (any(meta_cols %in% colnames(met_train))) {
      df <- merge(met_train[,c(colnames(met_train)[1],meta_cols)],df, all.y = T)
      df <- df %>% relocate(any_of(meta_cols) , .after = colnames(df)[1])
    }
    df
  })
  output$ModelFittedTab <- renderDataTable({
    req(pred_obj())
    req(ModelFittedTab_react())
    res <- pred_obj()
    df <- ModelFittedTab_react()
    if (input$ModelFittedTabHead == "View table head") {
      df <- head(df,c(100,100))
    }
    datatable(df,
              options = list(lengthMenu = c(5,10, 20, 100, 1000),
                             pageLength = 20,
                             scrollX = T),
              rownames = F) %>%
      formatRound(columns = colnames(res$`Model Fitted`), digits = 5)
  })
  TrainPredictionsTab_react <- reactive({
    req(pred_obj())
    res <- pred_obj()
    meta_cols <- input$AddMetaCols
    met_train <- met_train()
    df <- res$`Prediction Training`
    df <- df[order(df$Prediction),]
    colnames(df)[1] <- colnames(met_train)[1]
    if (any(meta_cols %in% colnames(met_train))) {
      df <- merge(met_train[,c(colnames(met_train)[1],meta_cols)],df, all.y = T)
      df <- df %>% relocate(any_of(meta_cols) , .after = Prediction)
    }
    df
  })
  output$TrainPredictionsTab <- renderDataTable({
    req(pred_obj())
    req(TrainPredictionsTab_react())
    res <- pred_obj()
    df <- TrainPredictionsTab_react()
    if (input$TrainPredictionsTabHead == "View table head") {
      df <- head(df,c(100,100))
    }
    datatable(df,
              options = list(lengthMenu = c(5,10, 20, 100, 1000),
                             pageLength = 20,
                             scrollX = T),
              rownames = F) %>%
      formatRound(columns = colnames(res$`Model Fitted`), digits = 5)
  })
  TestPredictionsTab_react <- reactive({
    req(pred_obj())
    res <- pred_obj()
    meta_cols <- input$AddMetaCols
    met_test <- met_test()
    df <- res$`Prediction Testing`
    df <- df[order(df$Prediction),]
    colnames(df)[1] <- colnames(met_test)[1]
    if (any(meta_cols %in% colnames(met_test))) {
      df <- merge(met_test[,c(colnames(met_test)[1],meta_cols)],df, all.y = T)
      df <- df %>% relocate(any_of(meta_cols) , .after = Prediction)
    }
    df
  })
  output$TestPredictionsTab <- renderDataTable({
    req(pred_obj())
    req(TestPredictionsTab_react())
    res <- pred_obj()
    df <- TestPredictionsTab_react()
    if (input$TestPredictionsTabHead == "View table head") {
      df <- head(df,c(100,100))
    }
    datatable(df,
              options = list(lengthMenu = c(5,10, 20, 100, 1000),
                             pageLength = 20,
                             scrollX = T),
              rownames = F) %>%
      formatRound(columns = colnames(res$`Model Fitted`), digits = 5)
  })
  TrainTestPredictionsTab_react <- reactive({
    req(pred_obj())
    res <- pred_obj()
    meta_cols <- input$AddMetaCols
    met_train <- met_train()
    met_test <- met_test()
    df <- res$`Prediction Training and Testing`
    df <- df[order(df$Prediction),]
    colnames(df)[1] <- colnames(met_train)[1]
    colnames(met_test)[1] <- colnames(met_train)[1]
    meta_merge <- merge(met_train[,which(colnames(met_train) %in% c(colnames(met_train)[1],meta_cols))],
                        met_test[,which(colnames(met_test) %in% c(colnames(met_test)[1],meta_cols))], all = T)
    if (any(meta_cols %in% colnames(meta_merge))) {
      df <- merge(meta_merge,df, all.y = T)
      df <- df %>% relocate(any_of(meta_cols) , .after = Prediction)
    }
    df
  })
  output$TrainTestPredictionsTab <- renderDataTable({
    req(pred_obj())
    req(TrainTestPredictionsTab_react())
    res <- pred_obj()
    df <- TrainTestPredictionsTab_react()
    if (input$TrainTestPredictionsTabHead == "View table head") {
      df <- head(df,c(100,100))
    }
    datatable(df,
              options = list(lengthMenu = c(5,10, 20, 100, 1000),
                             pageLength = 20,
                             scrollX = T),
              rownames = F) %>%
      formatRound(columns = colnames(res$`Model Fitted`), digits = 5)
  })
  
  ## Boxplot -------------------------------------------------------------------
  
  ### Data ---------------------------------------------------------------------
  
  met_train_ext <- reactive({
    req(met_train())
    req(TrainGroupCentersTab_react())
    req(TrainPredictionsTab_react())
    req(pred_obj())
    met_train <- met_train()
    centers_train <- TrainGroupCentersTab_react()
    pred_train <- TrainPredictionsTab_react()
    res <- pred_obj()
    group_choices <- colnames(res$`Group Center Training`)[-1]
    fit_choices <- colnames(res$`Model Fitted`)
    
    colnames(centers_train)[which(colnames(centers_train) %in% group_choices)] <-
      paste0(colnames(centers_train)[which(colnames(centers_train) %in% group_choices)],"_Center")
    colnames(pred_train)[which(colnames(pred_train) %in% fit_choices)] <-
      paste0(colnames(pred_train)[which(colnames(pred_train) %in% fit_choices)],"_Fitted")
    
    met_train_ext <- Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, all = TRUE),
                            list(met_train,centers_train,pred_train))
    met_train_ext <- met_train_ext %>% relocate(!any_of(grep("_Center$|_Fitted$",colnames(met_train_ext), value = T)))
    met_train_ext
  })
  met_test_ext <- reactive({
    req(met_test())
    req(TestGroupCentersTab_react())
    req(TestPredictionsTab_react())
    req(pred_obj())
    met_test <- met_test()
    centers_test <- TestGroupCentersTab_react()
    pred_test <- TestPredictionsTab_react()
    res <- pred_obj()
    group_choices <- colnames(res$`Group Center Testing`)
    fit_choices <- colnames(res$`Model Fitted`)
    colnames(centers_test)[which(colnames(centers_test) %in% group_choices)] <-
      paste0(colnames(centers_test)[which(colnames(centers_test) %in% group_choices)],"_Center")
    colnames(pred_test)[which(colnames(pred_test) %in% fit_choices)] <-
      paste0(colnames(pred_test)[which(colnames(pred_test) %in% fit_choices)],"_Fitted")
    
    met_test_ext <- Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, all = TRUE),
                           list(met_test,centers_test,pred_test))
    met_test_ext <- met_test_ext %>% relocate(!any_of(grep("_Center$|_Fitted$",colnames(met_test_ext), value = T)))
    met_test_ext
  })
  met_tt_ext <- reactive({
    req(met_train_ext())
    req(met_test_ext())
    met_train_ext <- met_train_ext()
    met_test_ext <- met_test_ext()
    met_tt_ext <- merge(met_train_ext,met_test_ext, all = T)
    met_tt_ext
  })
  
  observe({
    req(input$PredGroupCenters)
    req(input$FeatColumn)
    met_train <- met_train_ext()
    feature <- input$FeatColumn
    
    updateSelectizeInput(session, "BPgroupCriteriaTrain", choices = colnames(met_train)[-1], selected = feature, server = T)
    if (input$ttOrsplit != "Use Training Data") {
      met_test <- met_test_ext()
      met_tt <- met_tt_ext()
      updateSelectizeInput(session, "BPgroupCriteriaTest", choices = colnames(met_test)[-1], selected = feature, server = T)
      updateSelectizeInput(session, "BPgroupCriteriaTT", choices = colnames(met_tt)[-1], selected = feature, server = T)
    }
  })
  observe({
    req(input$BPgroupCriteriaTrain)
    req(input$PredGroupCenters)
    met_train <- met_train_ext()
    bpGroup_train <- input$BPgroupCriteriaTrain
    
    group_choices_train <- unique(met_train[,bpGroup_train])
    group_choices_train[is.na(group_choices_train)] <- "NA"
    
    updateSelectizeInput(session, "BPgroupSelectionTrain", choices = group_choices_train, selected = group_choices_train)
    if (input$ttOrsplit != "Use Training Data") {
      req(input$BPgroupCriteriaTest)
      req(input$BPgroupCriteriaTT)
      bpGroup_test <- input$BPgroupCriteriaTest
      bpGroup_tt <- input$BPgroupCriteriaTT
      met_test <- met_test_ext()
      met_tt <- met_tt_ext()
      group_choices_test <- unique(met_test[,bpGroup_test])
      group_choices_test[is.na(group_choices_test)] <- "NA"
      group_choices_tt <- unique(met_tt[,bpGroup_tt])
      group_choices_tt[is.na(group_choices_tt)] <- "NA"
      updateSelectizeInput(session, "BPgroupSelectionTest", choices = group_choices_test, selected = group_choices_test)
      updateSelectizeInput(session, "BPgroupSelectionTT", choices = group_choices_tt, selected = group_choices_tt)
    }
  })
  observe({
    req(input$PredGroupCenters)
    met_train <- met_train_ext()
    
    feat_choices_train <- names(which(unlist(lapply(met_train, is.numeric))==TRUE))
    
    updateSelectizeInput(session, "BPFeatSelectionTrain", choices = feat_choices_train, selected = "Group1_Center", server = T)
    if (input$ttOrsplit != "Use Training Data") {
      met_test <- met_test_ext()
      met_tt <- met_tt_ext()
      feat_choices_test <- names(which(unlist(lapply(met_test, is.numeric))==TRUE))
      feat_choices_tt <- names(which(unlist(lapply(met_tt, is.numeric))==TRUE))
      updateSelectizeInput(session, "BPFeatSelectionTest", choices = feat_choices_test, selected = "Group1_Center", server = T)
      updateSelectizeInput(session, "BPFeatSelectionTT", choices = feat_choices_tt, selected = "Group1_Center", server = T)
    }
  })
  
  bp_df_training <- reactive({
    req(met_train_ext())
    req(input$BPgroupCriteriaTrain)
    req(input$BPgroupSelectionTrain)
    req(input$BPFeatSelectionTrain)
    met_train <- met_train_ext()
    group_crit <- input$BPgroupCriteriaTrain
    group_sel <- input$BPgroupSelectionTrain
    feature_sel <- input$BPFeatSelectionTrain
    
    bp_df_training <- met_train[,c(colnames(met_train)[1],group_crit,feature_sel)]
    bp_df_training[which(is.na(bp_df_training[,group_crit])),group_crit] <- "NA"
    bp_df_training <- bp_df_training[which(bp_df_training[,group_crit] %in% group_sel),]
    bp_df_training
  })
  bp_df_testing <- reactive({
    req(met_test_ext())
    req(input$BPgroupCriteriaTest)
    req(input$BPgroupSelectionTest)
    req(input$BPFeatSelectionTest)
    met_test <- met_test_ext()
    group_crit <- input$BPgroupCriteriaTest
    group_sel <- input$BPgroupSelectionTest
    feature_sel <- input$BPFeatSelectionTest
    
    bp_df_testing <- met_test[,c(colnames(met_test)[1],group_crit,feature_sel)]
    bp_df_testing[which(is.na(bp_df_testing[,group_crit])),group_crit] <- "NA"
    bp_df_testing <- bp_df_testing[which(bp_df_testing[,group_crit] %in% group_sel),]
    bp_df_testing
  })
  bp_df_tt <- reactive({
    req(met_tt_ext())
    req(input$BPgroupCriteriaTT)
    req(input$BPgroupSelectionTT)
    req(input$BPFeatSelectionTT)
    met_tt <- met_tt_ext()
    group_crit <- input$BPgroupCriteriaTT
    group_sel <- input$BPgroupSelectionTT
    feature_sel <- input$BPFeatSelectionTT
    
    bp_df_tt <- met_tt[,c(colnames(met_tt)[1],group_crit,feature_sel)]
    bp_df_tt[which(is.na(bp_df_tt[,group_crit])),group_crit] <- "NA"
    bp_df_tt <- bp_df_tt[which(bp_df_tt[,group_crit] %in% group_sel),]
    bp_df_tt
  })
  
  #observe({
  #  req(met_tt_ext())
  #  req(input$BPgroupCriteriaTT)
  #  req(input$BPgroupSelectionTT)
  #  req(input$BPFeatSelectionTT)
  #  met_tt <- met_tt_ext()
  #  group_crit <- input$BPgroupCriteriaTT
  #  group_sel <- input$BPgroupSelectionTT
  #  feature_sel <- input$BPFeatSelectionTT
  #  print(head(met_tt))
  #  print(group_crit)
  #  print(group_sel)
  #  print(feature_sel)
  #})
  
  ### Plots --------------------------------------------------------------------
  
  TrainGroupCentersBP_react <- reactive({
    req(bp_df_training())
    plot_df <- bp_df_training()
    group <- colnames(plot_df)[2]
    feature <- colnames(plot_df)[3]
    stat <- input$BPplotstatCompTrain
    dot_choice <- input$BPplotsampledotsTrain
    dot_size <- input$BPplotDotSizeTrain
    xOrder <- input$BPplotXaxOrderTrain
    bpFlip <- input$BPflipBPTrain
    bpOrVi <- input$BPorViolinTrain
    bpOrVi <- ifelse(bpOrVi == "Violin Plot",TRUE,FALSE)
    
    bp <- make_boxplot(plot_df,group, feature, x_title = group, y_title = feature,
                       stat = stat, dots = dot_choice, dot_size = dot_size, violin = bpOrVi,
                       flip = bpFlip, x_order = xOrder, title = paste0(feature," Across ",group," In Training Data"))
    bp
  })
  output$TrainGroupCentersBP <- renderPlot({
    req(TrainGroupCentersBP_react())
    bp <- TrainGroupCentersBP_react()
    bp
  })
  TestGroupCentersBP_react <- reactive({
    req(bp_df_testing())
    plot_df <- bp_df_testing()
    group <- colnames(plot_df)[2]
    feature <- colnames(plot_df)[3]
    stat <- input$BPplotstatCompTest
    dot_choice <- input$BPplotsampledotsTest
    dot_size <- input$BPplotDotSizeTest
    xOrder <- input$BPplotXaxOrderTest
    bpFlip <- input$BPflipBPTest
    bpOrVi <- input$BPorViolinTest
    bpOrVi <- ifelse(bpOrVi == "Violin Plot",TRUE,FALSE)
    
    bp <- make_boxplot(plot_df,group, feature, x_title = group, y_title = feature,
                       stat = stat, dots = dot_choice, dot_size = dot_size, violin = bpOrVi,
                       flip = bpFlip, x_order = xOrder, title = paste0(feature," Across ",group," In Testing Data"))
    bp
  })
  output$TestGroupCentersBP <- renderPlot({
    req(TestGroupCentersBP_react())
    bp <- TestGroupCentersBP_react()
    bp
  })
  TrainTestGroupCentersBP_react <- reactive({
    req(bp_df_tt())
    plot_df <- bp_df_tt()
    group <- colnames(plot_df)[2]
    feature <- colnames(plot_df)[3]
    stat <- input$BPplotstatCompTT
    dot_choice <- input$BPplotsampledotsTT
    dot_size <- input$BPplotDotSizeTT
    xOrder <- input$BPplotXaxOrderTT
    bpFlip <- input$BPflipBPTT
    bpOrVi <- input$BPorViolinTT
    bpOrVi <- ifelse(bpOrVi == "Violin Plot",TRUE,FALSE)
    
    bp <- make_boxplot(plot_df,group, feature, x_title = group, y_title = feature,
                       stat = stat, dots = dot_choice, dot_size = dot_size, violin = bpOrVi,
                       flip = bpFlip, x_order = xOrder, title = paste0(feature," Across ",group," In Training and Testing Data"))
    bp
  })
  output$TrainTestGroupCentersBP <- renderPlot({
    req(TrainTestGroupCentersBP_react())
    bp <- TrainTestGroupCentersBP_react()
    bp
  })
  
  # Prediction Analysis --------------------------------------------------------
  
  observe({
    req(met_train())
    met_train <- met_train()
    featChoices <- c("Select All Samples",colnames(met_train)[-1])
    updateSelectizeInput(session,"SubsetCol",choices = featChoices, server = T)
  })
  output$rendSubsetFeat <- renderUI({
    req(met_train())
    req(input$SubsetCol)
    if (input$SubsetCol != "Select All Samples") {
      met_train <- met_train()
      SubsetCol <- input$SubsetCol
      valChoices <- unique(met_train[,SubsetCol])
      selectInput("SubsetFeat","Subset Criteria:", choices = valChoices, selected = NULL, multiple = T)
    }
    
  })
  
  confusMat_react <- reactive({
    req(pred_obj())
    res <- pred_obj()
    pred_train_data <- res$`Prediction Training`
    meta_train <- res$data$meta_train
    meta_test <- res$data$meta_test
    SubsetCol <- input$SubsetCol
    SubsetFeat <- input$SubsetFeat
    if (input$SubsetCol != "Select All Samples" & isTruthy(SubsetFeat)) {
      meta_train_sub <- meta_train[which(meta_train[,SubsetCol] %in% SubsetFeat),]
      pred_train_data <- pred_train_data[meta_train_sub[,1],]
    }
    feature_train <- pred_train_data[,2]
    feature_pred <- pred_train_data[,3]
    feature_union <- union(feature_pred, feature_train)
    #save(list = ls(), file = "confmat.RData", envir = environment())
    confMat <- caret::confusionMatrix(data = factor(feature_pred,feature_union), reference = factor(feature_train,feature_union))
    confMat
  })
  
  confMat_heat_react <- reactive({
    req(confusMat_react())
    req(pred_obj())
    res <- pred_obj()
    confMat <- confusMat_react()
    tab <- as.data.frame(confMat$table)
    feature <- res$data$feature
    p <- ggplot(tab, aes(x=Prediction, y=Reference, fill=Freq)) +
      geom_tile() + theme_bw() + coord_equal() +
      scale_fill_distiller(palette="Blues", direction=1) +
      guides(fill="none") + # removing legend for `fill`
      labs(title = paste(feature,"Distribution")) + # using a title instead
      geom_text(aes(label=Freq), color="black", fontface = "bold") +
      theme(axis.title = element_text(size = 16),
            axis.text.x = element_text(size = 14, angle = 45, vjust = 1, hjust = 1),
            axis.text.y = element_text(size = 14),
            title = element_text(size = 18),
            plot.margin = unit(c(0,0,0,2), "cm"))
  })
  output$confMat_heat <- renderPlot({
    req(confMat_heat_react())
    p <- confMat_heat_react()
    p
  })
  
  output$confMat_summ <- renderTable({
    req(confusMat_react())
    confMat <- confusMat_react()
    overall <- confMat$overall
    overall <- as.data.frame((cbind(names(overall),overall)))
    overall
  }, rownames = FALSE, colnames = FALSE)
  
  output$confMat_Featsumm <- renderTable({
    req(confusMat_react())
    req(pred_obj())
    res <- pred_obj()
    confMat <- confusMat_react()
    feature <- res$data$feature
    featSumm <- as.data.frame(confMat$byClass)
    if (ncol(featSumm) == 1) {
      colnames(featSumm) <- feature
    }
    featSumm
  }, digits = 4, rownames = TRUE)
  
  
  # Downloads ------------------------------------------------------------------
  ## Training Model ----------------------------------------------------------
  output$dnldModel <- downloadHandler(
    filename = function() {
      feature <- input$FeatColumn
      ref <- input$FeatRef
      clusters <- input$predictK
      paste0("Predict_", feature,"_Ref_",ref,"_",clusters,"_Clusters_RegressionModel_",Sys.Date(),".rds")
    },
    content = function(file) {
      res <- pred_obj()
      model <- res$`Regression Model`
      saveRDS(model, file = file)
    }
  )
  output$dnldTrainMat2 <- downloadHandler(
    filename = function() {
      feature <- input$FeatColumn
      ref <- input$FeatRef
      clusters <- input$predictK
      paste0("Predict_", feature,"_Ref_",ref,"_",clusters,"_Clusters_TrainingMatrix_",Sys.Date(),".txt")
    },
    content = function(file) {
      res <- pred_obj()
      df <- res$data$data_train
      samplenamecol <- colnames(res$data$meta_train)[1]
      df <- cbind(rownames(df),df)
      colnames(df)[1] <- samplenamecol
      write.table(df,file,sep = '\t', row.names = F)
    }
  )
  output$dnldTrainMet2 <- downloadHandler(
    filename = function() {
      feature <- input$FeatColumn
      ref <- input$FeatRef
      clusters <- input$predictK
      paste0("Predict_", feature,"_Ref_",ref,"_",clusters,"_Clusters_TrainingMeta_",Sys.Date(),".txt")
    },
    content = function(file) {
      res <- pred_obj()
      df <- res$data$meta_train
      write.table(df,file,sep = '\t', row.names = F)
    }
  )
  
  ## Boxplots ------------------------------------------------------------------
  
  output$dnldTrainGroupCentersBP <- downloadHandler(
    filename = function() {
      feature <- input$FeatColumn
      ref <- input$FeatRef
      clusters <- input$predictK
      paste0("Predict_", feature,"_Ref_",ref,"_",clusters,"_Clusters_TrainingBoxplot_",Sys.Date(),".svg")
    },
    content = function(file) {
      bp <- TrainGroupCentersBP_react()
      ggsave(file,bp,height = 8, width = 10)
    }
  )
  output$dnldTestGroupCentersBP <- downloadHandler(
    filename = function() {
      feature <- input$FeatColumn
      ref <- input$FeatRef
      clusters <- input$predictK
      paste0("Predict_", feature,"_Ref_",ref,"_",clusters,"_Clusters_TestingBoxplot_",Sys.Date(),".svg")
    },
    content = function(file) {
      bp <- TestGroupCentersBP_react()
      ggsave(file,bp,height = 8, width = 10)
    }
  )
  output$dnldTrainTestGroupCentersBP <- downloadHandler(
    filename = function() {
      feature <- input$FeatColumn
      ref <- input$FeatRef
      clusters <- input$predictK
      paste0("Predict_", feature,"_Ref_",ref,"_",clusters,"_Clusters_TrainTestBoxplot_",Sys.Date(),".svg")
    },
    content = function(file) {
      bp <- TrainTestGroupCentersBP_react()
      ggsave(file,bp,height = 8, width = 10)
    }
  )
  
  ## Prediction Tab Tables -----------------------------------------------------
  output$dnldTrainDataClusterCoeffTab <- downloadHandler(
    filename = function() {
      feature <- input$FeatColumn
      ref <- input$FeatRef
      clusters <- input$predictK
      paste0("Predict_", feature,"_Ref_",ref,"_",clusters,"_Clusters_ClusterCoefficients_",Sys.Date(),".txt")
    },
    content = function(file) {
      df <- TrainDataClusterCoeffTab_react()
      write.table(df,file, sep = '\t', row.names = F)
    }
  )
  output$dnldTrainDataClustersTab <- downloadHandler(
    filename = function() {
      feature <- input$FeatColumn
      ref <- input$FeatRef
      clusters <- input$predictK
      paste0("Predict_", feature,"_Ref_",ref,"_",clusters,"_Clusters_ClusterResutlts_",Sys.Date(),".txt")
    },
    content = function(file) {
      res <- pred_obj()
      df <- data.frame(Feature = names(res$clusters),
                       Cluster = res$clusters)
      df <- df[order(df$Cluster),]
      write.table(df,file, sep = '\t', row.names = F)
    }
  )
  output$dnldTrainGroupCentersTab <- downloadHandler(
    filename = function() {
      feature <- input$FeatColumn
      ref <- input$FeatRef
      clusters <- input$predictK
      paste0("Predict", feature,"_Ref",ref,"_",clusters,"Clusters_TrainingGroupCenters_",Sys.Date(),".txt")
    },
    content = function(file) {
      res <- pred_obj()
      df <- res$`Group Center Training`
      df <- cbind(SampleName = rownames(df),df)
      df <- df[order(as.character(df$CategoricalSubgroup)),]
      write.table(df,file, sep = '\t', row.names = F)
    }
  )
  output$dnldTestGroupCentersTab <- downloadHandler(
    filename = function() {
      feature <- input$FeatColumn
      ref <- input$FeatRef
      clusters <- input$predictK
      paste0("Predict", feature,"_Ref",ref,"_",clusters,"Clusters_TestingGroupCenters_",Sys.Date(),".txt")
    },
    content = function(file) {
      res <- pred_obj()
      df <- res$`Group Center Testing`
      df <- cbind(SampleName = rownames(df),df)
      write.table(df,file, sep = '\t', row.names = F)
    }
  )
  output$dnldTrainTestGroupCentersTab <- downloadHandler(
    filename = function() {
      feature <- input$FeatColumn
      ref <- input$FeatRef
      clusters <- input$predictK
      paste0("Predict", feature,"_Ref",ref,"_",clusters,"Clusters_TrainingTestingGroupCenters_",Sys.Date(),".txt")
    },
    content = function(file) {
      res <- pred_obj()
      df <- res$`Group Mean Train and Test`
      df <- cbind(SampleName = rownames(df),df)
      write.table(df,file, sep = '\t', row.names = F)
    }
  )
  output$dnldModelFittedTab <- downloadHandler(
    filename = function() {
      feature <- input$FeatColumn
      ref <- input$FeatRef
      clusters <- input$predictK
      paste0("Predict", feature,"_Ref",ref,"_",clusters,"Clusters_ModelFitted_",Sys.Date(),".txt")
    },
    content = function(file) {
      res <- pred_obj()
      df <- res$`Model Fitted`
      df <- cbind(SampleName = rownames(df),df)
      write.table(df,file, sep = '\t', row.names = F)
    }
  )
  output$dnldTrainPredictionsTab <- downloadHandler(
    filename = function() {
      feature <- input$FeatColumn
      ref <- input$FeatRef
      clusters <- input$predictK
      paste0("Predict", feature,"_Ref",ref,"_",clusters,"Clusters_TrainingPredictions_",Sys.Date(),".txt")
    },
    content = function(file) {
      res <- pred_obj()
      df <- res$`Prediction Training`
      df <- cbind(SampleName = rownames(df),df)
      df <- df %>% relocate(Prediction, .after = SampleName)
      df <- df[order(df$Prediction),]
      write.table(df,file, sep = '\t', row.names = F)
    }
  )
  output$dnldTestPredictionsTab <- downloadHandler(
    filename = function() {
      feature <- input$FeatColumn
      ref <- input$FeatRef
      clusters <- input$predictK
      paste0("Predict", feature,"_Ref",ref,"_",clusters,"Clusters_TestingPredictions_",Sys.Date(),".txt")
    },
    content = function(file) {
      res <- pred_obj()
      df <- res$`Prediction Testing`
      df <- cbind(SampleName = rownames(df),df)
      df <- df %>% relocate(Prediction, .after = SampleName)
      df <- df[order(df$Prediction),]
      write.table(df,file, sep = '\t', row.names = F)
    }
  )
  output$dnldTrainTestPredictionsTab <- downloadHandler(
    filename = function() {
      feature <- input$FeatColumn
      ref <- input$FeatRef
      clusters <- input$predictK
      paste0("Predict", feature,"_Ref",ref,"_",clusters,"Clusters_TrainingTestingPredictions_",Sys.Date(),".txt")
    },
    content = function(file) {
      res <- pred_obj()
      df <- res$`Prediction Training and Testing`
      df <- cbind(SampleName = rownames(df),df)
      df <- df %>% relocate(Prediction, .after = SampleName)
      df <- df[order(df$Prediction),]
      write.table(df,file, sep = '\t', row.names = F)
    }
  )
  
  
}



# Run the application
shinyApp(ui = ui, server = server)