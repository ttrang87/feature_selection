library(shiny)
library(DT)
library(randomForestSRC)
library(bnlearn)
library(shinyjs)
library(mice)
library(glmnet)
library(shinyWidgets)
library(shinyalert)
library(shinycssloaders)

source("model_test.R")
source("column_mapper_module.R")

ui <- fluidPage(
  useShinyjs(),
  
  titlePanel("Feature Selection Explorer"),
  
  tabsetPanel(
    id = "main_tabs",
    
    tabPanel("Upload & Select Target",
             sidebarLayout(
               sidebarPanel(
                 radioButtons(
                   "upload_mode",
                   "Upload Mode:",
                   choices = c("Single File (Auto Split)" = "single",
                               "Separate Train & Test Files" = "dual"),
                   selected = "single"
                 ),
                 
                 conditionalPanel(
                   condition = "input.upload_mode == 'single'",
                   fileInput("file", "Upload Data File (.csv / .tsv)",
                             accept = c(".csv", ".tsv")),
                   actionButton("load_example", "Load Example", 
                                class = "btn btn-default", 
                                style = "width: 100%; margin-top: -20px; margin-bottom: 10px; background-color: #6c757d; color: white;"),
                   actionButton("open_column_mapper", "Map Column Values", 
                                class = "btn btn-info", 
                                style = "width: 100%; margin-bottom: 10px;"),
                   helpText("Data will be split using the ratio in Model AUC Results tab.")
                 ),
                 
                 conditionalPanel(
                   condition = "input.upload_mode == 'dual'",
                   fileInput("file_train", "Upload Training Data (.csv / .tsv)",
                             accept = c(".csv", ".tsv")),
                   fileInput("file_test", "Upload Test Data (.csv / .tsv)",
                             accept = c(".csv", ".tsv")),
                   helpText("Files must have identical columns."),
                 ),
                 
                 
                 # Target: selectizeInput (choices populated server-side)
                 selectizeInput(
                   "target_col",
                   "Select Target Column",
                   choices = NULL,
                   options = list(
                     placeholder = 'Search or select a target column...',
                     maxOptions = 1000
                   )
                 ),
                 
                 # Predictor picker — static in UI (updated via updatePickerInput)
                 pickerInput(
                   "predictors",
                   "Select Predictor Columns",
                   choices = NULL,
                   multiple = TRUE,
                   options = list(
                     `actions-box` = TRUE,
                     `live-search` = TRUE,
                     size = 8,
                     `selectedTextFormat` = 'count > 3',
                     `container` = 'body'
                   )
                 ),
                 
            
                 actionButton("apply_predictors", "Apply Predictors", 
                              class = "btn btn-secondary", 
                              style = "width: 100%;"),
                 br(), br(),
                 
  
                 br(),
                 actionButton("run", "Run Feature Selection", 
                              class = "btn btn-primary",
                              style = "width: 100%;")
               ),
               mainPanel(
                 uiOutput("data_summary_boxes"),
                 br(),
                 
                 h4("Data Preview"),
                 conditionalPanel(
                   condition = "input.upload_mode == 'dual'",
                   tabsetPanel(
                     id = "data_preview_tabs",
                     tabPanel("Training Data", br(), withSpinner(DTOutput("train_preview"), type = 6)),
                     tabPanel("Test Data", br(), withSpinner(DTOutput("test_preview"), type = 6))
                     
                   )
                 ),
                 conditionalPanel(
                   condition = "input.upload_mode == 'single'",
                   withSpinner(DTOutput("data_preview"), type = 6)
                 ),
                 br(),
                 uiOutput("nav_buttons")
               )
             )
    ),
    
    tabPanel("Pearson Correlation",
             h3("Correlation with Target"),
             
             # Side-by-side layout for plot and table
             fluidRow(
               column(6,
                      DTOutput("cor_table")
               ),
               column(6,
                      plotOutput("cor_plot", height = "400px")
               )
             ),
             
             br(),
             uiOutput("nav_buttons")
    ),
    
    
    tabPanel("Random Forest",
             h3("Random Forest Variable Importance"),
             numericInput("rf_ntree", "Number of trees (ntree):", value = 500, min = 1, step = 1),
             actionButton("run_rf", "Run Random Forest", class = "btn btn-primary"),
             br(), br(),
             
             # Side-by-side layout
             fluidRow(
               column(6,
                      DTOutput("rf_table")
               ),
               column(6,
                      plotOutput("rf_plot",  height = "400px")  # keep default height
               )
             ),
             
             br(),
             uiOutput("nav_buttons")
    ),
    
    
    tabPanel("Markov Blanket",
             mainPanel(
               h3("Markov Blanket Results"),
               DTOutput("mb_table"),
               br(),
               uiOutput("nav_buttons")
             )
    ),
    
    tabPanel("LASSO",
             h3("LASSO Feature Selection"),
             selectInput("lasso_lambda_choice", "Choose Lambda:",
                         choices = c("lambda.min", "lambda.1se"), selected = "lambda.min"),
             actionButton("run_lasso", "Run LASSO", class = "btn btn-primary"),
             br(), br(),
             tableOutput("debug_lasso"),
             br(),
             
             # Side-by-side layout
             fluidRow(
               column(6,
                      DTOutput("lasso_table")
               ),
               column(6,
                      plotOutput("lasso_plot", height = "400px")
               )
             ),
             
             br(),
             uiOutput("nav_buttons")
    ),
    
    
    tabPanel("Final Selection",
             numericInput("topx", "Number of features you want to select:", value = 15, min = 1),
             downloadButton("download_final_table", "Download Final Table",
                            style = "margin-bottom: 10px;"),
             DTOutput("final_table"),
             br(),
             uiOutput("nav_buttons")
    ),
    
    tabPanel(
      "Model AUC Results",
      h3("AUC comparison and ROC curves"),
      fluidRow(
        # LEFT: parameter input with tabs
        column(
          width = 4,
          wellPanel(
            h4("Model Parameters"),
            
            tabsetPanel(
              id = "model_params_tabs",
              
              # General Settings Tab
              tabPanel(
                "General",
                br(),
                conditionalPanel(
                  condition = "input.upload_mode == 'single'",
                  numericInput("split_ratio", "Train-Test Split Ratio:", 
                               value = 0.8, min = 0.5, max = 0.95, step = 0.01),
                  helpText("Proportion of data used for training.")
                ),
                conditionalPanel(
                  condition = "input.upload_mode == 'dual'",
                  div(
                    style = "padding: 10px; background-color: #f0f0f0; border-radius: 5px; margin-bottom: 10px;",
                    strong("Split Ratio: Using uploaded train/test files"),
                    br(),
                    helpText("Split ratio is determined by your uploaded files and cannot be changed.")
                  )
                ),
                numericInput("cv_folds", "Number of CV Folds:", 
                             value = 5, min = 2, max = 20, step = 1)
              ),
              
              # Logistic Regression Tab
              tabPanel(
                "Logistic",
                br(),
                h5("Logistic Regression"),
                helpText("No tunable parameters for logistic regression."),
                br(),
                h5("Best Performance History"),
                DTOutput("logistic_best_table")
              ),
              
              # Random Forest Tab
              tabPanel(
                "Random Forest",
                br(),
                numericInput("rf_ntree_model", "Number of Trees:", 
                             value = 500, min = 10, max = 2000, step = 50),
                br(),
                h5("Best Performance History"),
                DTOutput("rf_best_table")
              ),
              
              # XGBoost Tab
              tabPanel(
                "XGBoost",
                br(),
                numericInput("xgb_eta", "Learning Rate (eta):", 
                             value = 0.05, min = 0.001, max = 1, step = 0.01),
                numericInput("xgb_max_depth", "Max Depth:", 
                             value = 6, min = 1, max = 20, step = 1),
                br(),
                h5("Best Performance History"),
                DTOutput("xgb_best_table")
              )
            ),
            
            br(),
            # --- Action Button ---
            actionButton("rerun_models", "Run Models with Current Parameters",
                         class = "btn btn-primary", style = "width: 100%; margin-top: 10px;"),
            actionButton("clear_history", "Clear History",
                         class = "btn btn-warning", style = "width: 100%; margin-top: 5px;")
            
          )
        ),
        
        # RIGHT: table (top) + plot (bottom) 
        column(
          width = 8,
          div(
            style = "display: flex; flex-direction: column; height: 100%;",
            
            # Top: Tabbed metric tables
            div(
              style = "flex: 1; overflow-y: auto; margin-bottom: 10px;",
              tabsetPanel(
                id = "metrics_tabs",
                tabPanel("AUC", br(), DTOutput("auc_table")),
                tabPanel("MCC", br(), DTOutput("mcc_table")),
                tabPanel("Recall", br(), DTOutput("recall_table")),
                tabPanel("Precision", br(), DTOutput("precision_table")),
                tabPanel("F1 Score", br(), DTOutput("f1_table"))
              )
            ),
            
            div(
              style = "margin-bottom: 10px; padding: 10px; border: 1px solid #ddd; border-radius: 5px; background-color: #f9f9f9;",
              h5("Add Custom Subset"),
              fluidRow(
                column(6,
                       textInput("custom_subset_name", "Subset Name:", placeholder = "e.g., My_Custom_Subset")
                ),
                column(6,
                       pickerInput(
                         "custom_subset_features",
                         "Select Features:",
                         choices = NULL,
                         multiple = TRUE,
                         options = list(
                           `actions-box` = TRUE,
                           `live-search` = TRUE,
                           size = 5,
                           `selectedTextFormat` = 'count > 3'
                         )
                       )
                )
              ),
              actionButton("add_custom_subset", "Add Custom Subset", class = "btn btn-success", style = "width: 100%;")
            ),
            
            # Bottom: ROC and SHAP plots side by side
            div(
              style = "display: flex; justify-content: space-around; margin-top: 20px;",
              div(
                style = "text-align: center;",
                plotOutput("roc_plot", width = "400px", height = "400px")
              ),
              div(
                style = "text-align: center;",
                actionButton("calc_shap", "Calculate SHAP", class = "btn btn-info", 
                             style = "margin-bottom: 10px; width: 300px;"),
                br(),
                withSpinner(plotOutput("shap_plot", width = "400px", height = "400px"), type = 6)
              )
            )
          )
        )
      )
    )
    
    
    
  )
)

server <- function(input, output, session) {
  # --- Store data and mappings ---
  data_raw <- reactiveVal(NULL)
  train_raw <- reactiveVal(NULL)
  test_raw <- reactiveVal(NULL)
  processed_data <- reactiveVal(NULL)
  processed_train <- reactiveVal(NULL)
  processed_test <- reactiveVal(NULL)
  upload_mode <- reactiveVal("single")
  target_mapping <- reactiveVal(NULL)
  applied_predictors <- reactiveVal(character(0)) # store currently applied predictor set
  target_unique_values <- reactiveVal(NULL)
  # Reactive value to hold data for mapper
  data_reactive <- reactiveVal(NULL)
  
  lasso_trigger <- reactiveVal(0)
  
  
  # Initialize column mapper module
  mapper <- columnMapperServer("mapper", data_reactive)
  rv <- reactiveValues(
    roc_data = NULL,
    custom_subsets = list(),
    current_model_info = NULL, 
    shap_values = NULL,   
    cv_lasso = NULL,        
    df_lasso = NULL,
    logistic_history = data.frame(
      Subset = character(),
      AUC = numeric(),
      split_ratio = numeric(),
      stringsAsFactors = FALSE
    ),
    rf_history = data.frame(
      Subset = character(),
      AUC = numeric(),
      ntree = numeric(),
      split_ratio = numeric(),
      stringsAsFactors = FALSE
    ),
    xgb_history = data.frame(
      Subset = character(),
      AUC = numeric(),
      eta = numeric(),
      max_depth = numeric(),
      cv_folds = numeric(),
      split_ratio = numeric(),
      stringsAsFactors = FALSE
    )
  )
  
  
  output$logistic_best_table <- renderDT({
    req(nrow(rv$logistic_history) > 0)
    best_df <- rv$logistic_history[order(-rv$logistic_history$AUC), ]
    datatable(
      best_df,
      options = list(pageLength = 5, dom = 't', scrollX = TRUE),
      rownames = FALSE
    ) %>% formatRound(c('AUC', 'split_ratio'), 4)
  })
  
  output$rf_best_table <- renderDT({
    req(nrow(rv$rf_history) > 0)
    best_df <- rv$rf_history[order(-rv$rf_history$AUC), ]
    datatable(
      best_df,
      options = list(pageLength = 5, dom = 't', scrollX = TRUE),
      rownames = FALSE
    ) %>% formatRound(c('AUC', 'split_ratio'), 4)
  })
  
  output$xgb_best_table <- renderDT({
    req(nrow(rv$xgb_history) > 0)
    best_df <- rv$xgb_history[order(-rv$xgb_history$AUC), ]
    datatable(
      best_df,
      options = list(pageLength = 5, dom = 't', scrollX = TRUE),
      rownames = FALSE
    ) %>% formatRound(c('AUC', 'eta', 'split_ratio'), 4)
  })
  
  observeEvent(input$clear_history, {
    rv$logistic_history <- data.frame(
      Subset = character(),
      AUC = numeric(),
      split_ratio = numeric(),
      stringsAsFactors = FALSE
    )
    rv$rf_history <- data.frame(
      Subset = character(),
      AUC = numeric(),
      ntree = numeric(),
      split_ratio = numeric(),
      stringsAsFactors = FALSE
    )
    rv$xgb_history <- data.frame(
      Subset = character(),
      AUC = numeric(),
      eta = numeric(),
      max_depth = numeric(),
      cv_folds = numeric(),
      split_ratio = numeric(),
      stringsAsFactors = FALSE
    )
    shinyalert("History Cleared", "All performance history has been cleared.", type = "info")
  })
  
  # --- Upload data ---
  observeEvent(input$file, {
    req(input$file, input$upload_mode == "single")
    ext <- tools::file_ext(input$file$name)
    
    if (ext == "csv") {
      df <- read.csv(input$file$datapath, header = TRUE, sep = ",", stringsAsFactors = FALSE)
    } else if (ext == "tsv") {
      df <- read.delim(input$file$datapath, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
    } else {
      shinyalert("Invalid file", "Please upload a .csv or .tsv file.", type = "error")
      return()
    }
    
    # Convert logicals to numeric 0/1 for all columns (temporarily)
    logi_cols <- sapply(df, is.logical)
    if (any(logi_cols)) df[, logi_cols] <- lapply(df[, logi_cols, drop = FALSE], as.integer)
    
    data_raw(df)
    processed_data(df)
    data_reactive(df)
    train_raw(NULL)
    test_raw(NULL)
    processed_train(NULL)
    processed_test(NULL)
    target_mapping(NULL)
    applied_predictors(character(0))
    
    
    # Update target choices with ALL columns (no filtering)
    updateSelectizeInput(session, "target_col", choices = names(df), server = TRUE)
    target_unique_values(NULL)
    
    # Populate predictors choices (exclude current target if already present)
    updatePickerInput(session, "predictors", choices = setdiff(names(df), input$target_col), selected = NULL)
  })
  
  # --- Load Example Data ---
  observeEvent(input$load_example, {
    req(input$upload_mode == "single")
    
    df <- read.delim("TCGA.tsv", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
    
    logi_cols <- sapply(df, is.logical)
    if (any(logi_cols)) df[, logi_cols] <- lapply(df[, logi_cols, drop = FALSE], as.integer)
    
    data_raw(df)
    processed_data(df)
    data_reactive(df)
    train_raw(NULL)
    test_raw(NULL)
    processed_train(NULL)
    processed_test(NULL)
    target_mapping(NULL)
    applied_predictors(character(0))
    
    # Preselect target as "gender"
    if ("gender" %in% names(df)) {
      updateSelectizeInput(session, "target_col", choices = names(df), selected = "gender", server = TRUE)
    } else {
      updateSelectizeInput(session, "target_col", choices = names(df), server = TRUE)
    }
    
    target_unique_values(NULL)
    
    # Preselect all predictors except PatientID and gender
    all_cols <- names(df)
    preselected <- setdiff(all_cols, c("PatientID", "gender"))
    
    updatePickerInput(session, "predictors", 
                      choices = setdiff(all_cols, "gender"), 
                      selected = preselected)
    
    shinyalert("Example Loaded", 
               paste0("TCGA.tsv loaded successfully with ", nrow(df), " rows and ", ncol(df), " columns.\n\n",
                      "Target preselected: gender\n",
                      "Predictors preselected (excluding PatientID)"), 
               type = "success", timer = 3000)
  })
  
  
  # --- When target column selected, show unique values ---
  observeEvent(input$target_col, {
    req(input$target_col)
    
    df <- if (upload_mode() == "dual" && !is.null(train_raw())) {
      train_raw()
    } else {
      data_raw()
    }
    
    req(df)
    
    if (input$target_col %in% names(df)) {
      col <- df[[input$target_col]]
      unique_vals <- sort(unique(na.omit(col)))
      
      # Store unique values
      target_unique_values(unique_vals)
      
      # Update predictor choices (exclude target)
      updatePickerInput(
        session,
        "predictors",
        choices = setdiff(names(df), input$target_col)
      )
      
      # Reset when target changes
      target_mapping(NULL)
      applied_predictors(character(0))
    }
  }, ignoreNULL = FALSE)
  
  # --- Render UI for selecting positive class ---
  output$target_value_selector_ui <- renderUI({
    req(input$target_col, target_unique_values())
    
    unique_vals <- target_unique_values()
    
    if (length(unique_vals) == 0) {
      return(div(
        style = "color: red; margin-top: 10px;",
        "Error: Target column has no non-NA values."
      ))
    }
    
    if (length(unique_vals) == 1) {
      return(div(
        style = "color: orange; margin-top: 10px;",
        icon("exclamation-triangle"),
        " Warning: Target has only 1 unique value. Cannot create binary classification."
      ))
    }
    
    # Check if column is numeric
    df <- if (upload_mode() == "dual" && !is.null(train_raw())) {
      train_raw()
    } else {
      data_raw()
    }
    
    is_numeric_col <- is.numeric(df[[input$target_col]])
    
    tagList(
      div(
        style = "background-color: #f0f8ff; padding: 10px; border-radius: 5px; margin-top: 10px;",
        strong(paste("Found", length(unique_vals), "unique values in target column")),
        br(),
        helpText("Choose how to create binary mapping:")
      ),
      
      # Radio buttons to choose mapping method
      radioButtons(
        "mapping_method",
        "Mapping Method:",
        choices = if (is_numeric_col) {
          c("Select Specific Value" = "value",
            "Threshold (Greater Than)" = "threshold_gt",
            "Threshold (Less Than)" = "threshold_lt",
            "Threshold (Greater Than or Equal)" = "threshold_gte",
            "Threshold (Less Than or Equal)" = "threshold_lte")
        } else {
          c("Select Specific Value" = "value")
        },
        selected = "value"
      ),
      
      # Show selector for specific value method
      conditionalPanel(
        condition = "input.mapping_method == 'value'",
        selectInput(
          "positive_class_value",
          "Select Positive Class (mapped to 1):",
          choices = as.character(unique_vals),  # Convert to character
          selected = as.character(unique_vals[1])
        ),
        helpText("All other values will be mapped to 0.")
      ),
      
      # Show numeric input for threshold methods (only if numeric)
      if (is_numeric_col) {
        tagList(
          conditionalPanel(
            condition = "input.mapping_method == 'threshold_gt'",
            numericInput(
              "threshold_value",
              "Threshold Value:",
              value = median(unique_vals, na.rm = TRUE),
              step = if (all(unique_vals == floor(unique_vals))) 1 else 0.1
            ),
            helpText("Values GREATER THAN this threshold → 1, others → 0")
          ),
          
          conditionalPanel(
            condition = "input.mapping_method == 'threshold_lt'",
            numericInput(
              "threshold_value",
              "Threshold Value:",
              value = median(unique_vals, na.rm = TRUE),
              step = if (all(unique_vals == floor(unique_vals))) 1 else 0.1
            ),
            helpText("Values LESS THAN this threshold → 1, others → 0")
          ),
          
          conditionalPanel(
            condition = "input.mapping_method == 'threshold_gte'",
            numericInput(
              "threshold_value",
              "Threshold Value:",
              value = median(unique_vals, na.rm = TRUE),
              step = if (all(unique_vals == floor(unique_vals))) 1 else 0.1
            ),
            helpText("Values GREATER THAN OR EQUAL to this threshold → 1, others → 0")
          ),
          
          conditionalPanel(
            condition = "input.mapping_method == 'threshold_lte'",
            numericInput(
              "threshold_value",
              "Threshold Value:",
              value = median(unique_vals, na.rm = TRUE),
              step = if (all(unique_vals == floor(unique_vals))) 1 else 0.1
            ),
            helpText("Values LESS THAN OR EQUAL to this threshold → 1, others → 0")
          )
        )
      } else {
        NULL  # Don't show threshold options for non-numeric
      },
      
      actionButton(
        "apply_target_mapping",
        "Apply Target Mapping",
        class = "btn btn-info",
        style = "width: 100%; margin-top: 5px; margin-bottom: 15px;"
      )
    )
  })
  # --- Apply target mapping when user confirms ---
  observeEvent(input$apply_target_mapping, {
    req(input$target_col, input$mapping_method)
    
    # Validate inputs based on method
    if (input$mapping_method == "value") {
      req(input$positive_class_value)
    } else {
      req(input$threshold_value)
    }
    
    disable("apply_target_mapping")
    on.exit(enable("apply_target_mapping"), add = TRUE)
    
    # Binary mapping function - now handles both methods
    apply_binary_mapping <- function(df, target_col, method, value = NULL, threshold = NULL) {
      if (is.null(df) || is.null(target_col) || !(target_col %in% names(df))) return(df)
      
      col <- df[[target_col]]
      original_col <- col  # Keep original for mapping display
      
      if (method == "value") {
        # Method 1: Specific value selection
        col_char <- as.character(col)
        positive_char <- as.character(value)
        df[[target_col]] <- ifelse(col_char == positive_char, 1, 0)
        
        # Create mapping info
        unique_vals <- sort(unique(na.omit(as.character(original_col))))
        mapping <- setNames(
          ifelse(unique_vals == positive_char, 1, 0),
          unique_vals
        )
        mapping_desc <- paste0("Value '", value, "' = 1, all others = 0")
        
      } else if (method == "threshold_gt") {
        # Method 2: Greater than threshold
        df[[target_col]] <- ifelse(col > threshold, 1, 0)
        mapping_desc <- paste0("Values > ", threshold, " = 1, others = 0")
        mapping <- list(rule = mapping_desc, threshold = threshold, method = "greater_than")
        
      } else if (method == "threshold_lt") {
        # Method 3: Less than threshold
        df[[target_col]] <- ifelse(col < threshold, 1, 0)
        mapping_desc <- paste0("Values < ", threshold, " = 1, others = 0")
        mapping <- list(rule = mapping_desc, threshold = threshold, method = "less_than")
        
      } else if (method == "threshold_gte") {
        # Method 4: Greater than or equal
        df[[target_col]] <- ifelse(col >= threshold, 1, 0)
        mapping_desc <- paste0("Values >= ", threshold, " = 1, others = 0")
        mapping <- list(rule = mapping_desc, threshold = threshold, method = "greater_equal")
        
      } else if (method == "threshold_lte") {
        # Method 5: Less than or equal
        df[[target_col]] <- ifelse(col <= threshold, 1, 0)
        mapping_desc <- paste0("Values <= ", threshold, " = 1, others = 0")
        mapping <- list(rule = mapping_desc, threshold = threshold, method = "less_equal")
      }
      
      attr(df, "target_mapping") <- mapping
      attr(df, "mapping_description") <- mapping_desc
      return(df)
    }
    
    # Apply to dataset(s)
    if (upload_mode() == "dual" && !is.null(train_raw()) && !is.null(test_raw())) {
      train_df <- train_raw()
      test_df <- test_raw()
      
      if (input$mapping_method == "value") {
        mapped_train <- apply_binary_mapping(train_df, input$target_col, 
                                             method = "value", 
                                             value = input$positive_class_value)
        mapped_test <- apply_binary_mapping(test_df, input$target_col, 
                                            method = "value", 
                                            value = input$positive_class_value)
      } else {
        mapped_train <- apply_binary_mapping(train_df, input$target_col, 
                                             method = input$mapping_method, 
                                             threshold = input$threshold_value)
        mapped_test <- apply_binary_mapping(test_df, input$target_col, 
                                            method = input$mapping_method, 
                                            threshold = input$threshold_value)
      }
      
      mapping <- attr(mapped_train, "target_mapping")
      mapping_desc <- attr(mapped_train, "mapping_description")
      
      processed_train(mapped_train)
      processed_test(mapped_test)
      processed_data(mapped_train)
      target_mapping(mapping)
      
    } else if (!is.null(data_raw())) {
      df <- data_raw()
      
      if (input$mapping_method == "value") {
        mapped_df <- apply_binary_mapping(df, input$target_col, 
                                          method = "value", 
                                          value = input$positive_class_value)
      } else {
        mapped_df <- apply_binary_mapping(df, input$target_col, 
                                          method = input$mapping_method, 
                                          threshold = input$threshold_value)
      }
      
      mapping_desc <- attr(mapped_df, "mapping_description")
      
      processed_data(mapped_df)
      target_mapping(attr(mapped_df, "target_mapping"))
    }
    
    # Show distribution
    mapped_col <- processed_data()[[input$target_col]]
    n_positive <- sum(mapped_col == 1, na.rm = TRUE)
    n_negative <- sum(mapped_col == 0, na.rm = TRUE)
    
    shinyalert(
      "Target Mapping Applied",
      paste0(
        mapping_desc, "\n\n",
        "Positive class (1): n=", n_positive, "\n",
        "Negative class (0): n=", n_negative
      ),
      type = "success"
    )
  })
  
  # Track upload mode changes
  observeEvent(input$upload_mode, {
    upload_mode(input$upload_mode)
    data_raw(NULL)
    train_raw(NULL)
    test_raw(NULL)
    processed_data(NULL)
    processed_train(NULL)
    processed_test(NULL)
    target_mapping(NULL)
    applied_predictors(character(0))
  })
  
  # Train file upload handler
  observeEvent(input$file_train, {
    req(input$file_train, input$upload_mode == "dual")
    
    ext <- tools::file_ext(input$file_train$name)
    
    if (ext == "csv") {
      df <- read.csv(input$file_train$datapath, header = TRUE, sep = ",", stringsAsFactors = FALSE)
    } else if (ext == "tsv") {
      df <- read.delim(input$file_train$datapath, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
    } else {
      shinyalert("Invalid file", "Please upload a .csv or .tsv file.", type = "error")
      return()
    }
    
    logi_cols <- sapply(df, is.logical)
    if (any(logi_cols)) df[, logi_cols] <- lapply(df[, logi_cols, drop = FALSE], as.integer)
    
    train_raw(df)
    processed_train(df)
    data_reactive(df)
    
    if (!is.null(test_raw())) {
      validate_dual_upload()
    } else {
      shinyalert("Train Loaded", paste0(nrow(df), " rows loaded. Upload test data next."), 
                 type = "info", timer = 2000)
    }
  })
  
  # Test file upload handler
  observeEvent(input$file_test, {
    req(input$file_test, input$upload_mode == "dual")
    
    ext <- tools::file_ext(input$file_test$name)
    
    if (ext == "csv") {
      df <- read.csv(input$file_test$datapath, header = TRUE, sep = ",", stringsAsFactors = FALSE)
    } else if (ext == "tsv") {
      df <- read.delim(input$file_test$datapath, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
    } else {
      shinyalert("Invalid file", "Please upload a .csv or .tsv file.", type = "error")
      return()
    }
    
    logi_cols <- sapply(df, is.logical)
    if (any(logi_cols)) df[, logi_cols] <- lapply(df[, logi_cols, drop = FALSE], as.integer)
    
    test_raw(df)
    processed_test(df)
    
    if (!is.null(train_raw())) {
      validate_dual_upload()
    } else {
      shinyalert("Test Loaded", paste0(nrow(df), " rows loaded. Upload train data next."), 
                 type = "info", timer = 2000)
    }
  })
  
  # Validation function for dual upload
  validate_dual_upload <- function() {
    train_df <- train_raw()
    test_df <- test_raw()
    
    # Check column match
    if (!setequal(colnames(train_df), colnames(test_df))) {
      shinyalert("Column Mismatch", 
                 "Train and test files must have identical columns.", 
                 type = "error")
      return()
    }
    
    # Reorder test to match train
    test_df <- test_df[, colnames(train_df)]
    test_raw(test_df)
    
    processed_train(train_df)
    processed_test(test_df)
    
    # Check ratio
    actual_ratio <- nrow(train_df) / (nrow(train_df) + nrow(test_df))
    
    # Set raw data references WITHOUT mapping yet
    data_raw(train_df)
    
    # Update UI - NOW ACCEPT ALL COLUMNS
    updateSelectizeInput(session, "target_col", choices = names(train_df), server = TRUE)
    
    updatePickerInput(session, "predictors", choices = colnames(train_df), selected = NULL)
    
    # No warning - just clear it
    output$ratio_warning <- renderUI(NULL)
    
    shinyalert("Success", 
               sprintf("Train: %d rows | Test: %d rows | Actual Ratio: %.2f", 
                       nrow(train_df), nrow(test_df), actual_ratio),
               type = "success", timer = 3000)
  }
  
  # --- Show target mapping ---
  output$target_mapping_ui <- renderUI({
    mapping <- target_mapping()
    if (is.null(mapping)) return(NULL)
    tagList(
      h5("Target Value Mapping:"),
      verbatimTextOutput("mapping_text")
    )
  })
  
  output$mapping_text <- renderText({
    mapping <- target_mapping()
    if (is.list(mapping) && !is.null(mapping$rule)) {
      # Threshold-based mapping
      return(mapping$rule)
    } else {
      # Value-based mapping
      paste(names(mapping), "→", mapping, collapse = "\n")
    }
  })
  
  process_dataset <- function(df, predictors, target_col = NULL, na_threshold = 0.7) {
    keep_cols <- c()
    dropped_cols <- c()
    
    # --- Clean predictors
    for (col_name in predictors) {
      if (!(col_name %in% names(df))) next
      col <- df[[col_name]]
      
      # Logical → 0/1
      if (is.logical(col)) {
        df[[col_name]] <- as.integer(col)
        keep_cols <- c(keep_cols, col_name)
        next
      }
      
      # Categorical → keep NA as level
      if (is.character(col) || is.factor(col)) {
        df[[col_name]] <- factor(col, exclude = NULL)
        keep_cols <- c(keep_cols, col_name)
        next
      }
      
      # Continuous
      if (is.numeric(col)) {
        na_frac <- sum(is.na(col)) / length(col)
        if (na_frac > na_threshold) {
          cat("Dropping continuous feature due to high NA:", col_name, "\n")
          dropped_cols <- c(dropped_cols, col_name)
          next
        }
        keep_cols <- c(keep_cols, col_name)
      }
    }
    
    # --- Run mice once on all selected predictors (+ target if provided)
    cols_for_impute <- unique(c(keep_cols, target_col))
    cols_for_impute <- cols_for_impute[cols_for_impute %in% names(df)]
    
    df_imputed <- tryCatch({
      complete(mice(df[, cols_for_impute, drop = FALSE], method = "cart", m = 1, printFlag = FALSE))
    }, error = function(e) {
      cat("MICE failed:", conditionMessage(e), "\n")
      return(df)
    })
    
    # Replace imputed columns back into main df
    df[, cols_for_impute] <- df_imputed[, cols_for_impute]
    
    list(df = df, keep_cols = unique(keep_cols), dropped_cols = dropped_cols)
  }
  
  # Sync mapper data changes back to processed_data
  observe({
    req(mapper$data())
    
    df <- mapper$data()
    
    # DON'T convert all columns - only update the data reference
    processed_data(df)
    data_reactive(df)
    
    # Also update train/test if in dual mode
    if (upload_mode() == "dual") {
      if (!is.null(train_raw())) {
        processed_train(df)  # Assuming mapper works on train data
      }
    }
  })
  
  
  # --- Apply Predictors ---
  observeEvent(input$apply_predictors, {
    req(input$target_col)
    
    if (is.null(input$predictors) || length(input$predictors) == 0) {
      shinyalert("No predictors selected", "Select at least one predictor before applying.", type = "warning")
      return()
    }
    
    disable("apply_predictors")
    on.exit(enable("apply_predictors"), add = TRUE)
    
    pred_cols <- input$predictors
    target_col <- input$target_col
    na_threshold <- 0.6
    
    # --- Process each uploaded file independently ---
    if (upload_mode() == "dual" && !is.null(processed_train()) && !is.null(processed_test())) {
      # Get the MAPPED data (not raw)
      train_df <- processed_train()
      test_df  <- processed_test()
      
      result_train <- process_dataset(train_df, predictors = pred_cols, target_col = target_col, na_threshold = na_threshold)
      result_test  <- process_dataset(test_df,  predictors = pred_cols, target_col = target_col, na_threshold = na_threshold)
      
      processed_train(result_train$df)
      processed_test(result_test$df)
      processed_data(result_train$df)
      applied_predictors(result_train$keep_cols)
      
    } else if (!is.null(processed_data())) {
      df <- processed_data()
      
      keep_cols <- c()
      for (col_name in pred_cols) {
        if (col_name %in% names(df)) {
          na_frac <- sum(is.na(df[[col_name]])) / nrow(df)
          if (na_frac < 0.5) {
            keep_cols <- c(keep_cols, col_name)
          }
        }
      }
      
      cols_to_keep <- c(target_col, keep_cols)
      df_subset <- df[, cols_to_keep, drop = FALSE]
      
      na_per_row <- rowSums(is.na(df_subset))
      df_clean <- df_subset[na_per_row < 4, ]
      
      for (col in keep_cols) {
        if (sum(is.na(df_clean[[col]])) > 0) {
          mean_val <- mean(df_clean[[col]], na.rm = TRUE)
          df_clean[[col]][is.na(df_clean[[col]])] <- mean_val
        }
      }
      
      #cols_to_keep <- c(target_col, keep_cols)
      #df_subset <- df[, cols_to_keep, drop = FALSE]
      
      # Drop rows with >= 7 NA
      #na_per_row <- rowSums(is.na(df_subset))
      #df_clean <- df_subset[na_per_row < 3, ]
      
      #write.table(
        #df_clean, 
        #"drop_50_and_4row_impute.tsv",
        #sep = "\t", 
       # row.names = FALSE, 
       # quote = FALSE
      #)
      
      
      
      result <- process_dataset(df, predictors = pred_cols, target_col = target_col, na_threshold = na_threshold)
      processed_data(result$df)
      applied_predictors(result$keep_cols)
    } else {
      shinyalert("Error", "No data available. Please upload data and select target first.", type = "error")
      return()
    }
    
    updatePickerInput(
      session,
      "predictors",
      choices = setdiff(names(data_raw()), input$target_col),
      selected = intersect(input$predictors, applied_predictors())
    )
    
    kept <- length(applied_predictors())
    dropped <- setdiff(input$predictors, applied_predictors())
    msg <- paste0("Applied ", kept, " predictor(s). Table refreshed.")
    if (length(dropped) > 0) msg <- paste0(msg, "\nDropped: ", paste(dropped, collapse = ", "))
    shinyalert("Applied", msg, type = "success")
    
  })
  
  
  # Train preview table
  output$train_preview <- renderDT({
    req(processed_train())
    datatable(
      processed_train(),
      options = list(scrollX = TRUE, pageLength = 10,
                     lengthMenu = c(10,25,50,100,nrow(processed_train())))
    )
  })
  
  # Test preview table
  output$test_preview <- renderDT({
    req(processed_test())
    datatable(
      processed_test(),
      options = list(scrollX = TRUE, pageLength = 10,
                     lengthMenu = c(10,25,50,100,nrow(processed_test())))
    )
  })
  
  
  output$data_preview <- renderDT({
    req(processed_data())
    datatable(
      processed_data(),
      options = list(
        scrollX = TRUE, 
        pageLength = 10,
        lengthMenu = c(10,25,50,100,nrow(processed_data()))
      )
    )
  })
  # Open column mapper with column selection
  observeEvent(input$open_column_mapper, {
    req(processed_data())
    
    df <- processed_data()
    all_cols <- names(df)
    
    # Show modal to select which column to map
    showModal(modalDialog(
      title = "Select Column to Map",
      size = "m",
      
      selectInput(
        "column_to_map",
        "Choose a column:",
        choices = all_cols,
        selected = NULL
      ),
      
      footer = tagList(
        actionButton("confirm_column_selection", "Next", class = "btn btn-primary"),
        modalButton("Cancel")
      )
    ))
  })
  
  # When user confirms column selection, open the mapper
  observeEvent(input$confirm_column_selection, {
    req(input$column_to_map)
    
    removeModal()  # Close the column selection modal
    
    # Small delay to ensure first modal is closed
    Sys.sleep(0.1)
    
    # Open the mapping modal for the selected column
    mapper$showModal(input$column_to_map)
  })
  
  
  # --- Results containers ---
  cor_results <- reactiveVal(NULL)
  rf_results <- reactiveVal(NULL)
  mb_results <- reactiveVal(NULL)
  rf_model_obj <- reactiveVal(NULL)
  lasso_results <- reactiveVal(NULL)
  
  lasso_cv_model <- reactiveVal(NULL)     # ADD THIS LINE
  lasso_result_df <- reactiveVal(NULL)
  
  mb_completed <- reactiveVal(FALSE)
  

  
  observeEvent(input$run, {
    if (is.null(input$target_col) || length(applied_predictors()) == 0) {
      shinyalert("Missing inputs", "Please select target, apply mapping, and commit predictors before running.", type = "error")
      return()
    }
    
    
    
    disable("run")
    on.exit(enable("run"), add = TRUE)
    
    df <- processed_data()
    target <- df[[input$target_col]]
    predictors <- df[, applied_predictors(), drop = FALSE]
    
    cor_vals <- sapply(names(predictors), function(col_name) {
      x <- predictors[[col_name]]
      
      tryCatch({
        if (!is.numeric(x)) x <- as.numeric(as.character(x))
        cor(x, target, use = "complete.obs")
      }, error = function(e) {
        cat("ERROR at column:", col_name, "-", e$message, "\n")
        return(NA)
      })
    }, USE.NAMES = TRUE)
    
    cor_vals <- cor_vals[!is.na(cor_vals)]
    cor_results(cor_vals[order(abs(cor_vals), decreasing = TRUE)])
    
    # Markov Blanket
    safe_discretize <- function(vec, breaks) {
      vec <- as.numeric(vec)
      unique_vals <- unique(na.omit(vec))
      if (length(unique_vals) < 2) {
        return(factor(vec))
      }
      adjusted_breaks <- min(length(unique_vals), breaks)
      out <- tryCatch(
        discretize(as.data.frame(vec), method = "interval", breaks = adjusted_breaks)[, 1],
        error = function(e) factor(vec)
      )
      return(out)
    }
    
    target_disc <- if (is.numeric(target)) {
      if (all(target %in% c(0, 1), na.rm = TRUE)) {
        safe_discretize(target, 2)
      } else {
        safe_discretize(target, 3)
      }
    } else {
      factor(target)
    }
    
    predictors_disc <- as.data.frame(lapply(df[applied_predictors()], function(col) {
      if (is.numeric(col)) {
        if (all(col %in% c(0, 1), na.rm = TRUE)) {
          safe_discretize(col, 2)
        } else {
          safe_discretize(col, 3)
        }
      } else {
        factor(col)
      }
    }))
    
    mb_df <- data.frame(target_disc, predictors_disc)
    colnames(mb_df)[1] <- input$target_col
    mb_df <- mb_df[sapply(mb_df, function(col) length(unique(col)) > 1)]
    
    mb_direct <- tryCatch(learn.mb(mb_df, node = input$target_col, method = 'inter.iamb', alpha = 0.15),
                          error = function(e) character(0))
    mb_indirect <- character(0)
    for (member in mb_direct) {
      mb_member <- tryCatch(learn.mb(mb_df, node = member, method = 'inter.iamb', alpha = 0.15),
                            error = function(e) character(0))
      mb_indirect <- unique(c(mb_indirect, setdiff(mb_member, c(input$target_col, mb_direct))))
    }
    all_mb_features <- c(mb_direct, mb_indirect)
    mb_results(all_mb_features)
    mb_completed(TRUE)  # Mark MB as completed
    
    # Update MB table output
    if (length(all_mb_features) > 0) {
      mb_result_df <- data.frame(
        Category = c(rep("Direct", length(mb_direct)), rep("Indirect", length(mb_indirect))),
        Feature = c(mb_direct, mb_indirect)
      )
      output$mb_table <- renderDT(datatable(mb_result_df))
    } else {
      # Show message when no features found
      mb_result_df <- data.frame(
        Category = "No Features",
        Feature = "Markov Blanket found no relevant features"
      )
      output$mb_table <- renderDT(datatable(mb_result_df, options = list(dom = 't')))
    }
    
    shinyalert("Done", "Feature selection finished successfully.", type = "success")
  })
  
  # --- Run Random Forest ---
  observeEvent(input$run_rf, {
    req(input$target_col, length(applied_predictors()) > 0)
    
    shinyalert(
      title = "Running Random Forest...",
      text = "Please wait while the model is being trained.",
      type = "info",
      showConfirmButton = FALSE,
      timer = 0,
      closeOnClickOutside = FALSE
    )
    
    df <- processed_data()
    target <- df[[input$target_col]]
    predictors <- df[, applied_predictors(), drop = FALSE]
    
    predictors[] <- lapply(predictors, function(x) {
      if (!is.numeric(x)) factor(x) else x
    })
    
    subdata <- data.frame(target = target, predictors)
    rf_model <- rfsrc(target ~ ., data = subdata, na.action = "na.impute",
                      importance = TRUE, ntree = input$rf_ntree)
    
    
    rf_results(sort(rf_model$importance, decreasing = TRUE))
    rf_model_obj(rf_model)
  })
  
  observeEvent(input$run_lasso, {
    message("=== RUN_LASSO: Button clicked ===")
    req(input$target_col, length(applied_predictors()) > 0, mb_completed())
    message("=== RUN_LASSO: req passed ===")
    
    shinyalert(
      title = "Running LASSO...",
      text = "Please wait while feature selection is being performed.",
      type = "info",
      showConfirmButton = FALSE,
      timer = 0,
      closeOnClickOutside = FALSE
    )
    
    df <- processed_data()
    message("=== RUN_LASSO: Data loaded: ", nrow(df), " rows ===")
    
    X <- df[, applied_predictors(), drop = FALSE]
    y <- df[[input$target_col]]
    
    
    # Check if we have enough data
    if (nrow(X) < 10) {
      message("=== RUN_LASSO: Not enough data, returning ===")
      return()
    }
    
    # Convert to matrix
    X <- as.matrix(X)
    
    message("=== RUN_LASSO: Running cv.glmnet ===")
    cv.lasso <- cv.glmnet(X, y, alpha = 1)
    message("=== RUN_LASSO: cv.glmnet complete ===")
    
    coef_lasso <- coef(cv.lasso, s = input$lasso_lambda_choice)
    nonzero_features <- rownames(coef_lasso)[which(coef_lasso != 0)]
    nonzero_coef <- as.numeric(coef_lasso[which(coef_lasso != 0)])
    
    df_lasso <- data.frame(
      Feature = nonzero_features[nonzero_features != "(Intercept)"],
      Coefficient = nonzero_coef[nonzero_features != "(Intercept)"]
    )
    
    df_lasso <- df_lasso[order(-abs(df_lasso$Coefficient)), ]
    
    message("=== RUN_LASSO: df_lasso created with ", nrow(df_lasso), " rows ===")
    message("=== RUN_LASSO: First few features: ===")
    print(head(df_lasso, 3))
    
    message("=== RUN_LASSO: BEFORE storing ===")
    lasso_cv_model(cv.lasso)
    lasso_result_df(df_lasso)
    lasso_results(df_lasso$Feature)
    message("=== RUN_LASSO: AFTER storing ===")
    
    lasso_trigger(lasso_trigger() + 1)
    message("=== RUN_LASSO: Trigger incremented to ", lasso_trigger(), " ===")
    
    message("=== RUN_LASSO: Storage complete! ===")
  })
  
  output$lasso_table <- renderDT({
    message("=== lasso_table called ===")
    
    # This will cause re-execution whenever lasso_trigger changes
    trigger <- lasso_trigger()
    message("=== lasso_table: trigger value is ", trigger, " ===")
    
    req(trigger > 0)  # Only render if button has been clicked
    message("=== lasso_table: trigger req passed ===")
    
    req(lasso_result_df())
    message("=== lasso_table: data req passed, rendering ===")
    
    datatable(
      lasso_result_df(),
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    ) %>% formatRound('Coefficient', 4)
  })
  
  output$lasso_plot <- renderPlot({
    message("=== lasso_plot called ===")
    
    trigger <- lasso_trigger()
    message("=== lasso_plot: trigger value is ", trigger, " ===")
    
    req(trigger > 0)
    message("=== lasso_plot: trigger req passed ===")
    
    req(lasso_cv_model())
    message("=== lasso_plot: model req passed, plotting ===")
    
    closeAlert()
    
    # ONLY the default cv.glmnet plot
    plot(lasso_cv_model())
  })
  
   
  # --- Pearson outputs ---
  output$cor_table <- renderDT({
    req(cor_results())
    cor_data <- cor_results()
    datatable(data.frame(
      Feature = names(cor_data),
      Correlation = as.numeric(cor_data),
      row.names = NULL
    ))
  })
  
  output$cor_plot <- renderPlot({
    req(cor_results())
    cor_vals <- cor_results()
    par(mar = c(12, 4, 4, 2))
    barplot(cor_vals, las = 2, cex.names = 0.8,
            main = "Pearson Correlation", col = "grey")
  })
  
  # --- Random Forest outputs ---
  output$rf_table <- renderDT({
    req(rf_results())
    closeAlert()
    rf_data <- rf_results()
    datatable(data.frame(
      Feature = names(rf_data),
      Importance = as.numeric(rf_data),
      row.names = NULL
    ))
  })
  
  output$rf_plot <- renderPlot({
    req(rf_model_obj())
    n_features <- length(rf_model_obj()$importance)
    plot_height <- max(600, n_features * 15)  # Dynamic height
    
    par(mar = c(5, 10, 4, 2))
    plot.rfsrc(rf_model_obj(), cex.axis = 0.7, cex.lab = 0.8)
  }, height = function() {
    req(rf_model_obj())
    n_features <- length(rf_model_obj()$importance)
    max(600, n_features * 15)
  }, width = 800)

  
  
  output$shap_plot <- renderPlot({
    req(rv$shap_values, rv$current_model_info, rv$feature_values)
    
    shap_vals <- rv$shap_values
    model_info <- rv$current_model_info
    feature_vals <- rv$feature_values
    
    # Ensure both have same dimensions
    if (nrow(shap_vals) != nrow(feature_vals) || ncol(shap_vals) != ncol(feature_vals)) {
      plot(1, 1, type = "n", main = "Dimension mismatch between SHAP values and features")
      return()
    }
    
    # Remove any rows with NA or infinite values
    valid_rows <- complete.cases(shap_vals) & complete.cases(feature_vals)
    valid_rows <- valid_rows & apply(shap_vals, 1, function(x) all(is.finite(x)))
    valid_rows <- valid_rows & apply(feature_vals, 1, function(x) all(is.finite(x)))
    
    if (sum(valid_rows) < 10) {
      plot(1, 1, type = "n", main = "Not enough valid data points for SHAP plot")
      return()
    }
    
    shap_vals <- shap_vals[valid_rows, , drop = FALSE]
    feature_vals <- feature_vals[valid_rows, , drop = FALSE]
    
    # Calculate mean absolute SHAP values to order features
    mean_shap <- colMeans(abs(shap_vals), na.rm = TRUE)
    
    # Remove any features with NA mean
    valid_features <- !is.na(mean_shap) & is.finite(mean_shap)
    if (sum(valid_features) == 0) {
      plot(1, 1, type = "n", main = "No valid features for SHAP plot")
      return()
    }
    
    mean_shap <- mean_shap[valid_features]
    shap_vals <- shap_vals[, valid_features, drop = FALSE]
    feature_vals <- feature_vals[, valid_features, drop = FALSE]
    
    feature_order <- names(sort(mean_shap, decreasing = TRUE))
    
    # Prepare plotting area
    n_features <- length(feature_order)
    n_samples <- nrow(shap_vals)
    
    # Get safe x-axis limits
    x_range <- range(shap_vals, na.rm = TRUE, finite = TRUE)
    if (!is.finite(x_range[1]) || !is.finite(x_range[2])) {
      plot(1, 1, type = "n", main = "No valid SHAP values to display")
      return()
    }
    
    # Add small buffer to range
    x_buffer <- diff(x_range) * 0.05
    x_range <- x_range + c(-x_buffer, x_buffer)
    
    par(mar = c(5, 12, 4, 6))
    plot(NULL, xlim = x_range, ylim = c(0.5, n_features + 0.5),
         xlab = "SHAP value (impact on model output)",
         ylab = "",
         yaxt = "n",
         main = paste("SHAP Values\n", model_info$subset_name, "-", toupper(model_info$model_name)))
    
    # Add vertical line at x=0
    abline(v = 0, col = "gray70", lwd = 1)
    
    # Define color palette
    colors <- colorRampPalette(c("#3B4CC0", "#7F3BB0", "#B40426"))(100)
    
    # Plot each feature
    for (i in 1:n_features) {
      feat_name <- feature_order[i]
      shap_col <- shap_vals[, feat_name]
      feat_col <- feature_vals[, feat_name]
      
      # Remove any remaining NA/Inf
      valid_idx <- is.finite(shap_col) & is.finite(feat_col)
      shap_col <- shap_col[valid_idx]
      feat_col <- feat_col[valid_idx]
      
      if (length(shap_col) == 0) next
      
      # Normalize feature values to [0, 1] for coloring
      feat_range <- range(feat_col, na.rm = TRUE, finite = TRUE)
      
      # Handle constant features
      if (!is.finite(feat_range[1]) || !is.finite(feat_range[2]) || feat_range[1] == feat_range[2]) {
        feat_normalized <- rep(0.5, length(feat_col))
      } else {
        feat_normalized <- (feat_col - feat_range[1]) / (feat_range[2] - feat_range[1])
        feat_normalized <- pmax(0, pmin(1, feat_normalized))  # Clamp to [0,1]
      }
      
      # Assign colors
      color_idx <- pmax(1, pmin(100, ceiling(feat_normalized * 100)))
      color_idx[!is.finite(color_idx)] <- 50  # Middle color for any issues
      point_colors <- colors[color_idx]
      
      # Add jitter to avoid overplotting
      y_jitter <- i + runif(length(shap_col), -0.2, 0.2)
      
      # Plot points
      points(shap_col, y_jitter, pch = 16, col = point_colors, cex = 0.5)
    }
    
    # Add y-axis labels
    axis(2, at = 1:n_features, labels = feature_order, las = 1, cex.axis = 0.8)
    
    # Add color legend
    tryCatch({
      legend_colors <- colorRampPalette(c("#3B4CC0", "#7F3BB0", "#B40426"))(100)
      legend_x <- par("usr")[2] + (par("usr")[2] - par("usr")[1]) * 0.05
      legend_y <- seq(1, n_features, length.out = 100)
      
      for (j in 1:99) {
        rect(legend_x, legend_y[j], 
             legend_x + (par("usr")[2] - par("usr")[1]) * 0.02, 
             legend_y[j+1], 
             col = legend_colors[j], border = NA, xpd = TRUE)
      }
      
      text(legend_x + (par("usr")[2] - par("usr")[1]) * 0.03, max(legend_y), 
           "High", pos = 4, xpd = TRUE, cex = 0.8)
      text(legend_x + (par("usr")[2] - par("usr")[1]) * 0.03, min(legend_y), 
           "Low", pos = 4, xpd = TRUE, cex = 0.8)
      mtext("Feature value", side = 4, line = 4, cex = 0.8)
    }, error = function(e) {
      # Legend rendering failed, but main plot is still shown
      cat("Legend rendering failed:", e$message, "\n")
    })
  })
  
  # --- Final selection reactive ---
  final_selection <- reactive({
    req(cor_results(), rf_results(), mb_completed(), lasso_results(), input$topx)
    
    topx_cor <- names(cor_results())[1:min(input$topx, length(cor_results()))]
    topx_rf <- names(rf_results())[1:min(input$topx, length(rf_results()))]
    mb_features <- mb_results()
    if (is.null(mb_features)) mb_features <- character(0)
    lasso_features <- lasso_results()
    
    both_cor_rf <- intersect(topx_cor, topx_rf)
    
    categories <- c()
    features <- c()
    
    add_cat <- function(cat, feats) {
      if (length(feats) > 0) {
        categories <<- c(categories, rep(cat, length(feats)))
        features <<- c(features, feats)
      }
    }
    
    # Combine multiple sets
    add_cat("Pearson + RF + Markov + LASSO", Reduce(intersect, list(topx_cor, topx_rf, mb_features, lasso_features)))
    add_cat("Pearson + RF + Markov", setdiff(Reduce(intersect, list(topx_cor, topx_rf, mb_features)), lasso_features))
    add_cat("Pearson + RF + LASSO", setdiff(Reduce(intersect, list(topx_cor, topx_rf, lasso_features)), mb_features))
    add_cat("Pearson + Markov + LASSO", setdiff(Reduce(intersect, list(topx_cor, mb_features, lasso_features)), topx_rf))
    add_cat("RF + Markov + LASSO", setdiff(Reduce(intersect, list(topx_rf, mb_features, lasso_features)), topx_cor))
    add_cat("Pearson + RF", setdiff(both_cor_rf, Reduce(union, list(mb_features, lasso_features))))
    add_cat("Pearson + Markov", setdiff(intersect(topx_cor, mb_features), Reduce(union, list(both_cor_rf, lasso_features))))
    add_cat("Pearson + LASSO", setdiff(intersect(topx_cor, lasso_features), Reduce(union, list(both_cor_rf, mb_features))))
    add_cat("RF + Markov", setdiff(intersect(topx_rf, mb_features), Reduce(union, list(topx_cor, lasso_features))))
    add_cat("RF + LASSO", setdiff(intersect(topx_rf, lasso_features), Reduce(union, list(topx_cor, mb_features))))
    add_cat("Markov + LASSO", setdiff(intersect(mb_features, lasso_features), Reduce(union, list(topx_cor, topx_rf))))
    add_cat("Pearson Only", setdiff(topx_cor, Reduce(union, list(topx_rf, mb_features, lasso_features))))
    add_cat("RF Only", setdiff(topx_rf, Reduce(union, list(topx_cor, mb_features, lasso_features))))
    add_cat("Markov Only", setdiff(mb_features, Reduce(union, list(topx_cor, topx_rf, lasso_features))))
    add_cat("LASSO Only", setdiff(lasso_features, Reduce(union, list(topx_cor, topx_rf, mb_features))))
    
    result <- data.frame(Category = categories, Feature = features)
    result <- result[result$Feature != "" & !is.na(result$Feature), ]
    
    return(result)
  })
  
  # --- Render final table only after run_lasso clicked ---
  output$final_table <- renderDT({
    req(final_selection())
    datatable(final_selection())
  })
  
  # Download Final Selection table
  output$download_final_table <- downloadHandler(
    filename = function() {
      paste0("final_feature_selection_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(final_selection(), file, row.names = FALSE)
    }
  )
  
  # --- Navigation buttons ---
  tabs <- c("Upload & Select Target", "Pearson Correlation", "Random Forest", "Markov Blanket", "LASSO", "Final Selection")
  
  
  observeEvent(input$prevTab, {
    current <- match(input$main_tabs, tabs)
    if(current > 1) updateTabsetPanel(session, "main_tabs", selected = tabs[current - 1])
  })
  
  observeEvent(input$nextTab, {
    current <- match(input$main_tabs, tabs)
    if(current < length(tabs)) updateTabsetPanel(session, "main_tabs", selected = tabs[current + 1])
  })
  
  # Update custom subset feature picker when predictors are applied
  observeEvent(applied_predictors(), {
    req(length(applied_predictors()) > 0)
    updatePickerInput(session, "custom_subset_features", 
                      choices = applied_predictors())
  })
  # Handle adding custom subset
  observeEvent(input$add_custom_subset, {
    req(input$custom_subset_name, input$custom_subset_features)
    
    if (input$custom_subset_name == "" || length(input$custom_subset_features) == 0) {
      shinyalert("Invalid Input", "Please provide a subset name and select at least one feature.", type = "error")
      return()
    }
    
    # Store custom subset
    rv$custom_subsets[[input$custom_subset_name]] <- input$custom_subset_features
    
    # Re-run models with the new custom subset included
    run_models_with_params()
    
    shinyalert("Custom Subset Added", 
               paste0("Subset '", input$custom_subset_name, "' with ", 
                      length(input$custom_subset_features), " features has been added and models are running."), 
               type = "success")
    
    # Clear inputs
    updateTextInput(session, "custom_subset_name", value = "")
    updatePickerInput(session, "custom_subset_features", selected = character(0))
  })
  
  # Helper function to run models with parameters
  run_models_with_params <- function() {
    # Show loading alert
    shinyalert(
      title = "Running Models...",
      text = "Please wait while models are being trained and evaluated.",
      type = "info",
      showConfirmButton = FALSE,
      timer = 0,
      closeOnClickOutside = FALSE
    )
    req(cor_results(), rf_results(), mb_completed(), lasso_results(), input$topx)
    
    topx_cor <- names(cor_results())[1:min(input$topx, length(cor_results()))]
    topx_rf <- names(rf_results())[1:min(input$topx, length(rf_results()))]
    mb_features <- mb_results()
    if (is.null(mb_features)) mb_features <- character(0)
    lasso_features <- lasso_results()
    df <- processed_data()
    target_col <- input$target_col
    
    custom_subsets_list <- rv$custom_subsets
    
    # run function with parameters (modify to pass custom subsets)
    res <- train_auc_matrix_multi(
      topx_cor,
      topx_rf,
      mb_features,
      lasso_features,
      df_clean = df,
      target_col = target_col,
      split_ratio = input$split_ratio,
      xgb_eta = input$xgb_eta,
      xgb_max_depth = input$xgb_max_depth,
      rf_ntree = input$rf_ntree_model,
      cv_folds = input$cv_folds,
      custom_subsets = custom_subsets_list,
      train_data = if(upload_mode() == "dual") processed_train() else NULL,  
      test_data = if(upload_mode() == "dual") processed_test() else NULL    
    )
    
    auc_results <- res$results
    roc_data <- res$rocs
    
    # Update history for each model and subset (with debugging)
    if (!is.null(auc_results) && nrow(auc_results) > 0) {
      
      for (i in 1:nrow(auc_results)) {
        tryCatch({
          new_log <- data.frame(
            Subset = as.character(auc_results$Subset[i]),
            AUC = as.numeric(auc_results$Logistic_AUC[i]),
            split_ratio = input$split_ratio,
            stringsAsFactors = FALSE
          )
          cat("Logistic row created successfully\n")
          rv$logistic_history <- rbind(rv$logistic_history, new_log)
        }, error = function(e) {
          cat("ERROR creating logistic row:", e$message, "\n")
        })
        
        tryCatch({
          new_rf <- data.frame(
            Subset = as.character(auc_results$Subset[i]),
            AUC = as.numeric(auc_results$RF_AUC[i]),
            ntree = input$rf_ntree_model,
            split_ratio = input$split_ratio,
            stringsAsFactors = FALSE
          )
          cat("RF row created successfully\n")
          rv$rf_history <- rbind(rv$rf_history, new_rf)
        }, error = function(e) {
          cat("ERROR creating RF row:", e$message, "\n")
        })
        
        tryCatch({
          new_xgb <- data.frame(
            Subset = as.character(auc_results$Subset[i]),
            AUC = as.numeric(auc_results$XGB_AUC[i]),
            eta = input$xgb_eta,
            max_depth = input$xgb_max_depth,
            cv_folds = input$cv_folds,
            split_ratio = input$split_ratio,
            stringsAsFactors = FALSE
          )
          cat("XGBoost row created successfully\n")
          rv$xgb_history <- rbind(rv$xgb_history, new_xgb)
          
          # ADD THIS DEBUG CODE:
          cat("\n=== XGBoost History After Update ===\n")
          print(rv$xgb_history)
          cat("Number of rows:", nrow(rv$xgb_history), "\n\n")
          
        }, error = function(e) {
          cat("ERROR creating XGBoost row:", e$message, "\n")
        })
      }
    }
    # AUC Table
    output$auc_table <- renderDT({
      datatable(
        auc_results[, c("Subset", "NumFeatures", "Logistic_AUC", "RF_AUC", "XGB_AUC")],
        selection = "single",
        options = list(pageLength = 5, scrollX = TRUE),
        rownames = FALSE,
        colnames = c("Subset", "Number of Features", "Logistic", "RF", "XGBoost")
      ) %>% formatRound(c('Logistic_AUC', 'RF_AUC', 'XGB_AUC'), 4)
    })
    
    # MCC Table
    output$mcc_table <- renderDT({
      datatable(
        auc_results[, c("Subset", "NumFeatures", "Logistic_MCC", "RF_MCC", "XGB_MCC")],
        selection = "single",
        options = list(pageLength = 5, scrollX = TRUE),
        rownames = FALSE,
        colnames = c("Subset", "Number of Features", "Logistic", "RF", "XGBoost")
      ) %>% formatRound(c('Logistic_MCC', 'RF_MCC', 'XGB_MCC'), 4)
    })
    
    # Recall Table
    output$recall_table <- renderDT({
      datatable(
        auc_results[, c("Subset", "NumFeatures", "Logistic_Recall", "RF_Recall", "XGB_Recall")],
        selection = "single",
        options = list(pageLength = 5, scrollX = TRUE),
        rownames = FALSE,
        colnames = c("Subset", "Number of Features", "Logistic", "RF", "XGBoost")
      ) %>% formatRound(c('Logistic_Recall', 'RF_Recall', 'XGB_Recall'), 4)
    })
    
    # Precision Table
    output$precision_table <- renderDT({
      datatable(
        auc_results[, c("Subset", "NumFeatures", "Logistic_Precision", "RF_Precision", "XGB_Precision")],
        selection = "single",
        options = list(pageLength = 5, scrollX = TRUE),
        rownames = FALSE,
        colnames = c("Subset", "Number of Features", "Logistic", "RF", "XGBoost")
      ) %>% formatRound(c('Logistic_Precision', 'RF_Precision', 'XGB_Precision'), 4)
    })
    
    # F1 Score Table
    output$f1_table <- renderDT({
      datatable(
        auc_results[, c("Subset", "NumFeatures", "Logistic_F1", "RF_F1", "XGB_F1")],
        selection = "single",
        options = list(pageLength = 5, scrollX = TRUE),
        rownames = FALSE,
        colnames = c("Subset", "Number of Features", "Logistic", "RF", "XGBoost")
      ) %>% formatRound(c('Logistic_F1', 'RF_F1', 'XGB_F1'), 4)
    })
    
    
    # Store ROC data in reactiveValues so we can use it later
    rv$roc_data <- roc_data
    
    closeAlert()
    shinyalert(
      title = "Complete!",
      text = "Models have finished running.",
      type = "success",
      timer = 2000,
      showConfirmButton = FALSE
    )
  }
  
  observeEvent(input$run_lasso, {
    req(cor_results(), rf_results(), mb_completed(), lasso_results(), input$topx)
    
    topx_cor <- names(cor_results())[1:min(input$topx, length(cor_results()))]
    topx_rf <- names(rf_results())[1:min(input$topx, length(rf_results()))]
    mb_features <- mb_results()
    if (is.null(mb_features)) mb_features <- character(0)
    lasso_features <- lasso_results()
    df <- processed_data()
    target_col <- input$target_col
    
    custom_subsets_list <- rv$custom_subsets
    
    # Run models WITHOUT showing loading alert
    res <- train_auc_matrix_multi(
      topx_cor,
      topx_rf,
      mb_features,
      lasso_features,
      df_clean = df,
      target_col = target_col,
      split_ratio = input$split_ratio,
      xgb_eta = input$xgb_eta,
      xgb_max_depth = input$xgb_max_depth,
      rf_ntree = input$rf_ntree_model,
      cv_folds = input$cv_folds,
      custom_subsets = custom_subsets_list,
      train_data = if(upload_mode() == "dual") processed_train() else NULL,  # ADD THIS
      test_data = if(upload_mode() == "dual") processed_test() else NULL     # ADD THIS
    )
    
    auc_results <- res$results
    roc_data <- res$rocs
    
    # Render all four tables (same as in run_models_with_params)
    output$auc_table <- renderDT({
      datatable(
        auc_results[, c("Subset", "NumFeatures", "Logistic_AUC", "RF_AUC", "XGB_AUC")],
        selection = "single",
        options = list(pageLength = 5, scrollX = TRUE),
        rownames = FALSE,
        colnames = c("Subset", "Number of Features", "Logistic", "RF", "XGBoost")
      ) %>% formatRound(c('Logistic_AUC', 'RF_AUC', 'XGB_AUC'), 4)
    })
    
    output$mcc_table <- renderDT({
      datatable(
        auc_results[, c("Subset", "NumFeatures", "Logistic_MCC", "RF_MCC", "XGB_MCC")],
        selection = "single",
        options = list(pageLength = 5, scrollX = TRUE),
        rownames = FALSE,
        colnames = c("Subset", "Number of Features", "Logistic", "RF", "XGBoost")
      ) %>% formatRound(c('Logistic_MCC', 'RF_MCC', 'XGB_MCC'), 4)
    })
    
    # Recall Table
    output$recall_table <- renderDT({
      datatable(
        auc_results[, c("Subset", "NumFeatures", "Logistic_Recall", "RF_Recall", "XGB_Recall")],
        selection = "single",
        options = list(pageLength = 5, scrollX = TRUE),
        rownames = FALSE,
        colnames = c("Subset", "Number of Features", "Logistic", "RF", "XGBoost")
      ) %>% formatRound(c('Logistic_Recall', 'RF_Recall', 'XGB_Recall'), 4)
    })
    
    # Precision Table
    output$precision_table <- renderDT({
      datatable(
        auc_results[, c("Subset", "NumFeatures", "Logistic_Precision", "RF_Precision", "XGB_Precision")],
        selection = "single",
        options = list(pageLength = 5, scrollX = TRUE),
        rownames = FALSE,
        colnames = c("Subset", "Number of Features", "Logistic", "RF", "XGBoost")
      ) %>% formatRound(c('Logistic_Precision', 'RF_Precision', 'XGB_Precision'), 4)
    })
    
    # F1 Score Table
    output$f1_table <- renderDT({
      datatable(
        auc_results[, c("Subset", "NumFeatures", "Logistic_F1", "RF_F1", "XGB_F1")],
        selection = "single",
        options = list(pageLength = 5, scrollX = TRUE),
        rownames = FALSE,
        colnames = c("Subset", "Number of Features", "Logistic", "RF", "XGBoost")
      ) %>% formatRound(c('Logistic_F1', 'RF_F1', 'XGB_F1'), 4)
    })
    
    rv$roc_data <- roc_data
  })
  
  
  observeEvent(input$rerun_models, {
    run_models_with_params()
  })
  
  
  
  # Handle clicks from any of the metric tables
  handle_table_click <- function(info) {
    req(info$row, info$col >= 2, rv$roc_data)  # col >= 2 means clicking on model columns
    
    # Get the AUC table data to find the actual subset name
    # This ensures we use the correct subset even if some rows are empty
    auc_data <- NULL
    
    # Try to get the current table data based on which tab is active
    tryCatch({
      # Get all metric results
      req(cor_results(), rf_results(), mb_completed(), lasso_results(), input$topx)
      
      topx_cor <- names(cor_results())[1:min(input$topx, length(cor_results()))]
      topx_rf <- names(rf_results())[1:min(input$topx, length(rf_results()))]
      mb_features <- mb_results()
      if (is.null(mb_features)) mb_features <- character(0)
      lasso_features <- lasso_results()
      
      all_feats <- unique(c(topx_cor, topx_rf, mb_features, lasso_features))
      
      count_in_lists <- function(feat) {
        sum(c(
          feat %in% topx_cor,
          feat %in% topx_rf,
          feat %in% mb_features,
          feat %in% lasso_features
        ))
      }
      
      subsets <- list(
        `4_algos` = all_feats[sapply(all_feats, count_in_lists) == 4],
        `3_algos` = all_feats[sapply(all_feats, count_in_lists) == 3],
        `2_algos` = all_feats[sapply(all_feats, count_in_lists) == 2],
        `1_algo`  = all_feats[sapply(all_feats, count_in_lists) == 1],
        `All_Features` = all_feats
      )
      
      if (length(rv$custom_subsets) > 0) {
        subsets <- c(subsets, rv$custom_subsets)
      }
      
      # Build a results data frame to match table structure
      subset_names <- names(subsets)
      num_features <- sapply(subsets, length)
      
      # Create a mini table to map row index to subset name
      auc_data <- data.frame(
        Subset = subset_names,
        NumFeatures = num_features,
        stringsAsFactors = FALSE
      )
      
    }, error = function(e) {
      cat("Error building subset mapping:", e$message, "\n")
      return()
    })
    
    req(auc_data)
    
    # Get the subset name from the clicked row using the data frame
    if (info$row > nrow(auc_data)) {
      shinyalert("Error", "Invalid row selection", type = "error")
      return()
    }
    
    subset_name <- auc_data$Subset[info$row]
    
    # Check if this subset has ROC data
    if (!subset_name %in% names(rv$roc_data)) {
      shinyalert("No Data", 
                 paste("No ROC data available for subset:", subset_name), 
                 type = "warning")
      return()
    }
    
    roc_set <- rv$roc_data[[subset_name]]
    
    # Column mapping (0-based indexing): 
    # 0=Subset, 1=NumFeatures, 2=Logistic, 3=RF, 4=XGBoost
    model_name <- switch(
      as.character(info$col),
      "2" = "logistic",  # Logistic column
      "3" = "rf",        # RF column
      "4" = "xgb",       # XGBoost column
      NULL
    )
    
    req(model_name)
    roc_obj <- roc_set[[model_name]]
    req(roc_obj)
    
    # STORE CURRENT MODEL INFO FOR SHAP
    rv$current_model_info <- list(
      subset_name = subset_name,
      model_name = model_name
    )
    rv$shap_values <- NULL  # Reset SHAP when new model selected
    
    output$roc_plot <- renderPlot({
      # Calculate AUC
      final_auc <- auc(roc_obj)
      
      # Get ALL points on the ROC curve
      all_coords <- coords(roc_obj, x = "all", ret = c("threshold", "sensitivity", "specificity"))
      
      # Calculate Youden's J statistic for each threshold
      youden <- all_coords$sensitivity + all_coords$specificity - 1
      
      # Find the index with maximum Youden's J (optimal threshold)
      best_index <- which.max(youden)
      
      # Get the optimal values
      optimal_threshold <- all_coords$threshold[best_index]
      optimal_sens <- all_coords$sensitivity[best_index]
      optimal_spec <- all_coords$specificity[best_index]
      
      # Plot ROC curve
      plot(
        roc_obj,
        legacy.axes = TRUE,
        col = switch(model_name,
                     "logistic" = "red",
                     "rf"       = "blue",
                     "xgb"      = "darkgreen",
                     "black"),
        lwd = 3,
        main = paste("ROC Curve -", subset_name, "-", toupper(model_name))
      )
      
      # Add legend showing AUC and optimal threshold
      legend(
        "bottomright",
        legend = c(
          sprintf("AUC = %.4f", final_auc),
          sprintf("Optimal Threshold = %.4f", optimal_threshold)
        ),
        lty = c(1, NA),
        pch = c(NA, 19),
        lwd = c(3, NA),
        col = c(
          switch(model_name,
                 "logistic" = "red",
                 "rf"       = "blue",
                 "xgb"      = "darkgreen",
                 "black"),
          "orange"
        ),
        bty = "n"
      )
    })
  }
  
  # Apply handler to AUC and MCC tables only
  observeEvent(input$auc_table_cell_clicked, { handle_table_click(input$auc_table_cell_clicked) })
  observeEvent(input$mcc_table_cell_clicked, { handle_table_click(input$mcc_table_cell_clicked) })
  observeEvent(input$recall_table_cell_clicked, { handle_table_click(input$recall_table_cell_clicked) })
  observeEvent(input$precision_table_cell_clicked, { handle_table_click(input$precision_table_cell_clicked) })
  observeEvent(input$f1_table_cell_clicked, { handle_table_click(input$f1_table_cell_clicked) })
  
  
  observeEvent(input$calc_shap, {
    req(rv$current_model_info, processed_data(), input$target_col, rv$roc_data)
    
    # Show loading alert
    shinyalert(
      title = "Calculating SHAP Values...",
      text = "This may take a minute depending on data size.",
      type = "info",
      showConfirmButton = FALSE,
      timer = 0,
      closeOnClickOutside = FALSE,
      immediate = TRUE
    )
    
    model_info <- rv$current_model_info
    subset_name <- model_info$subset_name
    model_name <- model_info$model_name
    
    df <- processed_data()
    target_col <- input$target_col
    
    # Recreate the subsets
    req(cor_results(), rf_results(), mb_completed(), lasso_results(), input$topx)
    
    topx_cor <- names(cor_results())[1:min(input$topx, length(cor_results()))]
    topx_rf <- names(rf_results())[1:min(input$topx, length(rf_results()))]
    mb_features <- mb_results()
    if (is.null(mb_features)) mb_features <- character(0)
    lasso_features <- lasso_results()
    
    all_feats <- unique(c(topx_cor, topx_rf, mb_features, lasso_features))
    
    count_in_lists <- function(feat) {
      sum(c(
        feat %in% topx_cor,
        feat %in% topx_rf,
        feat %in% mb_features,
        feat %in% lasso_features
      ))
    }
    
    subsets <- list(
      `4_algos` = all_feats[sapply(all_feats, count_in_lists) == 4],
      `3_algos` = all_feats[sapply(all_feats, count_in_lists) == 3],
      `2_algos` = all_feats[sapply(all_feats, count_in_lists) == 2],
      `1_algo`  = all_feats[sapply(all_feats, count_in_lists) == 1],
      `All_Features` = all_feats
    )
    
    if (length(rv$custom_subsets) > 0) {
      subsets <- c(subsets, rv$custom_subsets)
    }
    
    features <- subsets[[subset_name]]
    
    if (is.null(features) || length(features) == 0) {
      closeAlert()
      shinyalert("Error", 
                 paste("No features found for subset:", subset_name), 
                 type = "error")
      return()
    }
    
    features <- intersect(features, applied_predictors())
    
    if (length(features) == 0) {
      closeAlert()
      shinyalert("Error", "Selected features not found in applied predictors.", type = "error")
      return()
    }
    
    X_train <- df[, features, drop = FALSE]
    y_train <- df[[target_col]]
    
    
    # Check if we have enough data left
    if (nrow(X_train) < 10) {
      closeAlert()
      shinyalert("Error", 
                 "Not enough complete cases after removing missing values.", 
                 type = "error")
      return()
    }
    
    # Convert to matrix
    X_train <- as.matrix(X_train)
    
    tryCatch({
      library(fastshap)
      
      # Create prediction wrapper based on model type
      if (model_name == "logistic") {
        model <- glm(y_train ~ ., data = data.frame(y_train, X_train), family = binomial)
        pfun <- function(object, newdata) {
          predict(object, newdata = as.data.frame(newdata), type = "response")
        }
      } else if (model_name == "rf") {
        library(randomForest)
        model <- randomForest(x = X_train, y = as.factor(y_train), ntree = input$rf_ntree_model)
        pfun <- function(object, newdata) {
          predict(object, newdata = as.data.frame(newdata), type = "prob")[, 2]
        }
      } else if (model_name == "xgb") {
        library(xgboost)
        dtrain <- xgb.DMatrix(data = X_train, label = y_train)
        model <- xgboost(
          data = dtrain,
          nrounds = 100,
          objective = "binary:logistic",
          eta = input$xgb_eta,
          max_depth = input$xgb_max_depth,
          verbose = 0
        )
        pfun <- function(object, newdata) {
          predict(object, newdata = xgb.DMatrix(data = as.matrix(newdata)))
        }
      }
      
      # Calculate SHAP values
      n_sample <- min(20, nrow(X_train))
      sample_idx <- sample(nrow(X_train), n_sample)
      
      shap_values <- explain(
        model,
        X = X_train[sample_idx, , drop = FALSE],
        pred_wrapper = pfun,
        nsim = 10,
        adjust = TRUE
      )
      
      rv$shap_values <- shap_values
      rv$feature_values <- X_train[sample_idx, , drop = FALSE]  # Store feature values
      
      closeAlert()
      shinyalert("SHAP Complete!", 
                 paste("Calculated for", length(features), "features in", subset_name), 
                 type = "success", timer = 2000)
      
    }, error = function(e) {
      closeAlert()
      shinyalert("SHAP Error", paste("Error calculating SHAP:", e$message), type = "error")
    })
  })
}


shinyApp(ui, server)