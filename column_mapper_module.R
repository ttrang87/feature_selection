# column_mapper_module.R
# Save this as a separate file in the same directory as app.R

# UI Module
columnMapperUI <- function(id) {
  ns <- NS(id)
  tagList()  # No UI needed in main page, modal will be created dynamically
}

# Server Module
columnMapperServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Store current column being mapped
    current_column <- reactiveVal(NULL)
    column_unique_values <- reactiveVal(NULL)
    column_mappings <- reactiveVal(list())  # Store all mappings
    
    # Function to show mapping modal
    showMappingModal <- function(col_name) {
      req(data(), col_name)
      
      df <- data()
      
      # Check if column exists
      if (!col_name %in% names(df)) {
        shinyalert("Error", paste("Column", col_name, "not found"), type = "error")
        return()
      }
      
      col_data <- df[[col_name]]
      unique_vals <- sort(unique(na.omit(col_data)))
      is_numeric_col <- is.numeric(col_data)
      
      current_column(col_name)
      column_unique_values(unique_vals)
      
      # Determine available methods based on column type
      if (is_numeric_col) {
        method_choices <- c(
          "Value-to-Value Mapping" = "value_map",
          "Threshold (Greater Than)" = "threshold_gt",
          "Threshold (Less Than)" = "threshold_lt",
          "Threshold (Greater or Equal)" = "threshold_gte",
          "Threshold (Less or Equal)" = "threshold_lte",
          "Range Mapping" = "range"
        )
      } else {
        # For categorical, only show value mapping
        method_choices <- c("Value-to-Value Mapping" = "value_map")
      }
      
      # STEP 1: Method selection modal
      showModal(modalDialog(
        title = paste("Map Values for:", col_name),
        size = "m",
        
        div(
          style = "background-color: #f0f8ff; padding: 10px; border-radius: 5px; margin-bottom: 15px;",
          strong(paste("Column Type:", if(is_numeric_col) "Numeric" else "Categorical")),
          br(),
          strong(paste("Found", length(unique_vals), "unique values")),
          br(),
          helpText("Choose how to map this column's values:")
        ),
        
        # Mapping method selection
        radioButtons(
          ns("mapping_method"),
          "Mapping Method:",
          choices = method_choices,
          selected = "value_map"
        ),
        
        footer = tagList(
          actionButton(ns("next_to_inputs"), "Next", class = "btn btn-primary"),
          modalButton("Cancel")
        )
      ))
    }
    
    # Step 2: Show input fields after clicking Next
    observeEvent(input$next_to_inputs, {
      req(current_column(), input$mapping_method, column_unique_values())
      
      col_name <- current_column()
      method <- input$mapping_method
      unique_vals <- column_unique_values()
      
      # Determine if numeric for median calculation
      df <- data()
      is_numeric_col <- is.numeric(df[[col_name]])
      
      # Build the appropriate UI based on method
      if (method == "value_map") {
        # Value-to-value mapping inputs
        input_ui <- div(
          h5("Map Each Value:"),
          helpText("Specify what number each unique value should become:"),
          div(
            style = "max-height: 400px; overflow-y: auto;",
            lapply(seq_along(unique_vals), function(i) {
              val <- unique_vals[i]
              div(
                style = "margin: 10px 0; padding: 10px; border: 1px solid #ddd; border-radius: 5px; background-color: #fafafa;",
                fluidRow(
                  column(5, 
                         strong(style = "font-size: 16px;", as.character(val)),
                         style = "padding-top: 7px;"
                  ),
                  column(1,
                         div(style = "text-align: center; padding-top: 7px; font-size: 18px;", "→")
                  ),
                  column(6,
                         numericInput(
                           ns(paste0("map_", i)),
                           NULL,
                           value = i - 1,
                           step = 1,
                           width = "100%"
                         )
                  )
                )
              )
            })
          )
        )
        
      } else if (method == "threshold_gt") {
        input_ui <- div(
          numericInput(
            ns("threshold_value"),
            "Threshold Value:",
            value = if(is_numeric_col) median(unique_vals, na.rm = TRUE) else 0,
            step = if(is_numeric_col && all(unique_vals == floor(unique_vals))) 1 else 0.1
          ),
          fluidRow(
            column(6,
                   div(
                     style = "padding: 10px; background-color: #e8f5e9; border-radius: 5px;",
                     strong("Values > threshold"),
                     numericInput(ns("map_above"), "Map to:", value = 1, width = "100%")
                   )
            ),
            column(6,
                   div(
                     style = "padding: 10px; background-color: #fff3e0; border-radius: 5px;",
                     strong("Values ≤ threshold"),
                     numericInput(ns("map_below"), "Map to:", value = 0, width = "100%")
                   )
            )
          )
        )
        
      } else if (method == "threshold_lt") {
        input_ui <- div(
          numericInput(
            ns("threshold_value"),
            "Threshold Value:",
            value = if(is_numeric_col) median(unique_vals, na.rm = TRUE) else 0,
            step = if(is_numeric_col && all(unique_vals == floor(unique_vals))) 1 else 0.1
          ),
          fluidRow(
            column(6,
                   div(
                     style = "padding: 10px; background-color: #e8f5e9; border-radius: 5px;",
                     strong("Values < threshold"),
                     numericInput(ns("map_above"), "Map to:", value = 1, width = "100%")
                   )
            ),
            column(6,
                   div(
                     style = "padding: 10px; background-color: #fff3e0; border-radius: 5px;",
                     strong("Values ≥ threshold"),
                     numericInput(ns("map_below"), "Map to:", value = 0, width = "100%")
                   )
            )
          )
        )
        
      } else if (method == "threshold_gte") {
        input_ui <- div(
          numericInput(
            ns("threshold_value"),
            "Threshold Value:",
            value = if(is_numeric_col) median(unique_vals, na.rm = TRUE) else 0,
            step = if(is_numeric_col && all(unique_vals == floor(unique_vals))) 1 else 0.1
          ),
          fluidRow(
            column(6,
                   div(
                     style = "padding: 10px; background-color: #e8f5e9; border-radius: 5px;",
                     strong("Values ≥ threshold"),
                     numericInput(ns("map_above"), "Map to:", value = 1, width = "100%")
                   )
            ),
            column(6,
                   div(
                     style = "padding: 10px; background-color: #fff3e0; border-radius: 5px;",
                     strong("Values < threshold"),
                     numericInput(ns("map_below"), "Map to:", value = 0, width = "100%")
                   )
            )
          )
        )
        
      } else if (method == "threshold_lte") {
        input_ui <- div(
          numericInput(
            ns("threshold_value"),
            "Threshold Value:",
            value = if(is_numeric_col) median(unique_vals, na.rm = TRUE) else 0,
            step = if(is_numeric_col && all(unique_vals == floor(unique_vals))) 1 else 0.1
          ),
          fluidRow(
            column(6,
                   div(
                     style = "padding: 10px; background-color: #e8f5e9; border-radius: 5px;",
                     strong("Values ≤ threshold"),
                     numericInput(ns("map_above"), "Map to:", value = 1, width = "100%")
                   )
            ),
            column(6,
                   div(
                     style = "padding: 10px; background-color: #fff3e0; border-radius: 5px;",
                     strong("Values > threshold"),
                     numericInput(ns("map_below"), "Map to:", value = 0, width = "100%")
                   )
            )
          )
        )
        
      } else if (method == "range") {
        input_ui <- div(
          h5("Define Range:"),
          fluidRow(
            column(6,
                   numericInput(ns("range_low"), "Lower Bound:", 
                                value = if(is_numeric_col) min(unique_vals, na.rm = TRUE) else 0)
            ),
            column(6,
                   numericInput(ns("range_high"), "Upper Bound:", 
                                value = if(is_numeric_col) max(unique_vals, na.rm = TRUE) else 1)
            )
          ),
          br(),
          fluidRow(
            column(6,
                   div(
                     style = "padding: 10px; background-color: #e8f5e9; border-radius: 5px;",
                     strong("Values in range [low, high]"),
                     numericInput(ns("map_in_range"), "Map to:", value = 1, width = "100%")
                   )
            ),
            column(6,
                   div(
                     style = "padding: 10px; background-color: #fff3e0; border-radius: 5px;",
                     strong("Values outside range"),
                     numericInput(ns("map_out_range"), "Map to:", value = 0, width = "100%")
                   )
            )
          )
        )
      }
      
      # Show new modal with inputs
      showModal(modalDialog(
        title = paste("Configure Mapping -", col_name),
        size = "l",
        
        div(
          style = "background-color: #e3f2fd; padding: 10px; border-radius: 5px; margin-bottom: 15px;",
          strong(paste("Method:", switch(method,
                                         "value_map" = "Value-to-Value Mapping",
                                         "threshold_gt" = "Threshold (Greater Than)",
                                         "threshold_lt" = "Threshold (Less Than)",
                                         "threshold_gte" = "Threshold (Greater or Equal)",
                                         "threshold_lte" = "Threshold (Less or Equal)",
                                         "range" = "Range Mapping")))
        ),
        
        input_ui,
        
        footer = tagList(
          actionButton(ns("back_to_method"), "Back", class = "btn btn-default"),
          actionButton(ns("apply_mapping"), "Apply Mapping", class = "btn btn-success"),
          modalButton("Cancel")
        )
      ))
    })
    
    # Back button: return to method selection
    observeEvent(input$back_to_method, {
      showMappingModal(current_column())
    })
    
  
    
    observeEvent(input$apply_mapping, {
      req(current_column(), data())
      
      df <- data()
      col_name <- current_column()
      
      # Apply the mapping based on method
      mapped_df <- apply_column_mapping(
        df,
        col_name,
        input$mapping_method,
        column_unique_values(),
        input,
        ns
      )
      
      # Update ONLY the specific column, not entire dataframe
      df[[col_name]] <- mapped_df[[col_name]]
      
      # Store the mapping info
      mappings <- column_mappings()
      mappings[[col_name]] <- list(
        method = input$mapping_method,
        details = get_mapping_details(input$mapping_method, column_unique_values(), input, ns)
      )
      column_mappings(mappings)
      
      # Update the data
      data(df)
      
      removeModal()
      shinyalert("Success!", 
                 paste("Column", col_name, "has been mapped successfully."), 
                 type = "success", timer = 2000)
    })
    
    # Return functions and reactive values
    list(
      showModal = showMappingModal,
      mappings = column_mappings,
      data = data
    )
  })
}

# Helper function to create mapping preview
create_mapping_preview <- function(col_data, method, unique_vals, input, ns) {
  if (method == "value_map") {
    # Value-to-value mapping
    mapping <- sapply(seq_along(unique_vals), function(i) {
      val <- input[[paste0("map_", i)]]
      if (is.null(val)) return(i - 1)
      return(val)
    })
    
    data.frame(
      Original = as.character(unique_vals),
      Mapped = mapping,
      Count = sapply(unique_vals, function(v) sum(col_data == v, na.rm = TRUE)),
      stringsAsFactors = FALSE
    )
    
  } else if (method %in% c("threshold_gt", "threshold_lt", "threshold_gte", "threshold_lte")) {
    # Threshold mapping
    threshold <- input$threshold_value
    map_above <- input$map_above
    map_below <- input$map_below
    
    if (is.null(threshold)) threshold <- median(unique_vals, na.rm = TRUE)
    if (is.null(map_above)) map_above <- 1
    if (is.null(map_below)) map_below <- 0
    
    conditions <- switch(method,
                         "threshold_gt" = col_data > threshold,
                         "threshold_lt" = col_data < threshold,
                         "threshold_gte" = col_data >= threshold,
                         "threshold_lte" = col_data <= threshold)
    
    data.frame(
      Condition = c(
        paste(switch(method,
                     "threshold_gt" = ">",
                     "threshold_lt" = "<",
                     "threshold_gte" = "≥",
                     "threshold_lte" = "≤"), threshold),
        "Other"
      ),
      Mapped = c(map_above, map_below),
      Count = c(sum(conditions, na.rm = TRUE), sum(!conditions, na.rm = TRUE)),
      stringsAsFactors = FALSE
    )
    
  } else if (method == "range") {
    # Range mapping
    low <- input$range_low
    high <- input$range_high
    
    if (is.null(low)) low <- min(unique_vals, na.rm = TRUE)
    if (is.null(high)) high <- max(unique_vals, na.rm = TRUE)
    
    in_range <- col_data >= low & col_data <= high
    
    data.frame(
      Condition = c(paste0("[", low, ", ", high, "]"), "Outside range"),
      Mapped = c(input$map_in_range %||% 1, input$map_out_range %||% 0),
      Count = c(sum(in_range, na.rm = TRUE), sum(!in_range, na.rm = TRUE)),
      stringsAsFactors = FALSE
    )
  }
}

# Helper function to apply mapping
apply_column_mapping <- function(df, col_name, method, unique_vals, input, ns) {
  col_data <- df[[col_name]]
  
  if (method == "value_map") {
    # Create mapping vector
    mapping <- setNames(
      sapply(seq_along(unique_vals), function(i) {
        val <- input[[paste0("map_", i)]]
        if (is.null(val)) return(i - 1)
        return(val)
      }),
      as.character(unique_vals)
    )
    
    # Apply mapping
    df[[col_name]] <- as.numeric(unname(mapping[as.character(col_data)]))
    
  } else if (method == "threshold_gt") {
    df[[col_name]] <- ifelse(col_data > input$threshold_value, 
                             input$map_above, input$map_below)
    
  } else if (method == "threshold_lt") {
    df[[col_name]] <- ifelse(col_data < input$threshold_value, 
                             input$map_above, input$map_below)
    
  } else if (method == "threshold_gte") {
    df[[col_name]] <- ifelse(col_data >= input$threshold_value, 
                             input$map_above, input$map_below)
    
  } else if (method == "threshold_lte") {
    df[[col_name]] <- ifelse(col_data <= input$threshold_value, 
                             input$map_above, input$map_below)
    
  } else if (method == "range") {
    in_range <- col_data >= input$range_low & col_data <= input$range_high
    df[[col_name]] <- ifelse(in_range, input$map_in_range, input$map_out_range)
  }
  
  cat("\n=== DEBUG: After mapping column:", col_name, "===\n")
  cat("Class:", class(df[[col_name]]), "\n")
  cat("Type:", typeof(df[[col_name]]), "\n")
  cat("Is numeric:", is.numeric(df[[col_name]]), "\n")
  cat("First 10 values:", head(df[[col_name]], 10), "\n")
  cat("Unique values:", unique(df[[col_name]]), "\n")
  
  return(df)
}

# Helper function to get mapping details for storage
get_mapping_details <- function(method, unique_vals, input, ns) {
  if (method == "value_map") {
    mapping <- setNames(
      sapply(seq_along(unique_vals), function(i) {
        val <- input[[paste0("map_", i)]]
        if (is.null(val)) return(i - 1)
        return(val)
      }),
      as.character(unique_vals)
    )
    return(list(type = "value_map", mapping = mapping))
    
  } else if (method %in% c("threshold_gt", "threshold_lt", "threshold_gte", "threshold_lte")) {
    return(list(
      type = method,
      threshold = input$threshold_value,
      map_above = input$map_above,
      map_below = input$map_below
    ))
    
  } else if (method == "range") {
    return(list(
      type = "range",
      low = input$range_low,
      high = input$range_high,
      map_in = input$map_in_range,
      map_out = input$map_out_range
    ))
  }
}

# Helper for NULL coalescing
`%||%` <- function(a, b) if (is.null(a)) b else a