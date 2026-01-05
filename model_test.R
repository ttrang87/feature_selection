library(pROC)
library(randomForest)
library(xgboost)
library(caTools)

source('xgboost.R')

calculate_metrics <- function(roc_obj, actual) {
  # optimal threshold (Youden's J statistic)
  coords_obj <- coords(roc_obj, "best", ret = c("threshold", "sensitivity", "specificity"))
  optimal_threshold <- coords_obj$threshold
  
  # predictions at optimal threshold
  pred_probs <- roc_obj$predictor
  pred_class <- ifelse(pred_probs > optimal_threshold, 1, 0)
  
  # confusion matrix
  tp <- sum(pred_class == 1 & actual == 1)
  tn <- sum(pred_class == 0 & actual == 0)
  fp <- sum(pred_class == 1 & actual == 0)
  fn <- sum(pred_class == 0 & actual == 1)
  
  # MCC
  numerator <- (tp * tn) - (fp * fn)
  denominator <- sqrt((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn))
  mcc <- ifelse(denominator == 0, 0, numerator / denominator)
  
  # Recall (Sensitivity)
  recall <- ifelse((tp + fn) == 0, 0, tp / (tp + fn))
  
  # Precision
  precision <- ifelse((tp + fp) == 0, 0, tp / (tp + fp))
  
  # F1 Score
  f1 <- ifelse((precision + recall) == 0, 0, 2 * (precision * recall) / (precision + recall))
  
  return(c(
    MCC = mcc,
    Sensitivity = coords_obj$sensitivity,
    Specificity = coords_obj$specificity,
    Recall = recall,
    Precision = precision,
    F1 = f1
  ))
}

train_auc_matrix_multi <- function(topx_cor, topx_rf, mb_features, lasso_features, 
                                   df_clean, target_col,
                                   split_ratio = 0.8,
                                   xgb_eta = 0.05,
                                   xgb_max_depth = 6,
                                   rf_ntree = 500,
                                   cv_folds = 5,
                                   custom_subsets = list(),
                                   train_data = NULL,
                                   test_data = NULL) {
  # ---- 1. Combine all unique features ----
  all_feats <- unique(c(topx_cor, topx_rf, mb_features, lasso_features))
  
  # helper: count how many methods contain each feature
  count_in_lists <- function(f) {
    sum(
      f %in% topx_cor,
      f %in% topx_rf,
      f %in% mb_features,
      f %in% lasso_features
    )
  }
  
  # ---- 2. Split into subsets based on number of methods overlapped ----
  subsets <- list(
    `4_algos` = all_feats[sapply(all_feats, count_in_lists) == 4],
    `3_algos` = all_feats[sapply(all_feats, count_in_lists) == 3],
    `2_algos` = all_feats[sapply(all_feats, count_in_lists) == 2],
    `1_algo`  = all_feats[sapply(all_feats, count_in_lists) == 1],
    `All_Features` = all_feats
  )
  if (length(custom_subsets) > 0) {
    for (subset_name in names(custom_subsets)) {
      # Ensure custom subset features are in df_clean
      valid_features <- intersect(custom_subsets[[subset_name]], colnames(df_clean))
      if (length(valid_features) > 0) {
        subsets[[subset_name]] <- valid_features
      }
    }
  }
  # ---- 3. Initialize result table ----
  results <- data.frame(
    Subset = character(),
    NumFeatures = numeric(),
    Logistic_AUC = numeric(),
    Logistic_MCC = numeric(),
    Logistic_Recall = numeric(),
    Logistic_Precision = numeric(),
    Logistic_F1 = numeric(),
    RF_AUC = numeric(),
    RF_MCC = numeric(),
    RF_Recall = numeric(),
    RF_Precision = numeric(),
    RF_F1 = numeric(),
    XGB_AUC = numeric(),
    XGB_MCC = numeric(),
    XGB_Recall = numeric(),
    XGB_Precision = numeric(),
    XGB_F1 = numeric(),
    stringsAsFactors = FALSE
  )
  
  roc_list <- list()
  
  # ---- 4. Loop through subsets ----
  for (name in names(subsets)) {
    feats <- subsets[[name]]
    if (length(feats) == 0) {
      results <- rbind(results, data.frame(
        Subset = name,
        NumFeatures = 0,
        Logistic_AUC = NA,
        Logistic_MCC = NA,
        Logistic_Recall = NA,
        Logistic_Precision = NA,
        Logistic_F1 = NA,
        RF_AUC = NA,
        RF_MCC = NA,
        RF_Recall = NA,
        RF_Precision = NA,
        RF_F1 = NA,
        XGB_AUC = NA,
        XGB_MCC = NA,
        XGB_Recall = NA,
        XGB_Precision = NA,
        XGB_F1 = NA
      ))
      next
    }
    # ============================
    # DATA PREPARATION
    # ============================
    use_presplit <- !is.null(train_data) && !is.null(test_data)
    
    if (use_presplit) {
      # Use pre-split train/test data AS-IS
      train_df_full <- train_data[, c(target_col, feats), drop = FALSE]
      test_df_full <- test_data[, c(target_col, feats), drop = FALSE]
      
      # Just use the data as provided - no resampling
      train_y <- train_df_full[[target_col]]
      test_y <- test_df_full[[target_col]]
      
      train_X_raw <- train_df_full[, feats, drop = FALSE]
      test_X_raw <- test_df_full[, feats, drop = FALSE]
      
      # Convert to model matrix
      train_X <- model.matrix(~ . - 1, data = train_X_raw)
      test_X <- model.matrix(~ . - 1, data = test_X_raw)
      
      # Calculate actual ratio for reference
      actual_ratio <- nrow(train_df_full) / (nrow(train_df_full) + nrow(test_df_full))
      cat("Using pre-split data | Train:", nrow(train_df_full), 
          "Test:", nrow(test_df_full), 
          "Ratio:", round(actual_ratio, 3), "\n")
      
    } else {
      # Use automatic split
      df_sub <- df_clean[, c(target_col, feats), drop = FALSE]
      y <- df_sub[[target_col]]
      X_raw <- df_sub[, feats, drop = FALSE]
      
      # Convert to model matrix
      X <- model.matrix(~ . - 1, data = X_raw)
      
      # Split data
      split <- sample.split(y, SplitRatio = split_ratio)
      train_X <- X[split, ]
      test_X  <- X[!split, ]
      train_y <- y[split]
      test_y  <- y[!split]
    }
    
    # ============================
    #  Logistic Regression
    # ============================
    
    # IMPORTANT: Align test_X columns with train_X BEFORE creating data frames
    missing_cols <- setdiff(colnames(train_X), colnames(test_X))
    extra_cols <- setdiff(colnames(test_X), colnames(train_X))
    
    if (length(missing_cols) > 0) {
      # Add missing columns as zeros - create matrix with correct number of rows
      missing_matrix <- matrix(0, nrow = nrow(test_X), ncol = length(missing_cols))
      colnames(missing_matrix) <- missing_cols
      test_X <- cbind(test_X, missing_matrix)
    }
    
    if (length(extra_cols) > 0) {
      # Remove extra columns
      test_X <- test_X[, !colnames(test_X) %in% extra_cols, drop = FALSE]
    }
    
    # Reorder test_X to match train_X column order
    test_X <- test_X[, colnames(train_X), drop = FALSE]
    
    # Build data frames with SAME structure
    train_df <- data.frame(train_X, check.names = FALSE)
    train_df[[target_col]] <- train_y
    
    test_df <- data.frame(test_X, check.names = FALSE)
    
    # Train model
    log_formula <- as.formula(paste0("`", target_col, "` ~ ."))
    log_model <- glm(log_formula, data = train_df, family = binomial)
    
    # Predict - column names now match exactly
    log_preds <- predict(log_model, newdata = test_df, type = "response")
    log_ROC <- roc(
      response = test_y,
      predictor = log_preds,
      auc.polygon = TRUE,
      grid = TRUE,
      plot = FALSE,
      direction = "<"
    )
    log_auc <- auc(log_ROC)
    log_metrics <- calculate_metrics(log_ROC, test_y)  # ADD THIS LINE
    
    # ============================
    #  Random Forest (Train/Test Split)
    # ============================
    rf_model <- randomForest(x = train_X, y = as.factor(train_y), ntree = rf_ntree)
    rf_preds <- predict(rf_model, newdata = test_X, type = "prob")[, 2]
    rf_ROC <- roc(
      response = test_y,
      predictor = rf_preds,
      auc.polygon = TRUE,
      grid = TRUE,
      plot = FALSE,
      direction = "<"
    )
    rf_auc <- auc(rf_ROC)
    rf_metrics <- calculate_metrics(rf_ROC, test_y)  # ADD THIS LINE
    
    # ============================
    #  XGBoost 
    # ============================
    xgb_res <- train_xgboost_auc(train_X, train_y, test_X, test_y, 
                                 eta = xgb_eta, max_depth = xgb_max_depth, nfold = cv_folds)
    xgb_auc <- xgb_res$auc 
    xgb_roc <- xgb_res$roc
    xgb_metrics <- calculate_metrics(xgb_roc, test_y) 
    
    # ---- Store results ----
    results <- rbind(results, data.frame(
      Subset = name,
      NumFeatures = length(feats),
      Logistic_AUC = log_auc,
      Logistic_MCC = log_metrics["MCC"],
      Logistic_Recall = log_metrics["Recall"],
      Logistic_Precision = log_metrics["Precision"],
      Logistic_F1 = log_metrics["F1"],
      RF_AUC = rf_auc,
      RF_MCC = rf_metrics["MCC"],
      RF_Recall = rf_metrics["Recall"],
      RF_Precision = rf_metrics["Precision"],
      RF_F1 = rf_metrics["F1"],
      XGB_AUC = xgb_auc,
      XGB_MCC = xgb_metrics["MCC"],
      XGB_Recall = xgb_metrics["Recall"],
      XGB_Precision = xgb_metrics["Precision"],
      XGB_F1 = xgb_metrics["F1"]
    ))
    
    roc_list[[name]] <- list(
      logistic = log_ROC,
      rf = rf_ROC,
      xgb = xgb_roc
    )
  }
  
  # ---- 5. Return table + ROC ----
  return(list(results = results, rocs = roc_list))
}
