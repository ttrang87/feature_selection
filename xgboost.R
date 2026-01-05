# ====================================
# XGBoost Training & Evaluation Module
# ====================================
library(xgboost)
library(pROC)

train_xgboost_auc <- function(train_X, train_y, test_X, test_y,
                              eta = 0.05, max_depth = 6, nfold = 5) {
  
  # Ensure inputs are matrices and get feature names
  train_X <- as.matrix(train_X)
  test_X <- as.matrix(test_X)
  feature_names <- colnames(train_X)
  
  # Convert data frames to DMatrix format
  dtrain <- xgb.DMatrix(data = train_X, label = train_y)
  dtest  <- xgb.DMatrix(data = test_X,  label = test_y)
  
  # CRITICAL FIX: Set feature names on DMatrix objects
  colnames(dtrain) <- feature_names
  colnames(dtest) <- feature_names
  
  # XGBoost parameters (tunable)
  params <- list(
    objective = "binary:logistic",
    eval_metric = "auc",
    eta = eta,                 # learning rate
    max_depth = max_depth,     # tree depth
    subsample = 0.8,           # row sampling
    colsample_bytree = 0.8,    # feature sampling
    min_child_weight = 1,
    gamma = 1
  )
  
  # ================================
  # 1. Cross-validation to find best nrounds
  # ================================
  set.seed(123)
  xgb_cv <- xgb.cv(
    params = params,
    data = dtrain,
    nrounds = 1000,
    nfold = nfold,
    early_stopping_rounds = 30,
    verbose = 0
  )
  
  best_nrounds <- xgb_cv$best_iteration
  
  # ================================
  # 2. Train Final Model
  # ================================
  xgb_model <- xgb.train(
    params = params,
    data = dtrain,
    nrounds = best_nrounds,
    verbose = 0
  )
  
  # ================================
  # 3. Predict and Compute AUC
  # ================================
  preds <- predict(xgb_model, newdata = dtest)
  xgb_ROC <- roc(
    response = test_y,
    predictor = preds,
    auc.polygon = TRUE,
    grid = TRUE,
    plot = FALSE,
    direction = "<"
  )
  auc_val <- auc(xgb_ROC)
  
  # ================================
  # 4. Variable Importance (with error handling)
  # ================================
  importance <- tryCatch({
    xgb.importance(
      feature_names = feature_names,  # Explicitly pass feature names
      model = xgb_model
    )
  }, error = function(e) {
    # Fallback if importance extraction fails
    warning("Could not extract XGBoost importance: ", e$message)
    data.frame(
      Feature = feature_names,
      Gain = rep(0, length(feature_names)),
      Cover = rep(0, length(feature_names)),
      Frequency = rep(0, length(feature_names)),
      stringsAsFactors = FALSE
    )
  })
  
  # return both auc and importance table
  return(list(
    auc = auc_val,
    roc = xgb_ROC,
    importance = importance,
    model = xgb_model
  ))
}