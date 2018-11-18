library(xgboost)

trainXGBoost <- function(train_data, train_label, iterations = 200){
 
  dtrain <- xgb.DMatrix(data = as.matrix(train_data), label = as.matrix(train_label))
  
  params <- list(
    objective = "multi:softmax",
    eval_metric = "mlogloss",
    nthread = 12,
    num_class = length(unique(train_label)),
    eta_decay = .999,
    eta = 0.07,
    gamma = .63,
    max_depth = 6,
    min_child_weight = .66,
    #subsample = .9,
    colsample_bytree = .75,
    lambda = 1.5,
    feature_selector = "thrifty"
  )
  
  bstDMatrix <- xgboost(param = params, data = dtrain, nrounds = iterations)
  return(bstDMatrix)
}


classifyXGBoost <- function(model, test_data){
  
  return(predict(model, as.matrix(test_data)))
}
