library(xgboost)

trainXGBoost <- function(train_data, train_label){
 
  dtrain <- xgb.DMatrix(data = as.matrix(train_data), label = as.matrix(train_label))
  
  params <- list(
    objective = "multi:softmax",
    #eval_metric = "mlogloss",
    nthread = 2,
    num_class = length(unique(train_label)),
    eta_decay = .99,
    eta = .01,
    gamma = 1.15,
    max_depth = 7,
    min_child_weight = .8,
    subsample = .7,
    colsample_bytree = .6
    
  )
  
  bstDMatrix <- xgboost(param = params, data = dtrain, nrounds = 150)
  return(bstDMatrix)
}


classifyXGBoost <- function(model, test_data){
  
  return(predict(model, as.matrix(test_data)))
  
}