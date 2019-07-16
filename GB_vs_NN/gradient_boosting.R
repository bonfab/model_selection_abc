library(xgboost)

trainXGBoost <- function(train_data, train_label, iterations = 50){
 
  dtrain <- xgb.DMatrix(data = as.matrix(train_data), label = as.matrix(train_label))
  
  
  params <- list(
    objective = "multi:softmax",
    eval_metric = "mlogloss",
    nthread = 4,
    num_class = length(unique(train_label)),
    eta_decay = .99,
    eta = 0.06,
    gamma = .63,
    max_depth = 6,
    min_child_weight = 2,
    #subsample = .9,
    colsample_bytree = .75,
    lambda = 1.5,
    max_delta_step = 5,
    nrounds = 100
  )
  
  bstDMatrix <- xgboost(param = params, data = dtrain, nrounds = iterations)
  return(bstDMatrix)
}

trainXGBoostBinary <- function(train_data, train_label, iterations = 200){
  
  dtrain <- xgb.DMatrix(data = as.matrix(train_data), label = as.matrix(train_label))

params <- list(
  booster = "gbtree",
  objective = "binary:logistic",
  #eval_metric = "error",
  nthread = 8,
  #num_class = length(unique(train_label)),
  eta_decay = .999,
  eta = 0.06,
  gamma = .63,
  max_depth = 6,
  min_child_weight = .9,
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

classifyXGBoostSingle <- function(model, test_data){

  return(predict(model, t(test_data)))
}


parameterSearch <- function(){
  
  
  
  
}

save_model <- function(model, file = "./xgb_trained_simple.model"){

  xgb.save(model, file)

}

load_model <- function(file = "./xgb_trained_simple.model"){
  return(xgb.load(file))
}

