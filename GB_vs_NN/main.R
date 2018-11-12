source("load_data.R")
#source("gradient_boosting.R")
source("testing.R")
source("neural_net.R")

data <- getDataFirstN(100)
#data <- getData()

#data <- noramlizeData(data)

split <- splitTrainTestSet(data)
train <- split[[1]]
test <- split[[2]]

runXGBoost <- function(train, test){
  
  model <- trainXGBoost(train[,-ncol(train)], as.numeric(train[,ncol(train)]) - 1, 800)
  
  predictions <- classifyXGBoost(model, test[,-ncol(test)])
  
  validateOutput(predictions, as.numeric(test[,ncol(test)]) - 1)
  
}


runNN <- function(train, test){
  
  model <- trainNN(train[,-ncol(train)], as.numeric(train[,ncol(train)]) - 1, 40)
  
  predictions <- classifyNN(model, test[,-ncol(test)])
  
  validateOutput(predictions, as.numeric(test[,ncol(test)]) - 1)
  
}

#source("neural_net.R")
findParametersNN(train[,-ncol(train)], as.numeric(train[,ncol(train)]) - 1)



