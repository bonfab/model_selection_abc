source("load_data.R")
source("testing.R")
source("gradient_boosting.R")
source("neural_net.R")
#source("abc_NN.R")
Sys.setlocale("LC_MESSAGES", "en_US.utf8")


data <- getDataFirstN(30000)

data <- permutateData(data)

#data <- mergeAM_SIandIM_SC(data)
#data <- getData()

#data <- noramlizeData(data)

split <- splitTrainTestSet(data, .05)
train <- split[[1]]
test <- split[[2]]

runXGBoost <- function(train, test){
  
  model <- trainXGBoost(train[,-ncol(train)], as.numeric(train[,ncol(train)]) - 1, 300)
  
  return(classifyXGBoost(model, test[,-ncol(test)]))
  
  #predictions <- classifyXGBoost(model, test[,-ncol(test)])
  
  #validateOutput(predictions, as.numeric(test[,ncol(test)]) - 1)
  
}

runXGBoostBinary <- function(train, test){
  
  model <- trainXGBoostBinary(train[,-ncol(train)], train[,ncol(train)], 300)
  
  return(classifyXGBoost(model, test[,-ncol(test)]) > 0.5)
  
  #predictions <- classifyXGBoost(model, test[,-ncol(test)])
  
  #validateOutput(predictions, as.numeric(test[,ncol(test)]) - 1)
  
}

runNN <- function(train, test){
  
  model <- trainNNd(train[,-ncol(train)], as.numeric(train[,ncol(train)]) - 1, 30)
  
  return(classifyNN(model, test[,-ncol(test)]))
  
  #predictions <- classifyNN(model, test[,-ncol(test)])
  
}

stack <- function(model1, model2, data){
  
  folds <- makeFolds(nrow(data), 5)
  
  #metaData <- data.frame(data)
  labels <- data[,ncol(data)]
  features <- data[,-ncol(data)]
  model1_predict <- rep(NULL, nrow(data))
  
  
  
  for(indices in folds){
    model1_predict[indices] <- model1(data[-indices,], data[indices,])
  }
  
  print(unique(model1_predict))
  
  metaData <- cbind(cbind(features, model1_predict), labels)
  
  split <- splitTrainTestSet(metaData, .05)
  train <- split[[1]]
  test <- split[[2]]
  
  predictions <- model2(train, test)
  
  validateOutput(predictions, as.numeric(test[,ncol(test)]) - 1)
}


addFeatureModelCategory <- function(data, model){
  
  folds <- makeFolds(nrow(data), 5)
  
  real_labels <- data[,ncol(data)]
  
  data <- mergeAM_SIandIM_SC(data)
  
  features <- data[,-ncol(data)]
  prediction_category <- rep(NULL, nrow(data))
  
  for(indices in folds){
    prediction_category[indices] <- as.numeric(model(data[-indices,], data[indices,]))
  }
  
  metaData <- cbind(cbind(features, prediction_category), real_labels)
  
  return(metaData)
}

addBinaryFeatures <- function(data, model){
  
  folds <- makeFolds(nrow(data), 5)
  real_labels <- data[,ncol(data)]
  classes <- unique(real_labels)
  features <- data[,-ncol(data)]
  
  predictions <- matrix(-1, nrow = nrow(data), ncol = length(classes))
  
  bin_labels <- sapply(classes, makeClassLabel, true_labels = real_labels)
  
  bin_data <- cbind(features, bin_labels)
  
  sup <- 1:length(classes)
  
  for(fold in folds){
    for(i in 1:length(classes)){
      exclude <- sup[-i] + ncol(features)
      
      predictions[fold, i] <- model(bin_data[-fold,-exclude], bin_data[fold, -exclude])
    }
  }
  
  features <- cbind(features, predictions)

  return(cbind(features, real_labels))
}

data <- addBinaryFeatures(data, runXGBoostBinary)
#View(data)
#data <- addFeatureModelCategory(data, runXGBoostBinary)

#stack(runXGBoost, runNN, data)

#View(data)


#validateOutput(runXGBoost(train, test), as.numeric(test[,ncol(test)]) - 1)
#validateOutput(runNN(train, test), as.numeric(test[,ncol(test)]) - 1)
#validateOutput(as.numeric(runXGBoostBinary(train, test) > 0.5), as.numeric(test[,ncol(test)]) - 1)

crossValidate(data, runXGBoost)


useful <- function(){
#runXGBoost(train, test)
#runNN(train, test)

#source("neural_net.R")
#findParametersNN(train[,-ncol(train)], as.numeric(train[,ncol(train)]) - 1)
#data <- permutateData(data)
#model <- makeNN(c(2*ncol(data), 0.04, 2*ncol(data), 0.06), ncol(data) - 1, length(unique(data[,ncol(data)])))
#print(crossValidate(model, data, trainNN, classifyNN))

#out <- classifyABC(test[,-ncol(test)], train[,-ncol(train)], train[,ncol(train)])

#precision <- validateOutput(out, test[,ncol(test)])

#output_file <- file("output_gradientBoosting.txt")
#writeLines(toString(precision), output_file)
#close(output_file)

#t1 <- as.numeric(Sys.time())

#postpr(as.vector(test[1,-ncol(test)]), as.vector(train[,ncol(train)]), train[,-ncol(train)], tol = .05, method = "mnlogistic")

#t2 <- as.numeric(Sys.time())
#print(t2 - t1)
}

