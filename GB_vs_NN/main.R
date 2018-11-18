source("load_data.R")
source("gradient_boosting.R")
source("testing.R")
source("neural_net.R")
source("abc_NN.R")

data <- getDataFirstN(2000)
#data <- getData()

#data <- noramlizeData(data)

split <- splitTrainTestSet(data, .05)
train <- split[[1]]
test <- split[[2]]

runXGBoost <- function(train, test){
  
  model <- trainXGBoost(train[,-ncol(train)], as.numeric(train[,ncol(train)]) - 1, 200)
  
  predictions <- classifyXGBoost(model, test[,-ncol(test)])
  
  validateOutput(predictions, as.numeric(test[,ncol(test)]) - 1)
  
}


runNN <- function(train, test){
  
  model <- trainNNd(train[,-ncol(train)], as.numeric(train[,ncol(train)]) - 1, 30)
  
  predictions <- classifyNN(model, test[,-ncol(test)])
  
  validateOutput(predictions, as.numeric(test[,ncol(test)]) - 1)
  
}

runXGBoost(train, test)
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


