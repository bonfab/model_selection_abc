source("load_data.R")
source("gradient_boosting.R")
source("testing.R")

data <- getDataFirstN(1500)

split <- splitTrainTestSet(data)
train <- split[[1]]
test <- split[[2]]

model <- trainXGBoost(train[,-ncol(train)], as.numeric(train[,ncol(train)]) - 1, 500)

predictions <- classifyXGBoost(model, test[,-ncol(test)])

validateOutput(predictions, as.numeric(test[,ncol(test)]) - 1)
