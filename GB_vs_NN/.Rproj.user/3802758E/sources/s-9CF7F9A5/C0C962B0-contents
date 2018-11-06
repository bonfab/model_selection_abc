source("load_data.R")
source("gradient_boosting.R")
source("testing.R")

data <- getDataFirstN()

split <- splitTrainTestSet(data)
summary(split)
train <- split[[1]]
test <- split[[2]]

model <- trainXGBoost(train[,-ncol(train)], as.numeric(train[,ncol(train)]) - 1)

predictions <- classifyXGBoost(model, test[,-ncol(test)])

print(predictions)
