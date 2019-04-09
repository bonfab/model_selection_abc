source("gradient_boosting.R")
source("testing.R")


load_data2 <- function(RDS_file = "data_K/nr1_data_pop_2-16.rds"){
  
  data <- readRDS(RDS_file)

  data2 <- readRDS("data_K/nr2_data_pop_2-16.rds")

  m <- matrix(0, nrow = nrow(data[[1]]) + nrow(data2[[1]]), ncol = ncol(data[[1]])+1)


  #print(data)
  #print(data[[1]])
  for(i in 1:ncol(data[[1]])){
    for(j in 1:nrow(data[[1]])){
      #print(list(i,j))
      m[j, i] <- as.numeric(data[[1]][j, i])
    }
  }

  for(i in 1:ncol(data2[[1]])){
    for(j in (nrow(data[[1]])+1):(nrow(data[[1]])+nrow(data2[[1]]))){
      m[j, i] <- as.numeric(data2[[1]][j - nrow(data[[1]]), i])
    }
  }


  for(i in 1:nrow(data[[1]])){
    m[i, ncol(data[[1]])+1] <- data[[2]][i]
  }

  for(i in (nrow(data[[1]])+1):(nrow(data[[1]])+nrow(data2[[1]]))){
    m[i, ncol(data[[1]])+1] <- data2[[2]][i-nrow(data[[1]])]
  }

  return(m)
  #print(m)
  #print(typeof(m))
  #return(cbind(data[[1]], data[[2]]))
  
}

load_data <- function(RDS_file = "data_K/nr1_data_pop_2-16.rds"){

  data <- readRDS(RDS_file)


  m <- matrix(0, nrow = nrow(data[[1]]), ncol = ncol(data[[1]])+1)


  #print(data)
  #print(data[[1]])
  for(i in 1:ncol(data[[1]])){
    for(j in 1:nrow(data[[1]])){
      #print(list(i,j))
      m[j, i] <- as.numeric(data[[1]][j, i])
    }
  }

  for(i in 1:nrow(data[[1]])){
    m[i, ncol(data[[1]])+1] <- data[[2]][i]
  }


  return(m)
  #print(m)
  #print(typeof(m))
  #return(cbind(data[[1]], data[[2]]))

}


get_test_indices <- function(labels, proportion = 0.1){
  
  test_indices <- list()
  
  for(class in unique(labels)){
    
    class_indices <- which(labels == class)
    test_indices <- append(test_indices, class_indices[1:floor(length(class_indices) * 0.1)])
    
  }
  
  return(unlist(test_indices, use.names = F))
}


train_test <- function(m){
  indices <- get_test_indices(m[,ncol(m)])

  #print(indices)
  #print(as.vector(indices))

  train <- m[-indices,]
  train <- train[sample(nrow(train)),]
  test <- m[indices,]
  test <- test[sample(nrow(test)),]  # not really necessary

  print(min(m[,length(m[1,])]))

  xgb_compatible <- min(train[,length(train[1,])])

  model <- trainXGBoost(train[,1:ncol(train) -1], train[,ncol(train)] - xgb_compatible)

  predictions <- classifyXGBoost(model, test[,1:ncol(test)-1])

  validateOutput(predictions, test[,ncol(test)] - xgb_compatible)

}

train_save <- function(train){

  xgb_compatible <- min(train[,length(train[1,])])
  train <- train[sample(nrow(train)),]
  model <- trainXGBoost(train[,1:ncol(train) -1], train[,ncol(train)] - xgb_compatible)

  save_model(model)

}

m <- load_data2()

#train_test(m)

train_save(m)





