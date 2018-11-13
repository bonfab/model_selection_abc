
noramlizeData <- function(data){
  
  for(i in 1:ncol(data)){
    
    if(is.numeric(data[1, i])){
      data[, i] <- data[, i ] / max(data[, i])
    }
  }
  
  return(data)
  
}


splitTrainTestSet <- function(data, test_portion = .1){
  
  size = ceiling(test_portion * nrow(data))
  
  test_indices <- sample(nrow(data), size = size)
  
  test = data[test_indices,]
  
  train = data[-test_indices,]
  
  return(list(train, test))
}

validateOutput <- function(test_output, real_labels){
  
  correct <- 0
  incorrect <- 0
  
  print(test_output)
  print(real_labels)
  
  for(i in 1:length(test_output)){
    if(test_output[i] == real_labels[i]){
      correct <- correct +1
    } else {
      incorrect <- incorrect + 1
    }
  }
  
  print(paste("Correct: ", correct))
  print(paste("Incorrect: ", incorrect))
  print(paste("Precision ", correct/(correct + incorrect), "%"))
  
  return(correct/(correct + incorrect))
}


crossValidate <- function(model, data, train, predict, number_folds = 10){
  
  #print(data)
  
  split <- floor(nrow(data) / number_folds)
  
  precision_sum <- 0
  
  for(i in 1:number_folds){
    
    if(i < 2){
      
      indices <- 1: (i*split)
      train_split <- data[-indices,]
      validate_split <- data[indices,]
      
      train(model, train_split[,-ncol(train_split)], as.numeric(train_split[, ncol(train_split)]) - 1)
      precision_sum <- precision_sum + validateOutput(predict(model, validate_split[,-ncol(validate_split)]), as.numeric(validate_split[, ncol(validate_split)]) - 1)
      
      
    } else if(i > number_folds-1){
      
      indices <- ((i-1)*split) :nrow(data)
      train_split <- data[-indices,]
      validate_split <- data[indices,]
      
      train(model, train_split[,-ncol(train_split)], as.numeric(train_split[, ncol(train_split)]) - 1)
      precision_sum <- precision_sum + validateOutput(predict(model, validate_split[,-ncol(validate_split)]), as.numeric(validate_split[, ncol(validate_split)]) - 1)
    
    } else {
      
      indices <- ((i-1)*split) : (i*split)
      train_split <- data[-indices,]
      validate_split <- data[indices,]
      
      
      train(model, train_split[,-ncol(train_split)], as.numeric(train_split[, ncol(train_split)]) - 1)
      precision_sum <- precision_sum + validateOutput(predict(model, validate_split[,-ncol(validate_split)]), as.numeric(validate_split[, ncol(validate_split)]) - 1)
      
    }
  }
  
  return(precision_sum/number_folds)
}

permutateData <- function(data){
  return(data[sample(nrow(data)),])
}


