
splitTrainTestSet <- function(data, test_portion = .1){
  
  size = ceiling(test_portion * nrow(data))
  
  test_indices <- sample(nrow(data), size = size)
  
  print(test_indices)
  
  test = data[test_indices,]
  
  train = data[-test_indices,]
  
  return(list(train, test))
}

validateOutput <- function(test_output, real_labels){
  
  correct <- 0
  incorrect <- 0
  
  for(i in 1:length(test_output)){
    if(test_output[i] == real_labels[i]){
      correct <- correct +1
    } else {
      incorrect <- incorrect +1
    }
  }
  
  print("Correct: ")
  print(correct)
  print("Incorrect: ")
  print(incorrect)
}