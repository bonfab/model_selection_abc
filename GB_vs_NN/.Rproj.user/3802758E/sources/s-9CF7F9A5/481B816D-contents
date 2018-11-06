
splitTrainTestSet <- function(data, test_portion = .1){
  
  size = ceiling(test_portion * nrow(data))
  
  test_indices <- sample(nrow(data), size = size)
  
  print(test_indices)
  
  test = data[test_indices,]
  
  train = data[-test_indices,]
  
  return(list(train, test))
}