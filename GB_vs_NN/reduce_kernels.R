library(kernlab)

make_similarity_matrix <- function(data){
  
  rbf <- rbfdot(sigma = 0.00001)
  
  #m <- apply(data, 1, function(x) apply(data, 1, function(y) rbf(x, y)))
  
  #print(length(data[,1]))
  #for(i in 1:length(data[,1])){
  #  for(j in 1:length(data[,1])){
  #    print(rbf(data[i,], data[j,]))
  #  }
  #}
  
  m <- kernelMatrix(rbf, data)
  
  return(m)
}