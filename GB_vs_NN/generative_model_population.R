library(parallel)

generate <- function(K, number_locus, pop_size = 100, alpha = 1, beta = 1){
  
  #set.seed(1)
  pop <- replicate(number_locus, replicate(K, rbinom(n = pop_size, size = 1, prob = rbeta(1, alpha, beta))), simplify = T)
  
  return(pop)
}



PCA_summary <- function(data, number_components = 10){
  
  print(data)
  pca <- prcomp(data)
  print(pca)
  plot(pca)
  summary(pca)
}


PCA_summary(generate(2, 4, 3))