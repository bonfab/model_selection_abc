#library(parallel)
library(MCMCpack)
library(LaplacesDemon)

generate_simple <- function(K, number_locus, pop_size = 100, alpha = .1, beta = 1){
  
  #set.seed(1)
  pop <- replicate(number_locus, replicate(K, rbinom(n = pop_size, size = 1, prob = rbeta(1, alpha, beta))), simplify = T)
  #print(pop)
  return(pop)
}

generate_correlated <- function(K, number_locus, pop_size = 25, number_alleles = 8, alphas = NULL){
  
  
  correlate_locus <- function(){
    corr <- rgamma(1, 7.5)
    return(replicate(K, rcat(n = pop_size, p =rdirichlet(1, rep(corr, times = number_alleles))[1,])))
  }
  
  pop <- replicate(number_locus, correlate_locus(), simplify = T)
  
  return(pop)
}

generate_simple_dirichlet <- function(K, number_locus, pop_size = 25, number_alleles = 8, alphas = NULL){
  
  if(is.null(alphas)){
    alphas = rep(1, times = number_alleles)
  }
  
  pop <- replicate(number_locus, replicate(K, rcat(n = pop_size, p =rdirichlet(1, alphas)[1,])), simplify = T)
  
  return(pop)
}



PCA_summary <- function(data, number_components = 10){
  
  print(data)
  pca <- prcomp(data, scale. = T)
  print(pca)
  plot(pca)
  summary(pca)
}




make_data <- function(){
  
  label <- list()
  
  for(i in 3:8){
    
    pop <- replicate(50, generate_correlated(i, 10000))
    append(label, i)
  }
  
  
  saveRDS(list(pop, label), "data_pop.rds")
  
}

#stat <- generate_simple_dirichlet(6, 10000)

#stat <- generate_correlated(6, 10000)
#print(stat)

#PCA_summary(stat)

make_data()

#PCA_summary(generate(4, 10000, 100))