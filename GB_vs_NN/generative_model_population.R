library(parallel)
library(MCMCpack)
library(LaplacesDemon)
library(elasticnet)

generate_simple <- function(K, number_locus, pop_size = 100, alpha = .1, beta = 1){
  
  #set.seed(1)
  pop <- replicate(number_locus, replicate(K, rbinom(n = pop_size, size = 1, prob = rbeta(1, alpha, beta))), simplify = T)
  #print(pop)
  return(pop)
}

generate_correlated <- function(K, number_locus, pop_size = 25, number_alleles = 8, alphas = NULL){
  
  
  correlate_locus <- function(){
    corr <- rgamma(1, 7.5)
    return(replicate(K, rcat(n = pop_size, p = rdirichlet(1, rep(corr, times = number_alleles))[1,])))
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



PCA_summary <- function(data, reduce_to = 20){
  
  #print(data)
  pca <- prcomp(data, scale = T)
  
  #print(pca)
  
  #print(dim(pca$x))
  #print(dim(pca$rotation))
  
  #print(dim(pca$rotation[,1:reduce_to]))
  #print(dim(pca$x[1:reduce_to,1:reduce_to]))
  
  #trunc <- pca$rotation[,1:reduce_to] %*% pca$x[1:reduce_to,1:reduce_to]
  
  #print(pca)
  plot(pca)
  #summary(pca)
  
  return(trunc)
  
}

sparse_pca <- function(microarry, K){
  
  selected <- spca(microarry, K, rep(100, K))
  print(selected)
  
}


make_data <- function(samples = 50, populations = 3:8){
  
  #clust <- makeCluster(detectCores())
  #clusterExport(cl=clust, varlist=c("PCA_summary", "generate_correlated", "rcat", "rdirichlet"))
  
  pop <- lapply(populations, function(x) replicate(samples, PCA_summary(generate_correlated(x, 10000))))
  #pop <- parLapply(clust, populations, function(x) replicate(samples, PCA_summary(generate_correlated(x, 10000))))
  
  label <- lapply(populations, function(x) rep(x, samples))
  
  print(pop)
  print(label)
  
  saveRDS(list(pop, label), "data_pop.rds")
  
  
}

#stat <- generate_simple_dirichlet(6, 10000)

stat <- generate_correlated(6, 10000)
#print(stat)

summary <- PCA_summary(stat)

#print(summary)
#s2 <- sparse_pca(stat, 10)

#make_data()
