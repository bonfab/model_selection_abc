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

generate_admixture <- function(K, number_locus, pop_size = 100, alpha = 1, beta = 1, concentration = 1.5){
  
  set_alphas <- function(pop){
    alphas <- rep(1, K)
    alphas[pop] <- concentration
    return(alphas)
  }
  
  #set.seed(1)
  admixture <- function(){
    prob_locus <- rbeta(K, alpha, beta)
    
    sapply(1:K, function(p) replicate(pop_size, rbinom(1, 2, sum(prob_locus * rdirichlet(1, set_alphas(p))[1,]))))
    #replicate(pop_size, rbinom(1, 2, sum(prob_locus * rdirichlet(1, rep(1, K)[1,]))))
    
  }
  
  pop <- replicate(number_locus, admixture(), simplify = T)
  #print(pop)
  return(pop)
}

generate_admixture_prior <- function(K, number_locus, pop_size = 50, alpha = 1, beta = 1){
  
  set_alphas <- function(pop){
    alphas <- rep(1, K)
    alphas[pop] <- 1 + rbeta(1, 1.25, 1)
    return(alphas)
  }
  
  #set.seed(1)
  admixture <- function(){
    prob_locus <- rbeta(K, alpha, beta)
    
    sapply(1:K, function(p) replicate(pop_size, rbinom(1, 2, sum(prob_locus * rdirichlet(1, set_alphas(p))[1,]))))
    #replicate(pop_size, rbinom(1, 2, sum(prob_locus * rdirichlet(1, rep(1, K)[1,]))))
    
  }
  
  pop <- replicate(number_locus, admixture(), simplify = T)
  #print(pop)
  return(pop)
}


generate_correlated <- function(K, number_locus, pop_size = 200, number_alleles = 8, alphas = NULL){
  
  
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



PCA_summary <- function(data, reduce_to = 25){
  
  data <- data[, apply(data, 2, function(x) !(length(unique(x)) == 1))]
  
  pca <- prcomp(data, scale = T)
  
  #trunc <- pca$rotation[,1:reduce_to] %*% pca$x[1:reduce_to,1:reduce_to]
  
  #plot(pca)
  
  #print(pca$x[1:reduce_to, 1:reduce_to])
  #print(pca$sdev[1:reduce_to])
  #print(pca$sdev[1:reduce_to])
  eigen_sum <- sum(pca$sdev)
  
  #print(eigen_sum)
  #print(pca$sdev[1:reduce_to])
  
  return(pca$sdev[1:reduce_to]/eigen_sum)
  
}

sparse_pca <- function(microarry, K){
  
  selected <- spca(microarry, K, rep(100, K))
  print(selected)
  
}


make_data <- function(samples = 500, populations = 3:8){
  
  clust <- makeCluster(detectCores())
  clusterExport(cl=clust, varlist=c("PCA_summary", "generate_admixture_prior", "rdirichlet"))
  
  #pop <- do.call(rbind, lapply(populations, function(x) t(replicate(samples, PCA_summary(generate_admixture_prior(x, 10000))))))
  #pop <- do.call(rbind, parLapply(clust, populations, function(x) t(replicate(samples, PCA_summary(generate_admixture_prior(x, 10000))))))
  
  label <- unlist(lapply(populations, function(x) rep(x, samples)))
  
  print("begin generation")
  pop <- do.call(rbind, parLapply(clust, label, function(x) PCA_summary(generate_admixture_prior(x, 10000))))
  
  
  #print(pop)
  #print(label)
  
  saveRDS(list(pop, label), "data_pop_prio_1-25.rds")
  #saveRDS(list(pop, label), "data_pop.rds")
  
  
}

#source("reduce_kernels.R")
#stat <- generate_simple_dirichlet(5, 10000)
#summary <- PCA_summary(stat)
#red <- make_similarity_matrix(stat)
#PCA_summary(red)
#print(red)

#stat <- generate_correlated(6, 10000)
#print(stat)

#stat <- generate_admixture_prior(5, 5000)

#s <- PCA_summary(stat)

#print(s)
#s2 <- sparse_pca(stat, 10)

make_data()

#bla <- readRDS("data_pop.rds")
#print(do.call(rbind, bla[[1]]))

