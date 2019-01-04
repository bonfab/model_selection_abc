library(parallel)
library(MCMCpack)
library(LaplacesDemon)
library(elasticnet)


generate_simple <- function(K, number_locus, pop_size = 100, alpha = .1, beta = 1){
  
  #set.seed(1)
  pop <- replicate(number_locus, replicate(K, rbinom(n = pop_size, size = 1, prob = rbeta(1, alpha, beta))), simplify = T)
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
    
    vapply(1:K, function(p) replicate(pop_size, rbinom(1, 2, sum(prob_locus * rdirichlet(1, set_alphas(p))[1,]))), integer(pop_size))
    #replicate(pop_size, rbinom(1, 2, sum(prob_locus * rdirichlet(1, rep(1, K)[1,]))))
    
  }
  
  sim <- matrix(NA_integer_, pop_size*K, number_locus)
  
  for(col in 1:number_locus){
    sim[,col] <- admixture()
  }
  
  #pop <- replicate(number_locus, admixture(), simplify = T)
  #print(pop)
  return(pop)
  
  #return(sim)
}

generate_admixture_prior_fast <- function(K, number_locus, pop_size = 50, alpha = 1, beta = 1, membership = .25){
  
  set_alphas <- function(pop){
    alphas <- rep(1, K)
    alphas[pop] <- 1 + rbeta(1, 1 + membership, 1)
    return(alphas)
  }
  
  #set.seed(1)
  admixture <- function(pop){
    
    prior <- set_alphas(pop)
    
    sim_pop <- t(vapply(1:pop_size, function(x) apply(matrix(rbeta(number_locus*K, alpha, beta), nrow = number_locus) * rdirichlet(number_locus, prior), 1, function(x) rbinom(1, 2, sum(x))), integer(number_locus)))
    
    
    #dirichs <- array(t(rdirichlet(number_locus * pop_size, prior)), c(K, number_locus, pop_size))
    #betas <- array(rbeta(number_locus*K, alpha, beta), c(K, number_locus, pop_size))
    #sim_pop <- t(apply(betas * dirichs, c(2, 3), function(x) rbinom(1, 2, sum(x))))
    
    
    return(sim_pop)
  }
  
  sim <- matrix(NA_integer_, pop_size*K, number_locus)
  
  for(pop in 1:K){
    #print((pop_size*(pop-1) + 1):(pop_size*pop))
    sim[(pop_size*(pop-1) + 1):(pop_size*pop),] <- admixture(pop)
  }
  
  #pop <- replicate(number_locus, admixture(), simplify = T)
  #print(pop)
  #(pop)
  
  return(sim)
}

generate_Fmodel <- function(K, number_locus, number_alleles = 2, pop_size = 50, alpha = 1, beta = 1, membership = .25){
  
  parent_prob <- rbeta(number_locus, 1, 1)
  
  sim <- matrix(NA_integer_, pop_size*K, number_locus)
  
  for(pop in 1:K){
    F <- rbeta(1, 1, 1)
    p <- vapply(1:pop_size, function(x) vapply(parent_prob, function(y) rbinom(1, 2, rbeta(1, (1-y)*(1-F)/F, y*(1-F)/F)), integer(1)), integer(number_locus))
    sim[(pop_size*(pop-1) + 1):(pop_size*pop),] <- p
  }
  
  print(sim)
  return(sim)
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
  
  plot(pca$x)
  eigen_sum <- sum(pca$sdev)
  
  #print(eigen_sum)
  #print(pca$sdev[1:reduce_to])
  
  return(pca$sdev[1:reduce_to]/eigen_sum)
  
}

sparse_pca <- function(microarry, K){
  
  selected <- spca(microarry, K, rep(100, K))
  print(selected)
  
}


make_data <- function(samples = 200, populations = 3:13){
  
  clust <- makeCluster(detectCores())
  clusterExport(cl=clust, varlist=c("PCA_summary", "generate_admixture_prior_fast", "rdirichlet"))
  
  #pop <- do.call(rbind, lapply(populations, function(x) t(replicate(samples, PCA_summary(generate_admixture_prior(x, 10000))))))
  #pop <- do.call(rbind, parLapply(clust, populations, function(x) t(replicate(samples, PCA_summary(generate_admixture_prior(x, 10000))))))
  
  label <- unlist(lapply(populations, function(x) rep(x, samples)))
  
  print("begin generation")
  
  priors <- seq(0.1, 0.6, by=0.01)
  pop <- do.call(rbind, parLapply(clust, label, function(x) t(sapply(priors, function(y) PCA_summary(generate_admixture_prior_fast(x, 10000, membership = y))))))
  label <- as.vector(t(replicate(length(priors), label)))
  
  #print(pop)
  #print(label)
  
  #saveRDS(list(pop, label), "data_pop_prio_1-25.rds")
  saveRDS(list(pop, label), "data_pop_random_prio_.rds")
  
  
}

#source("reduce_kernels.R")
#stat <- generate_simple_dirichlet(5, 10000)
#summary <- PCA_summary(stat)
#red <- make_similarity_matrix(stat)
#PCA_summary(red)
#print(red)

#stat <- generate_correlated(6, 10000)
#print(stat)

#system.time(stat <- generate_admixture_prior_fast(6, 5000))

#print(stat)

#s <- PCA_summary(stat)

#print(s)
#s2 <- sparse_pca(stat, 10)

make_data()

#bla <- readRDS("data_pop.rds")
#print(do.call(rbind, bla[[1]]))

