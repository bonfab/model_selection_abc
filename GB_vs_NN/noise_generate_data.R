# Created by: bergmanf
# Created on: 22/01/19

library(parallel)
#library(MCMCpack)
library(LaplacesDemon)
library(Rcpp)
library(RSpectra)


F_layer <- function(K, F_values, number_locus = sample(40000 - 8000, 1) + 8000){

    ancestral_pop <- runif(number_locus)

    F <- t(vapply(F_values, function(F_value) vapply(ancestral_pop, function(x) rbeta(1, x*(1-F_value)/F_value, (1-x)*(1-F_value)/F_value), numeric(1)), numeric(number_locus)))

    #F <- F[, apply(F, 2, function(x) !(length(unique(x)) == 1))]

    return(F)
}

admixture_layer <- function(K, number_admixed, sizes){


    #check_dir <- function(alphas){
    #    repeat{
    #        values <- rdirichlet(1, alphas)
    #        if(sum(values) <= 1){
    #            break
    #        } else {
    #            print("wtf")
    #        }
    #    }
    #    return(values)
    #}

    Q <- matrix(0, nrow = sum(sizes), ncol = K)

    pointer <- 0

    for(pop in 1:K){

        Q[(pointer+1):(pointer+sizes[pop]), pop] <- 1

        pointer <- pointer + sizes[pop]
    }

    #print(sizes)

    if(number_admixed == 0){
        return(Q)
    }

    for(admixed in (K+1):(K+number_admixed)){

     #   admixed_prior <- rdirichlet(1, rep(1, K))
      #  admixed_prior <- rep(8, K)
        
       # if(admixed == K+1){
      #      admixed_prior <- rep(10, K)
       #     admixed_prior[1] <- 0
        #    admixed_prior[3] <- 30
            #admixed_prior[K-1] <- 120
        #}
        #if(admixed == K+2){
         #   admixed_prior <- rep(40, K)
          #  admixed_prior[K-1] <- 0
           # admixed_prior[K] <- 120
        #}
        #if(admixed == K+3){
         #   admixed_prior <- rep(5, K)
          #  admixed_prior[K-2] <- 1
        #}


      
        mixture <- sample(1:K, sample(K-2, 1) + 2)
        
        admixed_prior <- rep(0, K)
        
        for(i in mixture){
          admixed_prior[i] <- rbeta(1, 1, 1) * sample(8, 1)
        }

        Q[(pointer+1):(pointer + sizes[admixed]),] <- t(replicate(sizes[admixed], rdirichlet(1, admixed_prior), simplify = "matrix"))

        pointer <- pointer + sizes[admixed]
    }

    return(Q)
}



PCA_summary <- function(data, reduce_to = 25){

    print(dim(data))

    #data <- data[, apply(data, 2, function(x) !(length(unique(x)) == 1))]
    #data <- scale(data)
    #data <- data[, apply(data, 2, function(x) !(sum(x) == 0))
    #data <- data[,scale]
    #pca <- prcomp(data, scale = F)

    eigval <- svds(data, k = reduce_to + 1, nu = 0, nv = 0)
    #trunc <- pca$rotation[,1:reduce_to] %*% pca$x[1:reduce_to,1:reduce_to]
    #plot(pca)
    #plot(pca$x)
    eigval <- eigval$d^2 / (nrow(data)- 1)

    #plot(prcomp(data))
    #eigen_sum <- sum(eigval)
    eigval <- (eigval - min(eigval)) / (max(eigval) - min(eigval))
    #barplot(eigval)
    return(eigval[-(reduce_to+1)])
}

#generate_prob <- function(K, number_locus = sample(40000 - 2000, 1) + 2000, number_admixed = floor(rbeta(1, 1, 1.2) * K), pop_sizes = rdirichlet(1, rep(1, K + number_admixed)), sample_size = sample(10000 - 500, 1) + 500){
generate_prob <- function(K, number_locus = 4000, number_admixed = 0, pop_sizes = rdirichlet(1, rep(1, K + number_admixed)), sample_size = 120){

    #F_values <- runif(K, 0, 1)
    F_values <- rbeta(K, 1, 3)
    while(length(which(F_values < 0.01)) > 0){
        F_values <- rbeta(K, 1, 3)
    }

    #print(paste("F_values:", as.character(F_values), sep = " "))
    F <- F_layer(K, F_values, number_locus)


    scaling <- 1


    if(is.null(pop_sizes)){
        p <- 1/(K + scaling*number_admixed)
        a <- (1 - (K*p))/number_admixed
        pop_sizes <- rep(p, K)
        pop_sizes <- append(pop_sizes, rep(a, number_admixed))
        pop_sizes <- ceiling(pop_sizes * sample_size)
    } else {
      pop_sizes <- ceiling(pop_sizes * sample_size)
    }

    #print(paste("admixed:", as.character(number_admixed), sep = " "))
    #print(paste("pop sizes", as.character(pop_sizes), sep = " "))
    Q <- admixture_layer(nrow(F), number_admixed, sizes = pop_sizes)


    prob <- Q %*% F

    return(prob)
}

generate <- function(K, number_locus = sample(40000 - 2000, 1) + 2000, number_admixed = floor(rbeta(1, 1, 1.2) * K), pop_sizes = rdirichlet(1, rep(1, K + number_admixed)), sample_size = sample(5000 - 100, 1) + 100){

    while(length(which((pop_sizes[K] * sample_size) < sample_size*0.5 /(K+number_admixed))) > 0){
    pop_sizes <- rdirichlet(1, rep(1, K + number_admixed))
    }

    #F_values <- runif(K, 0, 1)
    F_values <- rbeta(K, 1, 3)
    print(paste("F_values:", as.character(F_values), sep = " "))


    F_values <- rbeta(K, 1, 3)
    while(length(which(F_values < 0.01)) > 0){
        F_values <- rbeta(K, 1, 3)
    }

    F <- F_layer(K, F_values, number_locus)

    scaling <- 1

    
    if(is.null(pop_sizes)){
        p <- 1/(K + scaling*number_admixed)
        a <- (1 - (K*p))/number_admixed
        pop_sizes <- rep(p, K)
        pop_sizes <- append(pop_sizes, rep(a, number_admixed))
        pop_sizes <- ceiling(pop_sizes * sample_size)
    } else {
      pop_sizes <- ceiling(pop_sizes * sample_size)
    }

    print(paste("admixed:", as.character(number_admixed), sep = " "))
    print(paste("pop sizes", as.character(pop_sizes), sep = " "))
    Q <- admixture_layer(nrow(F), number_admixed, sizes = pop_sizes)


    prob <- Q %*% F

    noise <- matrix(rnorm(length(prob), 0, 0.005), nrow = nrow(prob))
    
    prob <- prob + noise
    
    #data <- matrix(0, nrow = nrow(prob), ncol = ncol(prob))
    #for(i in 1:ncol(prob)){
    #    for(j in 1:nrow(prob)){
    #        data[j, i] <- rbinom(1, 2, prob[j, i])
    #    }
    #}

    #data <- data[, apply(data, 2, function(x) !(length(unique(x)) == 1))]
    #data <- scale(data)
    #print("scale")
    #print(dim(data))
    prob <- bernoulli_matrix(prob)
    #print("c")
    #print(dim(prob))
    #print(prob[,1])
    return(prob)
}

make_data <- function(samples = 1000, populations = 2:16){

  #clust <- makeCluster(detectCores() - 2)
  #clusterExport(cl=clust, varlist=c("PCA_summary", "generate", "rdirichlet", "F_layer", "admixture_layer", "bernoulli_matrix"))

   # clusterEvalQ(clust, source("generate_data2.R"))
  #pop <- do.call(rbind, lapply(populations, function(x) t(replicate(samples, PCA_summary(generate_admixture_prior(x, 10000))))))
  #pop <- do.call(rbind, parLapply(clust, populations, function(x) t(replicate(samples, PCA_summary(generate_admixture_prior(x, 10000))))))
  labels <- lapply(populations, function(x) rep(x, samples))

  print("begin generation")
  #priors <- seq(0.1, 0.6, by=0.1)
  result <- list()
    for(p in labels){

        print(paste("populations:", as.character(p[1]), sep = " "))
        #pop <- t(parSapply(clust , p, function(y) PCA_summary(generate(y, number_locus = sample(40000- 2000, 1)+2000))))
        pop <- t(sapply(p, function(y) PCA_summary(generate(y, number_locus = sample(40000- 2000, 1)+2000))))
        result <- rbind(result, pop)
        #pop <- lapply(p, function(y) generate(y, number_locus = sample(40000- 2000, 1)+2000))
        #result <- append(result, pop)

    }
    #stopCluster(clust)
    #print(result)
    #pop <- do.call(rbind, result)
    labels <- unlist(labels)
  #label <- as.vector(t(replicate(samples, populations)))


  #saveRDS(list(pop, label), "data_pop_prio_1-25.rds")
    #print(pop)
    #print(list(result, labels))
  saveRDS(list(result, labels), "./data_K/nr3_data_pop_2-16.rds")

}

Sys.setlocale("LC_MESSAGES", "en_US.utf8")
Rcpp::sourceCpp("sample_bernoulli_matrix.cpp")
#make_data()


#a <- generate(3, number_admixed = 1, pop_sizes = c(0.2, 0.2, 0.4, 0.2))
#print(dim(a))
#p <- prcomp(a)

#plot(p)

#PCA_summary(generate(10))
