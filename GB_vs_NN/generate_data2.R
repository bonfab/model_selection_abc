# Created by: bergmanf
# Created on: 22/01/19

library(parallel)
library(MCMCpack)
library(LaplacesDemon)
library(elasticnet)


F_layer <- function(K, F_values, number_locus = 10000){

    ancestral_pop <- runif(number_locus)

    #get_sub_pop <- function(F_value){


    #    p <- vapply(ancestral_pop, function(x) rbeta(pop_size, x*(1-F_value)/F_value, (1-x)*(1-F_value)/F_value), numeric(pop_size))

    #    return(p)
    #}

    #sub_pops <- matrix(nrow = pop_size*K, ncol = number_locus)

    #for(pop in 1:K){
    #    sub_pops[(pop_size*(pop-1) + 1):(pop_size*pop),] <- get_sub_pop(F_values[pop])
    #}

    F <- t(vapply(F_values, function(F_value) vapply(ancestral_pop, function(x) rbeta(1, x*(1-F_value)/F_value, (1-x)*(1-F_value)/F_value), numeric(1)), numeric(number_locus)))

    return(F)
}

admixture_layer <- function(K, number_admixed, sizes){


    Q <- matrix(0, nrow = sum(sizes), ncol = K)

    pointer <- 0

    for(pop in 1:K){

        Q[(pointer+1):(pointer+sizes[pop])] <- 1

        pointer <- pointer + sizes[pop]

    }

    #print(sizes)

    for(admixed in (K+1):(K+number_admixed)){

        admixed_prior <- rdirichlet(1, rep(1, K))

        Q[(pointer+1):(pointer + sizes[admixed]),] <- t(replicate(sizes[admixed], rdirichlet(1, admixed_prior), simplify = "matrix"))
        pointer <- pointer + sizes[admixed]
    }

    return(Q)

}

matrix_binom <- function(matrix, ploidy = 1){

    for(i in 1:nrow(matrix)){
        for(j in 1:ncol(matrix)){
            x <- rbinom(1, ploidy, matrix[i, j])
            if(is.na(x)){
                #print(matrix[i,j])
                #print(matrix[i,j] > 1)
                #print(matrix[i, j] - 1)
                #print(rbinom(1,1, matrix[i,j]))
                #print("")
                x <- 1
            }

            matrix[i,j] <- x
        }
    }

    return(matrix)
}



PCA_summary <- function(data, reduce_to = 25){

    data <- data[, apply(data, 2, function(x) !(length(unique(x)) == 1))]

    pca <- prcomp(data, scale = T)


    #trunc <- pca$rotation[,1:reduce_to] %*% pca$x[1:reduce_to,1:reduce_to]

    plot(pca)
    eigen_sum <- sum(pca$sdev)

    return(append(append(pca$sdev[1:reduce_to], eigen_sum), dim(data)))

}

generate <- function(K, number_locus = 10000, number_admixed = 1, pop_sizes = NULL, sample_size = 1000){

    F_values <- runif(K, 0, 1)
    F <- F_layer(K, F_values, number_locus)

    scaling <- 1.5

    if(is.null(pop_sizes)){
        p <- 1/(K + scaling*number_admixed)
        a <- (1 - (K*p))/number_admixed
        pop_sizes <- rep(p, K)
        pop_sizes <- append(pop_sizes, rep(a, number_admixed))
        pop_sizes <- pop_sizes * sample_size
    }


    Q <- admixture_layer(nrow(F), number_admixed, sizes = pop_sizes)

    prob <- Q %*% F

    return(matrix_binom(prob))

}

make_data <- function(samples = 1, populations = 3:13){

  clust <- makeCluster(detectCores())
  clusterExport(cl=clust, varlist=c("PCA_summary", "generate", "rdirichlet", "F_layer", "admixture_layer", "matrix_binom"))

  #pop <- do.call(rbind, lapply(populations, function(x) t(replicate(samples, PCA_summary(generate_admixture_prior(x, 10000))))))
  #pop <- do.call(rbind, parLapply(clust, populations, function(x) t(replicate(samples, PCA_summary(generate_admixture_prior(x, 10000))))))

  #label <- unlist(lapply(populations, function(x) rep(x, samples)))

  print("begin generation")

  #priors <- seq(0.1, 0.6, by=0.1)
  pop <- do.call(rbind, parLapply(clust, populations, function(x) t(sapply(1:samples, function(y) PCA_summary(generate(x))))))
  label <- as.vector(t(replicate(samples, populations)))

  #print(pop)
  #print(label)

  #saveRDS(list(pop, label), "data_pop_prio_1-25.rds")
  saveRDS(list(pop, label), "data/admixed_one_1_5.rds")

}


make_data()

#print(PCA_summary(generate(3)))