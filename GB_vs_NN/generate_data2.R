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

admixture_layer <- function(K, pop_size, number_admixed, scaling = NULL){

    if(is.null(scaling)){
        sizes <- rep(pop_size, K)
    } else {
        sizes <- sapply(scaling, function(x) ceiling(x*pop_size))
    }


    Q <- matrix(0, nrow = sum(sizes), ncol = K)

    for(pop in 1:K){
        if(pop == 1){
            Q[1:sum(sizes[1:pop]), pop] <- 1
        } else {
            Q[(sum(sizes[1:(pop-1)]) +1):sum(sizes[1:pop]),] <- 1
        }

    }

    print(sizes)

    for(admixed in (K+1):(K+number_admixed)){

        admixed_prior <- rdirichlet(1, rep(1, K))
        print((sum(sizes[1:(admixed-1)])+1):sum(sizes[1:admixed]))

        Q[(sum(sizes[1:(admixed-1)])+1):sum(sizes[1:admixed]),] <- t(replicate(pop_size, rdirichlet(1, admixed_prior), simplify = "matrix"))

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

    return(pca$sdev[1:reduce_to]/eigen_sum)

}


Sys.setlocale("LC_MESSAGES", "en_US.utf8")
K <- 5
F <- F_layer(K, runif(K, 0, 1), number_locus = 10000)
Q <- admixture_layer(nrow(F), 50, 1)


prob <- Q %*% F

d <- matrix_binom(prob)


PCA_summary(d)