# Created by: bergmanf
# Created on: 22/01/19

library(parallel)
library(MCMCpack)
library(LaplacesDemon)
library(elasticnet)


F_layer <- function(K, F_values, number_locus = 10000, scaling = NULL){

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

admixture_layer <- function(K, pop_size, number_admixed){

    Q <- matrix(0, nrow = pop_size* (K + number_admixed), ncol = K)

    for(pop in 1:K){
        Q[(pop_size*(pop-1) + 1):(pop_size*pop), pop] <- 1
    }

    for(admixed in (K+1):(K+number_admixed)){

        admixed_prior <- rdirichlet(1, rep(1, K))

        Q[(pop_size*(admixed-1) + 1):(pop_size*admixed),] <- t(replicate(pop_size, rdirichlet(1, admixed_prior), simplify = "matrix"))

    }

    return(Q)

}

matrix_binom <- function(matrix, ploidy = 1){

    for(i in 1:nrow(matrix)){
        for(j in 1:ncol(matrix)){
            matrix[i,j] <- rbinom(1, ploidy, matrix[i, j])
        }
    }

    return(matrix)
}



PCA_summary <- function(data, reduce_to = 25){

    data <- data[, apply(data, 2, function(x) !(length(unique(x)) == 1))]
    for(i in 1:ncol(data)){

        if(length(unique(data[,i])) == 1){
            print("impossible !!!!!!!!!!!!!!!!")
        }

    }

    pca <- prcomp(data, scale = T)

    #trunc <- pca$rotation[,1:reduce_to] %*% pca$x[1:reduce_to,1:reduce_to]

    plot(pca)
    eigen_sum <- sum(pca$sdev)

    return(pca$sdev[1:reduce_to]/eigen_sum)

}


Sys.setlocale("LC_MESSAGES", "en_US.utf8")
F <- F_layer(3, runif(3, 0, 1), number_locus = 10000)
Q <- admixture_layer(nrow(F), 50, 1)


print(F)
print("")
print(Q)
print("")

prob <- Q %*% F
print(prob)
print("")
d <- matrix_binom(prob)


print(d)

PCA_summary(d)