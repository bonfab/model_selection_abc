# Title     : TODO
# Objective : TODO
# Created by: bergmanf
# Created on: 22/01/19

library(parallel)
library(MCMCpack)
library(LaplacesDemon)
library(elasticnet)


F_model_layer <- function(K, F_values, number_locus = 10000, pop_size = 50, scaling = NULL){

    ancestral_pop <- runif(number_locus)


    get_sub_pop <- function(F_value, alpha = 1, beta = 1){
        #alpha <- runif(1, 0, 2)
        #beta <- runif(1, 0, 2)
        #F_value <- rbeta(1, alpha, beta)

        p <- vapply(ancestral_pop, function(x) rbeta(pop_size, x*(1-F_value)/F_value, (1-x)*(1-F_value)/F_value), numeric(pop_size))

        return(p)
    }

    sub_pops <- matrix(nrow = pop_size*K, ncol = number_locus)

    for(pop in 1:K){
        sub_pops[(pop_size*(pop-1) + 1):(pop_size*pop),] <- get_sub_pop(F_values[pop])
    }

    return(sub_pops)
}


PCA_summary <- function(data, reduce_to = 25){

  data <- data[, apply(data, 2, function(x) !(length(unique(x)) == 1))]

  pca <- prcomp(data, scale = T)

  #trunc <- pca$rotation[,1:reduce_to] %*% pca$x[1:reduce_to,1:reduce_to]

  plot(pca$x)
  eigen_sum <- sum(pca$sdev)


  return(pca$sdev[1:reduce_to]/eigen_sum)

}


Sys.setlocale("LC_MESSAGES", "en_US.utf8")
data <- F_model_layer(3, c(0.1, 0.5, 0.9), number_locus = 1000, pop_size = 1)
print(data)

PCA_summary(data)