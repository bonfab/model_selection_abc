# Title     : TODO
# Objective : TODO
# Created by: bergmanf
# Created on: 28/03/19

library(mclust)
library(RSpectra)

load_data <- function(RDS_file = "data_K/full_test_admixed1_data_pop_2-16.rds"){

  data <- readRDS(RDS_file)

}

reduce_dim <- function(data, reduce_to = 100){
  eigval <- svds(data, k = reduce_to, nu = reduce_to, nv = 0)
  U <- eigval$u

  t(U) %*% data
  
}


test <- function(data, true_k){

    print(dim(data))
    data <- reduce_dim(data)
    print(dim(data))
    #plot(data)
    print("true_k:")
    print(true_k)
    print("estimate:")
    print(Mclust(data, 1:20, c("EII"))$G)

}

run <- function(){
    d <- load_data()

    #test(d[[1]][[1]], d[[2]][[1]])

    for(i in 1:length(d[[1]])){
        test(d[[1]][[i]], d[[2]][[i]])
    }

}