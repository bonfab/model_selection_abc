source("clust.R")
source("structure.R")
source("tracy_widom.R")
source("generate_data2.R")
source("gradient_boosting.R")
library(elasticnet)
library(sparsepca)

compare <- function(K = 2:2, size = 1){

    #load xgb model
    xgb_model <- load_model()

    for(k in K){

        print(paste("current k:", k, sep = " "))

        for(i in 1:size){

            data <- generate_prob(k)
            data <- bernoulli_matrix(data)

            scaled <- scale(data, center = T, scale = F)

            data <- data[,sparsePCA_indices(scaled)]

            print(dim(scaled))
            print(dim(data))

            #call structure
            get_structure_result(data)




            #plot(prcomp(data))


        }

    }

}

xgb_result <- function(data, model){

    #summary <- PCA_summary(data)
    #print(paste("XGB:", classifyXGBoostSingle(xgb_model, summary) + 2, sep = " "))

}

sparsePCA_indices <- function(data){

    #arrayspc(data, K=25, rep(100, 25), trace = T)
    result <- rspca(data, k = 20)

    loadings <- result$loadings
    index = matrix()
    for(i in 1:ncol(loadings)){
        if(i == 1){
            index <- t(which(loadings[,i] > 0))
        } else {
            index <- cbind(index, t(which(loadings[,i] > 0)))
        }
    }

    index <- unique(index[1,])
}

Rcpp::sourceCpp("sample_bernoulli_matrix_no_central.cpp")
compare()