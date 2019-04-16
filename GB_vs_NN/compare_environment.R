library(elasticnet)
library(sparsepca)
library(LEA)

source("clust.R")
source("structure.R")
source("tracy_widom.R")
source("generate_data2.R")
source("gradient_boosting.R")
source("snmf.R")


compare <- function(K = 4:4, size = 1){

    #load xgb model
    xgb_model <- load_model()
    
    final <- matrix()
    reduction_degree <- list()

    for(k in K){

        print(paste("current k:", k, sep = " "))

        for(i in 1:size){

            data <- generate_prob(k)
            data <- bernoulli_matrix(data)

            scaled <- scale(data, center = T, scale = F)

            #data_reduced <- data[,sparsePCA_indices(scaled)]

            print(dim(scaled))
            #print(dim(data_reduced))

            #call structure
            #structure_estimate <- get_structure_result(data_reduced)
            
            #cluster_estimate <- get_clust_estimate(scaled)
            
            xgb_estimate <- classifyXGBoostSingle(xgb_model, PCA_summary(scaled))+2
            print(xgb_estimate)
            
            tracy_widom_estimate <- which.max(likelihood_derivative(likelihood_derivative(get_tw_estimate(data, all = T)))) + 2
            print(tracy_widom_estimate)
            
            snmf_estimate <- get_snmf_estimate(data, max_K = 6)
            #print(snmf_estimate)
            
            print(xgb_estimate)
            print(tracy_widom_estimate)
            print(snmf_estimate)
            #final <- rbind(final, c(k, xgb_estimate, structure_estimate, tracy_widom_estimate, cluster_estimate))
            #reduction_degree <- append(reduction_degree, dim(data_reduced)[2]/dim(data)[2])
        }

    }
    
    saveRDS(list(final, reduction_degree), "data_K/comparison_1.rds")

}

xgb_result <- function(data, model){

    summary <- PCA_summary(data)
    return(classifyXGBoostSingle(xgb_model, summary) + 2)
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