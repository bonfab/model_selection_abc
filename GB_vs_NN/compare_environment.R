#library(elasticnet)
#library(sparsepca)

library(LEA)
#source("clust.R")
source("structure.R")
source("tracy_widom.R")
source("generate_data2.R")
source("gradient_boosting.R")
source("snmf.R")



compare <- function(K = 3:16, size = 2){

    #load xgb model
    xgb_model <- load_model()
    
    final <- matrix(0, length(K)*size, 4)
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
            
            tracy_widom_estimate <- which.max(likelihood_derivative(likelihood_derivative(get_tw_estimate(data, all = T)))) + 2
            #print(tracy_widom_estimate)
            
            
            xgb_estimate <- classifyXGBoostSingle(xgb_model, PCA_summary(scaled))+2
            #print(xgb_estimate)
            
            snmf_estimate <- get_snmf_estimate(data, max_K = 17)
            #print(snmf_estimate)
            
            print(xgb_estimate)
            print(tracy_widom_estimate)
            print(snmf_estimate)
            final[size*(k-min(K))+i,] <-  c(k, xgb_estimate, snmf_estimate, tracy_widom_estimate)
            #reduction_degree <- append(reduction_degree, dim(data_reduced)[2]/dim(data)[2])
        }
    }
    
    #saveRDS(list(final, reduction_degree), "data_K/comparison_1.rds")
    saveRDS(final, "data_K/new_comparison.rds")

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

#Rcpp::sourceCpp("sample_bernoulli_matrix_no_central.cpp")
#compare()

library(rlist)

compare_saved <- function(source = "./data_K/save/test"){
  
  
  
  xgb_model_simple <- load_model()
  xgb_model_13 <- load_model("./plain.model")
  
  for(k in 5:8){
    print(k)
    snmf_list <- list()
    tw_list <- list()
    xgb_simple_list <- list()
    xgb_13_list <- list()
    for(i in 1:10){
      snp_data <- readRDS(paste(source, as.character(k),  as.character(i), sep = "_"))
      
      snmf_estimate <- get_snmf_estimate(snp_data)
      print("snmf")
      #snmf_estimate <- 1
      print(snmf_estimate)
      snmf_list <- list.append(snmf_list, snmf_estimate)
      
      tw_estimate <- get_tw_estimate(snp_data, all = F) +1
      #tw_estimate <- 1
      print("simple tw")
      print(tw_estimate)
      tw_list <- list.append(tw_list, tw_estimate)
      #tracy_widom_estimate <- which.max(likelihood_derivative(likelihood_derivative(get_tw_estimate(snp_data, all = T)))) + 2
      #print("heuristic tw")
      #print(tracy_widom_estimate)
      
      snp_data <- scale(snp_data, center = T, scale = F)
      xgb_estimate_simple <- classifyXGBoostSingle(xgb_model_simple, PCA_summary(snp_data))+2
      print("xgb simple")
      print(xgb_estimate_simple)
      xgb_simple_list <- list.append(xgb_simple_list, xgb_estimate_simple)
      
      xgb_estimate_13 <- classifyXGBoostSingle(xgb_model_13, PCA_summary(snp_data))+2
      print("xgb 13")
      print(xgb_estimate_13)
      xgb_13_list <- list.append(xgb_13_list, xgb_estimate_13)
    }
    print(snmf_list)
    print(tw_list)
    print(xgb_simple_list)
    print(xgb_13_list)
    saveRDS(list(snmf_list, tw_list, xgb_simple_list, xgb_13_list), paste("./data_K/save/results", as.character(k), sep = "_"))
  }
  
}

compare_saved()



