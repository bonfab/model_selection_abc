library(rlist)
source("F_noise_generate_data.R")

compare_saved_lower <- function(source = "./data_K/save/test"){
  
  xgb_model_lower <- load_model("./lower.model")
  
  for(k in 2:8){
    print(k)
    xgb_lower_list <- list()
    for(i in 1:10){
      snp_data <- readRDS(paste(source, as.character(k),  as.character(i), sep = "_"))
      
      xgb_estimate_lower <- classifyXGBoostSingle(xgb_model_lower, PCA_summary(snp_data))+2
      print("lower")
      print(xgb_estimate_lower)
      xgb_lower_list <- list.append(xgb_lower_list, xgb_estimate_lower)
    }
    print(xgb_lower_list)
    saveRDS(xgb_lower_list, paste("./data_K/save/results_lower", as.character(k), sep = "_"))
  }
}

compare_saved_lower()