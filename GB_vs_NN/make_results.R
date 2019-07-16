
mse <- function(values, k){
  return((sum(unlist(values, use.names = F) - k)^2)/length(values))
}

mse_1 <- function(values, k){
  return((sum((unlist(values, use.names = F)-1) - k)^2)/length(values))
}



make_results <- function(){
  
  for(k in 2:4){
    print(k)
    results <- readRDS(paste("data_K/save/results", as.character(k), sep = "_"))
    for(r in results){
      print(mse(r, k))
    }
    results_lower <- readRDS(paste("data_K/save/results_lower", as.character(k), sep = "_"))
    print(mse_1(results_lower, k))
  }
  
  
}