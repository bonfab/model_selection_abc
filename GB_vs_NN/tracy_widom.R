#library(LEA)

load_data <- function(RDS_file = "data_K/full_test_admixed1_data_pop_2-16.rds"){
  
  data <- readRDS(RDS_file)
  
}

get_tw_estimate <- function(data, all = F, p_value = 0.05){

  data <- matrix(data, nrow = nrow(data), byrow = T)
  #print(class(data))
  write.lfmm(data, "genotypes.lfmm")
  pc <- pca("genotypes.lfmm", scale = TRUE)
  
  tw <- tracy.widom(pc)
  
  #plot(tw)
  #print(tw$percentage)
  #print(tw$pvalue)
  
  if(all){
    remove.pcaProject("./genotypes.pcaProject")
    return(tw$percentage)
  }
  
  significant <- 0
  for(i in tw$percentage){
    if(i >= p_value){
      significant <- significant + 1
    }
  }
  
  #print(tw$pvalues[1:5])
  #plot(tw$percentage)
  remove.pcaProject("./genotypes.pcaProject")
  return(significant)
}

test <- function(data, true_k){
  
  #data <- reduce_dim(data)
  print("true_k:")
  print(true_k)
  print("estimate:")
  print(get_tw_estimate(data))
  
}

run <- function(){
    d <- load_data()

    #test(d[[1]][[1]], d[[2]][[1]])

    for(i in 1:length(d[[1]])){
        test(d[[1]][[i]], d[[2]][[i]])
    }

}

