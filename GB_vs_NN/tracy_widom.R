library(LEA)

load_data <- function(RDS_file = "data_K/full_test_admixed1_data_pop_2-16.rds"){
  
  data <- readRDS(RDS_file)
  
}

get_tw_estimate <- function(data, p_value = 0.03){
  
  write.lfmm(data, "genotypes.lfmm")
  pc <- pca("genotypes.lfmm", scale = TRUE)
  
  tw <- tracy.widom(pc)
  
  significant <- 0
  for(i in tw$percentage){
    if(i >= p_value){
      significant <- significant + 1
    }
  }
  
  #print(tw$pvalues[1:5])
  #plot(tw$percentage)
  return(significant)
}

test <- function(data, true_k){
  
  #data <- reduce_dim(data)
  print("true_k:")
  print(true_k)
  print("estimate:")
  print(get_tw_estimate(data))
  
}

d <- load_data()

for(i in 1:length(d[[1]])){
  test(d[[1]][[i]], d[[2]][[i]])
}

