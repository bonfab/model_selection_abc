# Title     : TODO
# Objective : TODO
# Created by: bergmanf
# Created on: 28/03/19

library(mclust)

load_data <- function(RDS_file = "data_K/full_admixed1_data_pop_2-16.rds"){

  data <- readRDS(RDS_file)

}



test <- function(data, true_k){

    print("true_k:")
    print(true_k)
    print("estimate:")
    print(Mclust(data, 1:20, c("EII"))$G)

}

d <- load_data()

for(i in 1:length(d[[1]])){
    test(d[[1]][[i]], d[[2]][[i]])
}

#for(d in load_data()){
 #   print(d[[2]])
#    test(d[[1]], d[[2]])
#}