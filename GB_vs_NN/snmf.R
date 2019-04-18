# Title     : TODO
# Objective : TODO
# Created by: bergmanf
# Created on: 12/04/19

library(LEA)
#source("generate_data2.R")
#Rcpp::sourceCpp("sample_bernoulli_matrix_no_central.cpp")

#data <- generate_prob(4, number_locus = 10000, sample_size = 400)
#data <- bernoulli_matrix(data)

#write.geno(data, "geno_output2.geno")



get_snmf_estimate <- function(data, rep = 5, max_K = 15){
  
  
  write.geno(data, "geno_output.geno")
  
  project.snmf = snmf("geno_output.geno",
                      K = 1:max_K,
                      entropy = TRUE,
                      repetitions = rep,
                      #alpha = 1000,
                      project = "new")
  
  # plot cross-entropy criterion of all runs of the project
  #plot(project.snmf, cex = 1.2, col = "lightblue", pch = 19)
  
  # get the cross-entropy of the 10 runs for K = 4
  ce = sapply(1:max_K, function(x) cross.entropy(project.snmf, K = x))
  
  best = ceiling(which.min(ce)/rep)
  
  
  remove.snmfProject("geno_output.snmfProject")
  
  return(best)
  
}



     # select the run with the lowest cross-entropy for K = 4
     #best = which.min(ce)
    

     # display the Q-matrix

     #my.colors <- c("tomato", "lightblue",
                   #"olivedrab", "gold", "pink")

     #barchart(project.snmf, K = 5, run = best,
      #       border = NA, space = 0, col = my.colors,
       #      xlab = "Individuals", ylab = "Ancestry proportions",
        #     main = "Ancestry matrix") -> bp

     #axis(1, at = 1:length(bp$order),
      #   labels = bp$order, las = 3, cex.axis = .4)

#remove.snmfProject("geno_output.geno")