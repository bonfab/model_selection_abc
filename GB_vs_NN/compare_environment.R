source("clust.R")
source("structure.R")
source("tracy_widom.R")
source("generate_data2.R")

compare <- function(K = 2:2, size = 1){

    for(k in K){

        for(i in 1:size){

            prob <- generate_prob(k)
            data <- bernoulli_matrix(prob)

            #call structure
            get_structure_result(data)




        }

    }

}

Rcpp::sourceCpp("sample_bernoulli_matrix_no_central.cpp")
compare()