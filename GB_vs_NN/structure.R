library(MASS)

load_data <- function(RDS_file = "data_K/full_test_admixed1_data_pop_2-16.rds"){

  data <- readRDS(RDS_file)
}

get_structure_result <- function(data, input_file = "~/structure/console/input", output_file = "~/structure/console/output", structure_path = "~/structure/console", max_k = 20, iter = 10){

  max_k <- 16
  iter <- 5
  
  results <- matrix(0, nrow = iter, ncol = max_k)
  
  seque <- vector("numeric", length = max_k)
  
  for(i in 1:iter){
  
    for(k in 1:max_k){
      
      
        
        write.matrix(data, input_file, sep = " ")
        K <- paste("-K", k, sep = " ")
        loci <- paste("-L", ncol(data), sep = " ")
        sample_size <- paste("-N", nrow(data), sep = " ")
        input <- paste("-i", input_file, sep = " ")
        output <- paste("-o", output_file, sep = " ")
        program <- paste(structure_path, "structure", sep = "/")
        mainparams <- paste(paste("-m", structure_path, sep = " "), "mainparams", sep = "/")
        extraparams <- paste(paste("-e", structure_path, sep = " "), "extraparams", sep = "/")
    
        
        command <- paste(c(program, mainparams, extraparams, K, loci, sample_size, input, output), collapse = " ")
        system(command)
        
        seque[k] = parse_output(paste(output_file, "_f", sep = ""))
      }
      
    results[i,] <- seque
    }
  
  print(evanno_statistic(results))
}


likelihood_derivative <- function(values){
  
  derivative <- vector("numeric", length = length(values)-1)
  
  for(i in 1:(length(values)-1)){
    derivative[i] <- values[i+1]- values[i]
  }
  return(derivative)
}

# FIRST SEQUENCE THEN MEAN

evanno_statistic <- function(stats){
  
  #print(stats)
  #sec_derivatives <- abs(likelihood_derivative(likelihood_derivative(stats[,1])))
  
  deriv <- t(apply(stats, 1, likelihood_derivative))
  #print("div1")
  #print(deriv)
  deriv <- t(apply(deriv, 1, likelihood_derivative))
  #print("div2")
  #print(deriv)
  deriv <- t(apply(deriv, 1, abs))
  means <- apply(deriv, 2, mean)
  
  sds <- apply(stats, 2, sd)
  
  #print("sds:")
  #print(sds)
  #print("means:")
  #print(means)
  
  result <- means/(sds[3:length(sds)])
  
  #print(result)
  
  return(which.max(result)+2)
}



parse_output <- function(filepath = "~/structure/console/output_f"){
  
  con <- file(filepath, "r")
  #readall <- readLines(con, n = -1)
  #print(readall)
  while(T){
    line <- readLines(con, n = 1)
    #print(line)
    if(grepl("^Estimated Ln Prob of Data*", line,  perl = T)[1]){
      llike <- strsplit(line, " ")[[1]]
      llike <- llike[length(llike)]
      break
    }
  }
  
  close(con)
  return(as.numeric(llike))
}

test <- function(data, true_k){

    plot(data)
    print("true_k:")
    print(true_k)
    print("estimate:")
    print(get_structure_result(data))

}


run <- function(){
    d <- load_data()

    test(d[[1]][[1]], d[[2]][[1]])

    #for(i in 1:length(d[[1]])){
    #    test(d[[1]][[i]], d[[2]][[i]])
    #}

}
