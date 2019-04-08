library(MASS)

load_data <- function(RDS_file = "data_K/full_test_admixed1_data_pop_2-16.rds"){

  data <- readRDS(RDS_file)
}

get_structure_result <- function(data, input_file = "~/structure/console/input", output_file = "~/structure/console/output", structure_path = "~/structure/console", max_k = 20, iter = 10){

  max_k <- 10
  iter <- 4
  
  results <- matrix(0, nrow = max_k, ncol = 2)
  
  round <- matrix(0, nrow = iter, ncol = 2)
  
    for(k in 2:max_k){
      
      for(i in 1:iter){
        
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
        
        round[i] = parse_output()
      }
      
      results[k,] <- round_stats(round)
    }
  
  print(evanno_statistic(results))
}


round_stats <- function(round_results){
  
  mean_llike <- sum(round_results)/(length(round_results))
  
  std <- sqrt(sum((round_results - mean_llike)^2)/(length(round_results)-1))
  
  return(c(mean_llike, std))
}

likelihood_derivative <- function(values){
  
  derivative <- vector("numeric", length = length(values)-1)
  
  for(i in 2:length(values)){
    derivative[i-1] <- values[i]- values[i-1]
  }
  print(past("d ", derivative))
  return(derivative)
}

evanno_statistic <- function(stats){
  
  print(stats)
  sec_derivatives <- likelihood_derivative(likelihood_derivative(stats[1,]))
  
  result <- vector("numeric", length = length(sec_derivatives))
  
  for(i in 1:length(sec_derivatives)){
    result[i] <- sec_derivatives[i] / stats[i]
  }
  
  return(result)
}



parse_output <- function(filepath = "~/structure/console/output_f"){
  
  con <- file(filepath, "r")
  #readall <- readLines(con, n = -1)
  #print(readall)
  while(T){
    line <- readLines(con, n = 1)
    #print(line)
    if(grepl("^Mean value of ln likelihood = *", line,  perl = T)[1]){
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
