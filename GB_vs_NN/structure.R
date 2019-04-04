library(MASS)

load_data <- function(RDS_file = "data_K/full_test_admixed1_data_pop_2-16.rds"){

  data <- readRDS(RDS_file)

}

get_structure_result <- function(data, input_file = "~/structure/console/input", output_file = "~/structure/console/output", structure_path = "~/structure/console"){

    write.matrix(data, input_file, sep = " ")
    K <- paste("-K", 2, sep = " ")
    loci <- paste("-L", ncol(data), sep = " ")
    sample_size <- paste("-N", nrow(data), sep = " ")
    input <- paste("-i", input_file, sep = " ")
    output <- paste("-o", output_file, sep = " ")
    program <- paste(structure_path, "structure", sep = "/")
    mainparams <- paste(paste("-m", structure_path, sep = " "), "mainparams", sep = "/")
    extraparams <- paste(paste("-e", structure_path, sep = " "), "extraparams", sep = "/")


    command <- paste(c(program, mainparams, extraparams, K, loci, sample_size, input, output), collapse = " ")
    print(command)
    system(command)
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