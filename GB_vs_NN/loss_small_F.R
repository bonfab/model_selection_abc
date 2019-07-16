
source("gradient_boosting.R")
source("F_noise_generate_data.R")

F_values = c(0.001, 0.002, 0.003, 0.005, 0.01, 0.02, 0.05, 0.1, 0.2, 0.5)
xgb_model <- load_model("./plain.model")
n <- 50
mses <- c()

for(f in F_values){
  s <- 0
  print(f)
  for(i in 1:10){
    for(k in 3:8){
      summary <- PCA_summary(generate(k, F_value = f))
      xgb_estimate <- classifyXGBoostSingle(xgb_model, summary) + 2
      s <- s + (xgb_estimate - k)^2
    }
  }
  
  s <- s/n
  mses <- c(mses, s)
}

b <- prcomp(generate(3, number_admixed = 0, pop_sizes = c(0.1, 0.3, 0.6), sample_size = 200, number_locus = 10000, F_value = 0.0032))
plot(b$x)
plot(b)
c <- prcomp(generate(3, number_admixed = 0, pop_sizes = c(0.1, 0.3, 0.6), sample_size = 200, number_locus = 10000, F_value = 0.04))
plot(c$x)
plot(c)

library(LEA)
data <- read.table("data_K/chr21.geno", header = T, sep = "\t", row.names = 1)
a <- data[1,]
b <- as.factor(data[1,])

make_normal <- function(data){
  data2 <- matrix(,nrow = nrow(data), ncol = ncol(data))
  for(i in 1:nrow(data)){
    data2[i,] <- as.integer(as.factor(as.integer(data[i,]))) -1
  }
  return(data2)
}

make_normal()

url = "http://membres-timc.imag.fr/Olivier.Francois/Arabidopsis/A_thaliana_chr1.geno"
download.file(url = url, destfile = "./data_K/A_thaliana_chr1.geno")

Rcpp::sourceCpp("sample_bernoulli_matrix_no_central.cpp")

for(k in 2:8){
  for(i in 1:10){
    saveRDS(bernoulli_matrix(generate_prob(k)), paste("./data_K/save/test", as.character(k),  as.character(i), sep = "_"))
  }
}

