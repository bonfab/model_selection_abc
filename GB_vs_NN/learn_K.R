source("gradient_boosting.R")


load_data <- function(RDS_file = "data_K/data_pop_prio_1-25_1.rds"){
  
  data <- readRDS(RDS_file)
  
  return(cbind(data[[1]], data[[2]]))
  
}


m <- load_data()

normalize <- min(m[,length(m[1,])])

model <- trainXGBoost(m[,1:length(m[1,]) -1], m[,length(m[1,])] - normalize)