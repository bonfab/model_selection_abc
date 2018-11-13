library(abc)
library(parallel)


classifyABC <- function(test, train, label, method = "mnlogistic"){
  
  no_cores <- detectCores() - 2
  
  #print(as.vector(test))
  
  clust <- makeCluster(no_cores)
  
  l <- parApply(clust, as.vector(test), 1, postpr, index = as.vector(label), sumstat = train, tol = 0.05, method = method)
  
  l <- as.factor(sapply(l, function(x) names(x$pred)[which.max(x$pred)]))
  
  return(l)

}
