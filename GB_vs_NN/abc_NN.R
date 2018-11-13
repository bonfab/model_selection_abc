library(abc)


classifyABC <- function(test, sim, method = "mnlogistic"){
  
  for(t in test){
    modsel <- postpr(t, sim[,ncol(summary)], sim[], method = method)
    print(modsel$pred)
  }
  
}