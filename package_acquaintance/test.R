Sys.setlocale("LC_MESSAGES", "en_US.utf8")

library(abc)

obs <- ABCstat[1,]



result <- abc(target = obs, param = priorfile, sumstat = ABCstat[-1,], tol = 0.02, method = "loclinear")

