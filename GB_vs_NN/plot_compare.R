library(ggplot2)

data <- readRDS("data_K/comparison_test.rds")

print(data)

means <- t(vapply(min(data[,1]):max(data[,1]), function(x) apply(data[which(data[,1] == x),], 2, mean), numeric(ncol(data))))
colnames(means) <- c("m", "xgb", "snmf", "tw")
means <- as.data.frame(means)

print(means)

ggplot(means, aes(m)) +
    geom_line(aes(y = xgb), colour = "blue") +
    geom_line(aes(y = snmf), colour = "red") +
    geom_line(aes(y = tw), colour = "green")