library(ggplot2)

pA <- c(0.5, 0.3, 0.95)
F <- 0.01
#a <- data.frame(row.names = as.character(pA))
Frequencies <- lapply(pA, function(p) rbeta(10000000, p * (1-F)/F, (1-p)*(1-F)/F))
pA <- lapply(pA, rep, 10000000)
  
Frequencies <- as.vector(do.call(cbind, Frequencies))
pA <- as.vector(do.call(cbind, pA))

a <- t(rbind(Frequencies, pA))

a <- data.frame(a)

a[,2] <- as.factor(a[,2])

#print(a)
#d <- density(a)
#plot(d, type = "l")
#a <- as.data.frame(a)

ggplot(a, aes(x = Frequencies, y=..scaled.., color = pA)) + geom_density()

ggsave("rebta_plot_001", device = "png")

x <- rbeta(10000, 0.2, 2)

x <- as.data.frame(x)

ggplot(x, aes(x = x, y=..scaled..)) + geom_density()

