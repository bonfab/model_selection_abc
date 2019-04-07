source("gradient_boosting.R")


genotype = read.table("http://membres-timc.imag.fr/Olivier.Francois/genotype_Asia.txt")

pc = prcomp(genotype)

plot(pc)

