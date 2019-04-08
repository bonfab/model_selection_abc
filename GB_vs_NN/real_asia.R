source("gradient_boosting.R")
source("generate_data2.R")
library(missMDA)

snp_data <- readRDS("data_K/real_asia_data")

snp_data[snp_data == -1] <- NA

snp_data <- snp_data[,-which(apply(snp_data,2,sd,na.rm=TRUE)==0)]


# make data.frame, numeric columns
snp_data <- sapply(snp_data, as.factor)
snp_data <- as.data.frame(snp_data)

imputed <- imputeMCA(snp_data)$completeObs

imputed <- sapply(imputed, as.numeric)

pc = prcomp(imputed)
plot(pc)



xgb_model <- load_model()

summary <- PCA_summary(imputed)
print(paste("XGB:", classifyXGBoostSingle(xgb_model, summary) + 2, sep = " "))

