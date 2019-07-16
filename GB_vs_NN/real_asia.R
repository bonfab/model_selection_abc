source("gradient_boosting.R")
source("generate_data2.R")
#source("structure.R")
#source("clust.R")

#library(missMDA)
#library(LEA)

#snp_data <- readRDS("data_K/real_asia_data")

#snp_data[snp_data == -1] <- NA

#snp_data <- snp_data[,-which(apply(snp_data,2,sd,na.rm=TRUE)==0)]


# make data.frame, numeric columns
#snp_data <- apply(snp_data, 2, as.factor)
#snp_data <- as.data.frame(snp_data)


#print("impute")
#imputed <- imputeMCA(snp_data)$completeObs

#saveRDS(imputed, "imputed.rds")
imputed <- readRDS("imputed.rds")

#snp_data <- apply(snp_data, 2, as.numeric)
#colnames(snp_data) <- NULL

#snp_data <- scale(snp_data, center = T, scale = F)

#print("integer")
#imputed <- apply(imputed, 2, as.integer)
colnames(imputed) <- NULL

print("numeric scaled")
imputed_n <- scale(apply(imputed, 2, as.numeric), center = T, scale = F)
#imputed_n <- apply(imputed, 2, as.numeric)
#imputed_n <- scale(imputed_n, center = T, scale = F)

#pc = prcomp(imputed_n)
#plot(pc)

#print(imputed_n)
#print(class(imputed_n))


#print(dim(snp_data))

xgb_model <- load_model("./lower.model")

#print("summary")
summary <- PCA_summary(thaliana)
#print(paste("XGB:", classifyXGBoostSingle(xgb_model, summary) + 2, sep = " "))

print("xgb")
xgb_estimate <- classifyXGBoostSingle(xgb_model, summary) + 2

print(xgb_estimate)

xgb_model <- load_model("./plain.model")
xgb_estimate <- classifyXGBoostSingle(xgb_model, summary) + 2
print(xgb_estimate)

xgb_model <- load_model()
xgb_estimate <- classifyXGBoostSingle(xgb_model, summary) + 2

print(xgb_estimate)


#chr21 <- readRDS("./data_K/chr21_clean.rds")

#plot(prcomp(chr21))

#plot(PCA_summary(chr21))

#xgb_estimate <- classifyXGBoostSingle(xgb_model, PCA_summary(chr21)) + 2
#print(xgb_estimate)
#print("structure")
#structure_estimate <- get_structure_result(imputed, "~/structure/console/asia_input", "~/structure/console/asia_output")
#print(structure_estimate)

source("tracy_widom.R")
print("tracy widom")
tw_estimate <- get_tw_estimate(imputed) +1
print(tw_estimate)


tw_con <- which.max(likelihood_derivative(likelihood_derivative(get_tw_estimate(imputed, all = T)))) + 2
print(tw_con)

source("snmf.R")
print("snmf")
snmf_estimate <- get_snmf_estimate(imputed)
print(snmf_estimate)

library(LEA)
thaliana <- read.geno("data_K/A_thaliana_chr1.geno")
summary <- PCA_summary(thaliana)
xgb_estimate <- classifyXGBoostSingle(xgb_model, summary) + 2
print(xgb_estimate)

#print("cluster")
#clust_estimate <- get_clust_estimate(imputed_n)
#print(clust_estimate)

#saveRDS(c(xgb_estimate, structure_estimate, tw_estimate, clust_estimate), "data_K/results_asia.rds")
