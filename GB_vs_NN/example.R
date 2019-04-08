library(missMDA)

# snp data, values \elem {-1, 0, 1, 2}, -1 for missing
snp_data <- read.table("http://membres-timc.imag.fr/Olivier.Francois/genotype_Asia.txt")

# encode missing as NA
snp_data[snp_data == -1] <- NA

# make data.frame, numeric columns
snp_data <- sapply(snp_data, as.numeric)
snp_data <- as.data.frame(snp_data)

#impute
imputed <- imputePCA(snp_data)


# also throws error, but different
#snp_data <- sapply(snp_data, as.factor)
#snp_data <- as.data.frame(snp_data)
#imputed <- imputeMCA(snp_data)
