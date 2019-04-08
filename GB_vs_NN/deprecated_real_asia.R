source("gradient_boosting.R")
library(missMDA)

distance <- function(i1, i2, data){
  
  stds <- sqrt(apply(data, 2, var))
  
  sum <- 0
  for(i in 1:length(i1)){
    add <- i1[i] + i2[i]
    if(!is.na(add)){
      sum <- sum + add
    } else {
      sum <- sum + stds[i]
    }
  }
  return(sum)
}

insert <- function(dist, value){
  
  closest <- rep(NA, 4)
  values <- rep(NA, 4)
  
  for(i in 1:length(closest)){
    if(is.na(closest[i] || closest[i] < dist)){
      closest[i] <- dist
      values[i] <- value
    }
  }
  return(list(closest, values))
}

impute_from_closest <- function(row, col, dist, data){
  
  #dist[row] <- 2 * ncol(data) + 1
  names(dist) <- as.character(1:length(dist))
  sorted <- sort(dist)
  
  closest <- rep(NA, 4)
  values <- rep(NA, 4)
  
  for(i in 1:length(sorted)){
    d<- sorted[i]
    row_name <- as.integer(names(sorted[i]))
    #(paste("rowname", row_name))
    value <- data[row_name, col]
    
    if(value != -1){
      
      for(i in 1:length(closest)){
        if(is.na(closest[i]) || closest[i] < d) {
          closest[i] <- d
          values[i] <- value
          break
        }
      }
      if(!is.na(tail(closest, 1))){
        break
      }
    }
  }
    
  result <- as.integer(names(sort(table(values))))
  for(i in tail(result, length(result))){
    if(!is.na(i)){
      print(paste("success:", i))
      return(i)
    }
  }
  print(paste("no replacement:", i))
  return(1)
}
  
impute <- function(data){
  
  for(row in 1:nrow(data)){
    print(row)
    
    dist <- apply(data, 1, function(x) distance(x, data[row,], data))
    #print(dist)
    
    for(i in 1:ncol(data)){
      if(data[row, i] == -1){
        data[row, i] <- impute_from_closest(row, i, dist, data)
      }
    }
  }
  return(data)
}

to_matrix <- function(data){
  
  m <- matrix(0, nrow = nrow(data), ncol = ncol(data))
  
  for(i in 1:ncol(m)){
    for(j in 1:nrow(m)){
      m[j, i] <- as.numeric(data[j,i])
    }
  }
  return(m)
}

snp_data <- readRDS("data_K/real_asia_data")

snp_data[snp_data == -1] <- NA

snp_data <- snp_data[,-which(apply(snp_data,2,sd,na.rm=TRUE)==0)]


# make data.frame, numeric columns
snp_data <- sapply(snp_data, as.factor)
snp_data <- as.data.frame(snp_data)

imputed <- imputeMCA(snp_data)$completeObs

print(imputed)

#data <- matrix(NA, nrow = nrow(genotype), ncol = ncol(genotype))

#for(i in 1:ncol(genotype)){
#  for(j in 1:nrow(genotype)){
#    data[j, i] <- genotype[j, i]
#  }
#}

check <- function(genotype){
  count <- 0
  for(col in 1:ncol(genotype)){
    for(row in 1:nrow(genotype)){
      if(genotype[row, col] == -1){}
      count <- count +1
    }
  }
  print(count)
}

#data <- impute(data)
#genotype <- impute(genotype)
#genotype <- imputeMCA(genotype)$completeObs

#print(genotype[1:20,1:20])

pc = prcomp(genotype)
plot(pc)



