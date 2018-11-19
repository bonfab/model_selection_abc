

getData <- function(dir = "../data/4models"){
  
  ancestral_migration <- read.table(paste(dir, "AM_2M_2N_0/ABCstat.txt", sep = "/"), header = TRUE)
  ancestral_migration <- cbind(ancestral_migration[,-1], data.frame(theory = rep(factor("am"), nrow(ancestral_migration))))

  isolation_migration <- read.table(paste(dir, "IM_2M_2N_0/ABCstat.txt", sep = "/"), header = TRUE)
  isolation_migration <- cbind(isolation_migration[,-1], data.frame(theory = rep(factor("im"), nrow(isolation_migration))))
  
  secondary_contact <- read.table(paste(dir, "SC_2M_2N_0/ABCstat.txt", sep = "/"), header = TRUE)
  secondary_contact <- cbind(secondary_contact[,-1], data.frame(theory = rep(factor("sc"), nrow(secondary_contact))))
  
  strict_isolation <- read.table(paste(dir, "SI_2N_0/ABCstat.txt", sep = "/"), header = TRUE)
  strict_isolation <- cbind(strict_isolation[,-1], data.frame(theory = rep(factor("si"), nrow(strict_isolation))))
  
  combined <- rbind(ancestral_migration, isolation_migration, secondary_contact, strict_isolation)
  combined$theory <- as.factor(combined$theory)
  return(combined)
}

getDataFirstN <- function(size = 100, dir = "../data/4models"){
  
  ancestral_migration <- read.table(paste(dir, "AM_2M_2N_0/ABCstat.txt", sep = "/"), header = TRUE, nrows = size)
  ancestral_migration <- cbind(ancestral_migration[,-1], data.frame(theory = rep(factor("am"), nrow(ancestral_migration))))

  isolation_migration <- read.table(paste(dir, "IM_2M_2N_0/ABCstat.txt", sep = "/"), header = TRUE, nrows = size)
  isolation_migration <- cbind(isolation_migration[,-1], data.frame(theory = rep(factor("im"), nrow(isolation_migration))))
  
  secondary_contact <- read.table(paste(dir, "SC_2M_2N_0/ABCstat.txt", sep = "/"), header = TRUE, nrows = size)
  secondary_contact <- cbind(secondary_contact[,-1], data.frame(theory = rep(factor("sc"), nrow(secondary_contact))))
  
  strict_isolation <- read.table(paste(dir, "SI_2N_0/ABCstat.txt", sep = "/"), header = TRUE, nrows = size)
  strict_isolation <- cbind(strict_isolation[,-1], data.frame(theory = rep(factor("si"), nrow(strict_isolation))))
  
  combined <- rbind(ancestral_migration, isolation_migration, secondary_contact, strict_isolation)
  combined$theory <- as.factor(combined$theory)
  return(combined)
}

mergeAM_SIandIM_SC <- function(data){
  
  merge <- function(theory){
    theory <- as.character(theory)
    if(theory == 'am' || theory == 'si'){
      return('am_si')
    } else{
      return('im_sc')
    }
  }
  
  data[,ncol(data)] <- as.factor(sapply(data[,ncol(data)], merge))
  
  return(data)
}

