################
# Median Abundance Analysis #
################
## Uses alt method for median richness
## Using colleen date only
## DIFFERENCE HERE IS EXTRACTS COEF FOR EACH CV TO GET CI and incidence rate

## Set up environment: set seed, packages, functions

rm(list=ls())
set.seed(123)
RNGkind("L'Ecuyer-CMRG")

library(readr)
library(plyr)
library(dplyr)
library(AER)
library(MASS)
library(car)
library(mpath)
library(cbar)
library(psych) 
library(stargazer)
library(xtable)
library(ape)
library(caret)
library(glmnet)
library(corrplot)
library(tibble)
library(tidyr)
library(scales)
library(cowplot)
#library(drlib)
library(grid)
library(gridExtra)
library(parallel); no_cores <- detectCores() - 1

cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}


output.function.poisson <- function(cv_size){
  
  fit <- cv.glmnet(X,y, alpha = 1, standardize = TRUE, lambda = lambdas_to_try, family = "poisson", nfolds = cv_size)
  
  # coefficients from lambda min
  coef.included <- as.data.frame(as.matrix(coef(fit, s = fit$lambda.min)))
  coef.included <- coef.included %>% rownames_to_column(var = "Variable")
  names(coef.included)[2] <- "Value"
  
  # adjusted R sq
  
  yhat <- predict(fit, X, s = fit$lambda.min, type = "response")
  sst <- sum((y - mean(y))^2)
  ssr <- sum((y - yhat)^2)
  rsq <- 1 - (ssr / sst)
  number.coefs <- coef(fit,s = "lambda.min")[1:nrow(coef(fit, s = "lambda.min"))]
  number.coefs <- length(number.coefs[number.coefs!=0]) - 1 
  adj.rsq <- 1-(((1-rsq)*(29-1))/(29-number.coefs-1))
  
  # spatial autocorrelation
  
  res <- y - yhat
  fit.i <- Moran.I(as.vector(res), site.dists.inv, alternative = "greater")
  
  
  #return(list(differences, adj.rsq, AICc))
  return(list(coef.included, rsq, adj.rsq, fit.i$p.value))
  
}

## Read in data 

if(getwd() == "/Users/Zoe/Desktop/Bee_Project/Analysis1"){
  
  abun <- read_csv("Outputs/abundance_metrics_col.csv")
  
  effort <- read_csv("Outputs/effort_col.csv")
  names(effort)[3] <- "Effort"
  
  sites <- read.csv("~/Desktop/Bee_Project/Analysis1/Outputs/col_rae_sites.csv", stringsAsFactors = F)
  
  preds_300m <- read.csv("~/Desktop/Bee_Project/Analysis1/GIS/Analysis1_predictors_300m_updated_units.csv", stringsAsFactors = F)
  preds_500m <- read.csv("~/Desktop/Bee_Project/Analysis1/GIS/Analysis1_predictors_500m_updated_units.csv", stringsAsFactors = F)
  preds_1km <- read.csv("~/Desktop/Bee_Project/Analysis1/GIS/Analysis1_predictors_1km_updated_units.csv", stringsAsFactors = F)
  
  
} else{
  
  setwd("/home/zvolenec/bees")
  
  abun <- read_csv("abundance_metrics_col.csv")
  
  effort <- read_csv("effort_col.csv")
  names(effort)[3] <- "Effort"
  
  sites <- read.csv("col_rae_sites.csv", stringsAsFactors = F)
  
  preds_300m <- read.csv("Analysis1_predictors_300m_updated_units.csv", stringsAsFactors = F)
  preds_500m <- read.csv("Analysis1_predictors_500m_updated_units.csv", stringsAsFactors = F)
  preds_1km <- read.csv("Analysis1_predictors_1km_updated_units.csv", stringsAsFactors = F)
  
  
  
}

# Get together inverse distance matrix
sites <- merge(sites, abun, by = "Unit_Nm")
sites <- sites[!duplicated(sites[1]),]
site.dists <- as.matrix(dist(cbind(sites$Longitude, sites$Latitude),
                             method = "euclidean"))
site.dists.inv <- 1/site.dists
diag(site.dists.inv) <- 0


###################
##### Models ######
###################

plot(density(abun$Rare_Abundance)); shapiro.test(abun$Rare_Abundance)


#### 1km ####


## Get variables in order

data.1km <- abun[c(2,3)] %>% left_join(preds_1km, by = "Unit_Nm") %>% 
  left_join(effort[c(2,3)], by = "Unit_Nm") %>% left_join(sites[c(1,7)], by = "Unit_Nm") %>%
  dplyr::select(-Unit_Nm)

colnames(data.1km)[6:71] <- substr(colnames(data.1km[6:71]), 1, 3)
for(i in 6:70){
  
  for(j in (i+1):71){
    
    if(colnames(data.1km)[i] == colnames(data.1km)[j]){
      data.1km[j]<- data.1km[i] + data.1km[j]
      colnames(data.1km)[i] <- "delete"
    }
    
    else{
      next
    }
  }
}

data.1km <- data.1km[,-which(names(data.1km) %in% "delete")]

for(i in ncol(data.1km):2){ # take out variables where one / two nonzeros are driving relationship
  pred <- names(data.1km)[i]
  plot(Rare_Abundance ~ get(pred, data.1km), data = data.1km, xlab = pred)
} 

data.1km <- data.1km[-c(grep("X13",colnames(data.1km)), grep("X14",colnames(data.1km)),
                              grep("X15",colnames(data.1km)),
                              grep("X16",colnames(data.1km)), 
                              grep("X22",colnames(data.1km)),
                              grep("X23",colnames(data.1km)), 
                              grep("X42",colnames(data.1km)), 
                              grep("X51",colnames(data.1km)), 
                              grep("X52",colnames(data.1km)), 
                              grep("X53",colnames(data.1km)),
                              grep("X54",colnames(data.1km)), 
                              grep("X61",colnames(data.1km)), 
                              grep("X71",colnames(data.1km)),
                              grep("X72",colnames(data.1km)), 
                              grep("X73",colnames(data.1km)),
                              grep("X74",colnames(data.1km)), 
                              grep("Road_length_km",colnames(data.1km)))]                         



## Take out correlated predictors

data.1km.cleaned <- data.1km

predictors.1km <- data.1km.cleaned[-1]
p.mat.1km <- cor.mtest(predictors.1km)
cor.1km <- cor(predictors.1km)
cor.1km[abs(cor.1km) < 0.85 ] = 0 
corrplot(cor.1km, type="lower",
         p.mat = p.mat.1km, sig.level = 0.05,insig = "blank",method="number",number.cex=0.7)

data.1km.cleaned <- data.1km.cleaned[-grep("^Perimeter_km$",colnames(data.1km.cleaned))]
data.1km.cleaned <- data.1km.cleaned[-grep("SDI_LU12",colnames(data.1km.cleaned))] 
data.1km.cleaned <- data.1km.cleaned[-grep("^SDI_TYPE12_Anthro$",colnames(data.1km.cleaned))] 
data.1km.cleaned <- data.1km.cleaned[-grep("^Number_Houses$",colnames(data.1km.cleaned))] 
data.1km.cleaned <- data.1km.cleaned[-grep("^SDI_TYPE12_Nat$",colnames(data.1km.cleaned))] 
data.1km.cleaned <- data.1km.cleaned[-grep("^LDI$",colnames(data.1km.cleaned))] 

#saveRDS(data.1km.cleaned, "Bee_Ecology_Models/abundance_data_univariate_graphs.rds")

## Run Lasso

lambdas_to_try <- c(0,10^seq(-3, 2, length.out = 1000))
n <- 1000

y <- data.1km.cleaned %>% dplyr::select(Rare_Abundance) %>% as.matrix()
X <- data.1km.cleaned %>% dplyr::select(-Rare_Abundance) %>% as.matrix()

set.seed(123)
cl <- makeCluster(no_cores)
m.1km <- mclapply(rep(5, n), output.function.poisson,
               mc.set.seed = T); stopCluster(cl)


# Get coefficients summary
# http://www.haowang.pw/blog/Poisson-Coefficient-Interpretation/
# https://bookdown.org/roback/bookdown-bysh/ch-poissonreg.html
coefficients.1km <- data.frame()
for(i in 1:n){
  coefficients.1km <- rbind(coefficients.1km,m.1km[[i]][[1]])
}

coef.1km.summ <- coefficients.1km %>% group_by(Variable) %>%
  summarize(Avg = mean(Value), SD = sd(Value), Min = min(Value), Max = max(Value),
            Conf.low = sd(Value) * (-1.96/sqrt(n)), Conf.high = sd(Value) * (1.96/sqrt(n)),
            Included = sum(Value != 0)) %>% arrange(desc(Included))

# Get adj R sq summary
rsq.1km <- c()
for(i in 1:n){
  rsq.1km[i] <- m.1km[[i]][[2]]
}
summary(rsq.1km)

# Get adj R sq summary
adj.rsq.1km <- c()
for(i in 1:n){
  adj.rsq.1km[i] <- m.1km[[i]][[3]]
}
summary(adj.rsq.1km)

# Get moran'i summary
i.p.1km <- c()
for(i in 1:n){
  i.p.1km[i] <- m.1km[[i]][[4]]
}
summary(i.p.1km)


#### 500m ####

## Get variables in order
data.500m <- abun[c(2,3)] %>% left_join(preds_500m, by = "Unit_Nm") %>% 
  left_join(effort[c(2,3)], by = "Unit_Nm") %>% left_join(sites[c(1,7)], by = "Unit_Nm") %>%
  dplyr::select(-Unit_Nm)

colnames(data.500m)[6:66] <- substr(colnames(data.500m[6:66]), 1, 3)
for(i in 6:65){
  
  for(j in (i+1):66){
    
    if(colnames(data.500m)[i] == colnames(data.500m)[j]){
      data.500m[j]<- data.500m[i] + data.500m[j]
      colnames(data.500m)[i] <- "delete"
    }
    
    else{
      next
    }
  }
}

data.500m <- data.500m[,-which(names(data.500m) %in% "delete")]

for(i in ncol(data.500m):2){ # take out variables where one / two nonzeros are driving relationship
  pred <- names(data.500m)[i]
  plot(Rare_Abundance ~ get(pred, data.500m), data = data.500m, xlab = pred)
} 


data.500m <- data.500m[-c(grep("X13",colnames(data.500m)), grep("X14",colnames(data.500m)), 
                                grep("X16",colnames(data.500m)), 
                                grep("X17",colnames(data.500m)),
                                grep("X22",colnames(data.500m)), 
                                grep("X23",colnames(data.500m)), 
                                grep("X24",colnames(data.500m)),
                                grep("X42",colnames(data.500m)), 
                                grep("X51",colnames(data.500m)),
                                grep("X52",colnames(data.500m)), 
                                grep("X53",colnames(data.500m)),
                                grep("X54",colnames(data.500m)), 
                                grep("X71",colnames(data.500m)),
                                grep("X72",colnames(data.500m)), 
                                grep("X73",colnames(data.500m)),
                                grep("X74",colnames(data.500m)),
                                grep("LDI",colnames(data.500m)), 
                                grep("Road_length_km",colnames(data.500m))
)]


## Take out correlated predictors

data.500m.cleaned <- data.500m

predictors.500m <- data.500m.cleaned[-1]
p.mat.500m <- cor.mtest(predictors.500m)
cor.500m <- cor(predictors.500m)
cor.500m[abs(cor.500m) < 0.85 ] = 0 
corrplot(cor.500m, type="lower",
         p.mat = p.mat.500m, sig.level = 0.05,insig = "blank",method="number",number.cex=0.7)

data.500m.cleaned <- data.500m.cleaned[-grep("^Perimeter_km$",colnames(data.500m.cleaned))]
data.500m.cleaned <- data.500m.cleaned[-grep("SDI_LU12",colnames(data.500m.cleaned))] 
data.500m.cleaned <- data.500m.cleaned[-grep("^SDI_TYPE12_Anthro$",colnames(data.500m.cleaned))] 
data.500m.cleaned <- data.500m.cleaned[-grep("^Number_Houses$",colnames(data.500m.cleaned))] 
data.500m.cleaned <- data.500m.cleaned[-grep("^SDI_TYPE12_Nat$",colnames(data.500m.cleaned))] 
data.500m.cleaned <- data.500m.cleaned[-grep("^MEAN_PATCH$",colnames(data.500m.cleaned))] 

## Run Lasso

lambdas_to_try <- c(0,10^seq(-3, 2, length.out = 1000))
n <- 1000

y <- data.500m.cleaned %>% dplyr::select(Rare_Abundance) %>% as.matrix()
X <- data.500m.cleaned %>% dplyr::select(-Rare_Abundance) %>% as.matrix()

set.seed(123)
cl <- makeCluster(no_cores)
m.500m <- mclapply(rep(5, n), output.function.poisson,
                  mc.set.seed = T); stopCluster(cl)


# Get coefficients summary
coefficients.500m <- data.frame()
for(i in 1:n){
  coefficients.500m <- rbind(coefficients.500m,m.500m[[i]][[1]])
}

coef.500m.summ <- coefficients.500m %>% group_by(Variable) %>%
  summarize(Avg = mean(Value), SD = sd(Value), Min = min(Value), Max = max(Value),
            Conf.low = sd(Value) * (-1.96/sqrt(n)), Conf.high = sd(Value) * (1.96/sqrt(n)),
            Included = sum(Value != 0)) %>% arrange(desc(Included))

# Get adj R sq summary
rsq.500m <- c()
for(i in 1:n){
  rsq.500m[i] <- m.500m[[i]][[2]]
}
summary(rsq.500m)

# Get adj R sq summary
adj.rsq.500m <- c()
for(i in 1:n){
  adj.rsq.500m[i] <- m.500m[[i]][[3]]
}
summary(adj.rsq.500m)

# Get moran'i summary
i.p.500m <- c()
for(i in 1:n){
  i.p.500m[i] <- m.500m[[i]][[4]]
}
summary(i.p.500m)



#### 300m ####

## Get variables in order
data.300m <- abun[c(2,3)] %>% left_join(preds_300m, by = "Unit_Nm") %>% 
  left_join(effort[c(2,3)], by = "Unit_Nm") %>% left_join(sites[c(1,7)], by = "Unit_Nm") %>%
  dplyr::select(-Unit_Nm)

colnames(data.300m)[6:64] <- substr(colnames(data.300m[6:64]), 1, 3)
for(i in 6:63){
  
  for(j in (i+1):64){
    
    if(colnames(data.300m)[i] == colnames(data.300m)[j]){
      data.300m[j]<- data.300m[i] + data.300m[j]
      colnames(data.300m)[i] <- "delete"
    }
    
    else{
      next
    }
  }
}

data.300m <- data.300m[,-which(names(data.300m) %in% "delete")]

for(i in ncol(data.300m):2){ # take out variables where one / two nonzeros are driving relationship
  pred <- names(data.300m)[i]
  plot(Rare_Abundance ~ get(pred, data.300m), data = data.300m, xlab = pred)
} 

data.300m <- data.300m[-c(grep("X13",colnames(data.300m)), grep("X14",colnames(data.300m)),
                                grep("X17",colnames(data.300m)), 
                                grep("X22",colnames(data.300m)), 
                                grep("X23",colnames(data.300m)), 
                                grep("X24",colnames(data.300m)), 
                                grep("X42",colnames(data.300m)), 
                                grep("X51",colnames(data.300m)),
                                grep("X52",colnames(data.300m)), 
                                grep("X53",colnames(data.300m)),
                                grep("X54",colnames(data.300m)), 
                                grep("X71",colnames(data.300m)),
                                grep("X72",colnames(data.300m)), 
                                grep("X73",colnames(data.300m)),
                                grep("X74",colnames(data.300m)), 
                                grep("LDI",colnames(data.300m)), 
                                grep("Road_length_km",colnames(data.300m))
)]

## Take out correlated predictors

data.300m.cleaned <- data.300m

predictors.300m <- data.300m.cleaned[-1]
p.mat.300m <- cor.mtest(predictors.300m)
cor.300m <- cor(predictors.300m)
cor.300m[abs(cor.300m) < 0.85 ] = 0 
corrplot(cor.300m, type="lower",
         p.mat = p.mat.300m, sig.level = 0.05,insig = "blank",method="number",number.cex=0.7)


data.300m.cleaned <- data.300m.cleaned[-grep("^Perimeter_km$",colnames(data.300m.cleaned))]
data.300m.cleaned <- data.300m.cleaned[-grep("SDI_LU12",colnames(data.300m.cleaned))] 
data.300m.cleaned <- data.300m.cleaned[-grep("^SDI_TYPE12_Anthro$",colnames(data.300m.cleaned))] 
data.300m.cleaned <- data.300m.cleaned[-grep("^MEAN_PATCH$",colnames(data.300m.cleaned))] 
data.300m.cleaned <- data.300m.cleaned[-grep("^Number_Houses$",colnames(data.300m.cleaned))] 
data.300m.cleaned <- data.300m.cleaned[-grep("^SDI_TYPE12_Nat$",colnames(data.300m.cleaned))] 

## Run Lasso

lambdas_to_try <- c(0,10^seq(-3, 2, length.out = 1000))
n <- 1000

y <- data.300m.cleaned %>% dplyr::select(Rare_Abundance) %>% as.matrix()
X <- data.300m.cleaned %>% dplyr::select(-Rare_Abundance) %>% as.matrix()

set.seed(123)
cl <- makeCluster(no_cores)
m.300m <- mclapply(rep(5, n), output.function.poisson,
                   mc.set.seed = T); stopCluster(cl)


# Get coefficients summary
coefficients.300m <- data.frame()
for(i in 1:n){
  coefficients.300m <- rbind(coefficients.300m,m.300m[[i]][[1]])
}

coef.300m.summ <- coefficients.300m %>% group_by(Variable) %>%
  summarize(Avg = mean(Value), SD = sd(Value), Min = min(Value), Max = max(Value),
            Conf.low = sd(Value) * (-1.96/sqrt(n)), Conf.high = sd(Value) * (1.96/sqrt(n)),
            Included = sum(Value != 0)) %>% arrange(desc(Included))

# Get adj R sq summary
rsq.300m <- c()
for(i in 1:n){
  rsq.300m[i] <- m.300m[[i]][[2]]
}
summary(rsq.300m)

# Get adj R sq summary
adj.rsq.300m <- c()
for(i in 1:n){
  adj.rsq.300m[i] <- m.300m[[i]][[3]]
}
summary(adj.rsq.300m)

# Get moran'i summary
i.p.300m <- c()
for(i in 1:n){
  i.p.300m[i] <- m.300m[[i]][[4]]
}
summary(i.p.300m)

#### Save output ####

if(getwd() == "/Users/Zoe/Desktop/Bee_Project/Analysis1"){
  
  #saveRDS(m.1km, "Bee_Ecology_Models/abundance_1km.rds")
  #saveRDS(m.500m, "Bee_Ecology_Models/abundance_500m.rds")
  #saveRDS(m.300m, "Bee_Ecology_Models/abundance_300m.rds")
  
} else{
  
  saveRDS(m.1km, file = "/scratch/gpfs/zvolenec/abundance_1km.rds")
  saveRDS(m.500m, file = "/scratch/gpfs/zvolenec/abundance_500m.rds")
  saveRDS(m.300m, file = "/scratch/gpfs/zvolenec/abundance_300m.rds")
  
}














