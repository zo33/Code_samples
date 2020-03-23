
#############
## Visitation prediction model 
#############
# ML prediction model using Flickr image upload data to build and test a relaxed
# LASSO model predicting visitor use to NJ open space areas

## Set up environment - packages, functions, reproducibility

rm(list =ls())
RNGkind(kind = "L'Ecuyer-CMRG", sample.kind = "Rounding")

library(readr)
library(readxl)
library(plyr)
library(dplyr)
library(leaps)
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
library(broom)
library(elasticnet)
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

lambdas.function <- function(alpha_value, cv_sizes){
  fit <- cv.glmnet(X,y, alpha = alpha_value, family = "poisson", standardize = TRUE, lambda = lambdas_to_try, nfolds = cv_sizes)
  errors = data.frame(fit$lambda, fit$cvm, cv_sizes)
  return(errors)
}

## Load data
setwd("~/Google_Princeton/NJ_Flickr_Project/Analyses/Model_data/")

# Used the landscape variables calculated at 2km 
# Did a separate sensitivity analyses comparing RMSE across 1km,2km,5km,10km prediction models
buffer <- "two"

if(buffer == "one"){
  model_data <- readRDS("PUD_data_1km.rds")
  data.v1 <- model_data[c(2,8:25,31:32)]
} else if(buffer == "two"){
  model_data <- readRDS("PUD_data_2km.rds")
  data.v1 <- model_data[c(2,8:25,27:28)]
} else if(buffer == "five"){
  model_data <- readRDS("PUD_data_5km.rds")
  data.v1 <- model_data[c(2,8:25,27:28)]
} else{
  model_data <- readRDS("PUD_data_10km.rds")
  data.v1 <- model_data[c(2,8:25,27:28)]
}

#### PUD PREDICTION MODEL ####

  ## Prep & plot data

  data.v1 <- data.v1[-c(2,15),] #TAKE OUT D&R and assunpink wma
  data.v1$TOT_POP <- as.numeric(data.v1$TOT_POP)
  data.v1$HSE_UNITS <- as.numeric(data.v1$HSE_UNITS)
  data.v1$FAMILIES <- as.numeric(data.v1$FAMILIES)
  
  plot(density(data.v1$PUD)); shapiro.test(data.v1$PUD)
  
  for(i in ncol(data.v1):2){
    pred <- names(data.v1)[i]
    plot(PUD ~ get(pred, data.v1), data = data.v1, xlab = pred)
  }
  

## Get rid of correlated predictors

  data.v1.combined <- data.v1

  predictors <- data.v1.combined[-1]
  p.mat <- cor.mtest(predictors)
  cor1 <- cor(predictors)
  cor1[ abs(cor1) < 0.85 ] = 0 
  corrplot(cor1, type="lower",
         p.mat = p.mat, sig.level = 0.05,insig = "blank",method="number",number.cex=0.7)
  
  if(buffer == "one"){
    
    data.v1.combined <- data.v1.combined[-grep("^FAMILIES$",colnames(data.v1.combined))]
    data.v1.combined <- data.v1.combined[-grep("^HSE_UNITS$",colnames(data.v1.combined))]
    
  } else if(buffer == "two"){
   
    data.v1.combined <- data.v1.combined[-grep("^FAMILIES$",colnames(data.v1.combined))]
    data.v1.combined <- data.v1.combined[-grep("^HSE_UNITS$",colnames(data.v1.combined))]
  
    
  } else if(buffer == "five"){
      
    data.v1.combined <- data.v1.combined[-grep("^FAMILIES$",colnames(data.v1.combined))]
    data.v1.combined <- data.v1.combined[-grep("^HSE_UNITS$",colnames(data.v1.combined))]
    data.v1.combined <- data.v1.combined[-grep("^URBAN$",colnames(data.v1.combined))]
    data.v1.combined <- data.v1.combined[-grep("^Road_length_m$",colnames(data.v1.combined))]
    
  } else{
    
    data.v1.combined <- data.v1.combined[-grep("^FAMILIES$",colnames(data.v1.combined))]
    data.v1.combined <- data.v1.combined[-grep("^HSE_UNITS$",colnames(data.v1.combined))]
    data.v1.combined <- data.v1.combined[-grep("^URBAN$",colnames(data.v1.combined))]
    data.v1.combined <- data.v1.combined[-grep("^Road_length_m$",colnames(data.v1.combined))]
    data.v1.combined <- data.v1.combined[-grep("^PROPORTION_HISPANIC$",colnames(data.v1.combined))]
    data.v1.combined <- data.v1.combined[-grep("^FOREST$",colnames(data.v1.combined))]
    data.v1.combined <- data.v1.combined[-grep("BARREN.LAND",colnames(data.v1.combined))]
    
 
  }
  
  

## Prep train /tests split
  RNGkind(sample.kind = "Rounding")
  set.seed(123)
  trainIndex <- createDataPartition(data.v1.combined$PUD, p = .8, 
                    list = FALSE, 
                    times = 1)
  
  data.v1.train <- data.v1.combined[trainIndex,]
  data.v1.test <- data.v1.combined[-trainIndex,]

  y <- data.v1.train %>% dplyr::select(PUD) %>% as.matrix()
  X <- data.v1.train %>% dplyr::select(-PUD) %>% as.matrix()

  y.test.v1 <- data.v1.test %>% dplyr::select(PUD) %>% as.matrix()
  X.test.v1 <- data.v1.test %>% dplyr::select(-PUD) %>% as.matrix()


## Lasso
  
  lambdas_to_try <- c(0,10^seq(-3, 1, length.out = 500))
  n <- 1000 # bump up later

  lambdas.lasso.v1 <- NULL
  cl <- makeCluster(no_cores)
  clusterSetRNGStream(cl, iseed = 123)
  lambdas.lasso.v1 <- do.call(rbind,mcmapply(lambdas.function, alpha_value = 1, cv_sizes = rep(10, each = n), SIMPLIFY = F,
                                           mc.set.seed = T)); stopCluster(cl)
  lambdas.agg.lasso.v1 <- lambdas.lasso.v1 %>% group_by(fit.lambda) %>% 
    summarize(avg_cmv = mean(fit.cvm))
  
  # Lambda min
  bestindex.lasso.v1 = which(lambdas.agg.lasso.v1[2]==min(lambdas.agg.lasso.v1[2])) # select the best one
  lambda.lasso.v1 = as.numeric(min(lambdas.agg.lasso.v1[bestindex.lasso.v1,1]))
  
  # Lambda se
  bestindex.lasso.v1 = which(lambdas.agg.lasso.v1[2]==min(lambdas.agg.lasso.v1[2])) 
  error.lasso.v1 = as.numeric(lambdas.agg.lasso.v1[bestindex.lasso.v1,2])
  se.error.lasso.v1= sapply(lambdas.agg.lasso.v1[,2],sd) / sqrt(nrow(lambdas.agg.lasso.v1))
  max.v1= error.lasso.v1 +  se.error.lasso.v1
  min.v1 =  error.lasso.v1 -  se.error.lasso.v1
  lambdas.within.one.se.v1 = subset(lambdas.agg.lasso.v1, avg_cmv > min.v1 & avg_cmv < max.v1)
  lambda.lasso.v1.se = max(lambdas.within.one.se.v1$fit.lambda)
  
  mod.lasso.v1 <- glmnet(X, y, alpha = 1, family = "poisson",lambda = lambda.lasso.v1, standardize = TRUE) # run model with best lambda
  
## Relaxed Lasso
    
  coef.relaxed.lasso.m1 <- as.data.frame(as.matrix(coef(mod.lasso.v1)))
  retain.coef.relaxed.lasso.v1<- row.names(subset(coef.relaxed.lasso.m1, coef.relaxed.lasso.m1$s0 != 0))
    
  X <- data.v1.train[names(data.v1.train) %in% retain.coef.relaxed.lasso.v1] %>% as.matrix()
    
  lambdas.relaxed.lasso.v1 <- NULL
  cl <- makeCluster(no_cores)
  clusterSetRNGStream(cl, iseed = 123)
  lambdas.relaxed.lasso.v1 <- do.call(rbind,mcmapply(lambdas.function, alpha_value = 1, cv_sizes = rep(10, each = n), SIMPLIFY = F,
                                                       mc.set.seed = T)); stopCluster(cl)
  lambdas.agg.relaxed.lasso.v1 <- lambdas.relaxed.lasso.v1 %>% group_by(fit.lambda) %>% 
    summarize(avg_cmv = mean(fit.cvm)) # take mean cvm for each lambda
  
  # Lambda min
  bestindex.relaxed.lasso.v1 = which(lambdas.agg.relaxed.lasso.v1[2]==min(lambdas.agg.relaxed.lasso.v1[2])) # s$
  lambda.relaxed.lasso.v1 = as.numeric(min(lambdas.agg.relaxed.lasso.v1[bestindex.relaxed.lasso.v1,1]))
    
  # Lambda se
  bestindex.relaxed.lasso.v1 = which(lambdas.agg.relaxed.lasso.v1[2]==min(lambdas.agg.relaxed.lasso.v1[2])) # select the best one
  error.relaxed.lasso.v1 = as.numeric(min(lambdas.agg.relaxed.lasso.v1[bestindex.relaxed.lasso.v1,2]))
  se.error.relaxed.lasso.v1 = sapply(lambdas.agg.relaxed.lasso.v1[,2],sd) / sqrt(nrow(lambdas.agg.relaxed.lasso.v1))
  max.m3.500m.relaxed = error.relaxed.lasso.v1 +  se.error.relaxed.lasso.v1
  min.m3.500m.relaxed =  error.relaxed.lasso.v1 -  se.error.relaxed.lasso.v1
  lambdas.within.one.se.m3.500m.relaxed = subset(lambdas.agg.relaxed.lasso.v1, avg_cmv > min.m3.500m.relaxed & avg_cmv < max.m3.500m.relaxed)
  lambda.relaxed.lasso.v1.se = max(lambdas.within.one.se.m3.500m.relaxed$fit.lambda)
    
  mod.relaxed.lasso.v1 <- glmnet(X, y, alpha = 1, lambda = lambda.relaxed.lasso.v1, standardize = TRUE) # run model with best lambda
  coef(mod.relaxed.lasso.v1)
    
  X.test.v1 <- data.v1.test[names(data.v1.test) %in% retain.coef.relaxed.lasso.v1] %>% as.matrix()
  yhat.relaxed.lasso.v1 <- predict(mod.relaxed.lasso.v1, X.test.v1, type = "response")
  plot(yhat.relaxed.lasso.v1 ~ y.test.v1, xlim= c(0,45))
    
  # test data RMSE
  RMSE.relaxed.lasso.v1 <- sqrt(mean((yhat.relaxed.lasso.v1 - y.test.v1)^2))
    
  # train data RMSE 
  RMSE.train.relaxed.lasso.v1 <- sqrt(mean((predict(mod.relaxed.lasso.v1, X, type = "response") - y)^2))
    
    
  ## Spatial autocorrelation test
    
    # for test data
    test.sites <- model_data[-c(13,56),]
    test.sites <- test.sites[-trainIndex,]
    test.site.dists <- as.matrix(dist(cbind(test.sites$Longitude, test.sites$Latitude),
                                      method = "euclidean"))
    test.site.dists.inv <- 1/test.site.dists
    diag(test.site.dists.inv) <- 0
    
    residuals.test.relaxed.lasso.v1 <- as.vector(y.test.v1 - yhat.relaxed.lasso.v1)
    Moran.I(residuals.test.relaxed.lasso.v1, test.site.dists.inv, alternative = "greater")
    
    # for train data
    
    train.sites <- model_data[-c(15,36),]
    train.sites <- train.sites[trainIndex,]
    train.site.dists <- as.matrix(dist(cbind(train.sites$Longitude, train.sites$Latitude),
                                       method = "euclidean"))
    train.site.dists.inv <- 1/train.site.dists
    diag(train.site.dists.inv) <- 0
    residuals.train.relaxed.lasso.v1 <- as.vector(y - predict(mod.relaxed.lasso.v1, X, type = "response"))
    Moran.I(residuals.train.relaxed.lasso.v1, train.site.dists.inv, alternative = "greater")
  
#### Graph ####
    
 # https://www.scikit-yb.org/en/latest/api/regressor/peplot.html   
    
  plot_data <- as.data.frame(cbind(y.test.v1, yhat.relaxed.lasso.v1))
  colnames(plot_data) <- c("Observed", "Predicted")    

  best_fit <- summary(lm(plot_data$Predicted ~ plot_data$Observed))

  ggplot(data = plot_data, aes(x = Observed, y = Predicted)) +
    geom_point(size = 2, col = "steelblue") + theme_bw() +
    #geom_abline(slope = 1, intercept = 0, color = "darkgray", linetype = "dashed")+
    #geom_abline(slope =  best_fit$coefficients[2,1], intercept =  best_fit$coefficients[1,1],
          #      color = "black", linetype = "dashed")+ xlim(0,45)+
    stat_smooth(method = "lm", formula = y ~ x + I(x^2))+
    xlab("Test data PUD")+ ylab("Test data PUD predictions")+
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14))#+
    #annotate("text", x = 30, y = 31.5, angle = 53, label = "Identity", col = "darkgray", size = 5)+
    #annotate("text", x = 22.5, y = 37.3, angle = 54, label = "Best fit", col = "black", size = 5)
    
  
  
  
      
  