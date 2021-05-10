### ---___---___---___---___---___---___---___---___---___---___---___---___---___---
### Gradient Boosting Machine (Frequency)
### ---___---___---___---___---___---___---___---___---___---___---___---___---___---
  # For GBMs it is easier to use continuous features, so age will be looked at
  # in it continuous for in stead of the made factors 

library(gbm)

### Preparing dataset 
Data_gbm <- Data[,c(1, 3:13, 16)]
names(Data_gbm)
str(Data_gbm)
dim(Data_gbm)[1] == dim(Data)[1] # Same number of observations 

gbm_train <- Data_gbm[trainIndex,]  # Use the same trainIndex as for GLM!!!
gbm_test <- Data_gbm[-trainIndex,]

head(gbm_train)
names(gbm_train)


### First, full GBM-model 
set.seed(1729)  # Needed because you work with random draws 
gbm_0 <- gbm(freq ~ offset(log(expo)) + ageph + geo + agecar + sexph + fuel # mind the offset for exposure !! 
             + split + use + fleet + sportc + cover + power,
             data = gbm_train,
             distribution = "poisson",
             n.trees = 1000,          # First try 500 -> insufficient, then 1000 -> insufficient for cv, then 1500 or 2000
             interaction.depth = 2,   # model up to 2-way interactions
             shrinkage = 0.01,        # 0.01 learning rate (low -> better performance, increase computation time)
             bag.fraction = 0.75,     # each step uses a subsample size delta*n
             cv.folds = 5,            # use 5- or 10-fold CV 
             verbose = F,             # do not print progress 
             keep.data = T,
             train.fraction = 1,      # use full training dataset 
             n.cores = 1,             # Number of CPU cores to use
             n.minobsinnode = 10000)  # minimum number of observations in the terminal nodes of the trees

# Tuning parameters: n.trees (T) and interaction.depth (d)
# Hyper Parameters: shrinkage (lambda = 0.01), bag.fraction (delta = 0.75) -> Values based on paper of Roel

gbm_0   # There were 11 predictors of which 8 had non-zero influence.
summary(gbm_0) # use, fleet, sportc do not have a non-zero influence, possibly because there are few observations of the 'other' case
    # ageph, split and geo are the most important ! 

plot(gbm_0$train.error, type = "l", main = "Training error per iteration",
     xlab = "Number of trees")

### Looking for best hyperparameters (max n.tree and cv.folds)

gbm_0$train.error[gbm_0$n.trees]
# interaction.depth = 2
  # CV = 5
  # n.tree = 500  -> 0.7515610
  # n.tree = 1000 -> 0.7505098  (!)
  # n.tree = 1500 -> 0.7502090
  # n.tree = 2000 -> 0.7500273

  # CV = 10
  # n.tree = 500  -> 0.7515611
  # n.tree = 1000 -> 0.7504996
  # n.tree = 1500 -> 0.7501954
  # n.tree = 2000 -> 0.7500185

# interaction.depth = 3
  # CV = 5
  # n.tree = 500  -> 0.7510068
  # n.tree = 1000 -> 0.7501817
  # n.tree = 1500 -> 0.7498289
  # n.tree = 2000 -> 0.7496027
  
  # CV = 10
  # n.tree = 500  -> 0.7510112
  # n.tree = 1000 -> 0.7501880
  # n.tree = 1500 -> 0.7498340
  # n.tree = 2000 -> 0.7496349

pretty.gbm.tree(gbm_0, i.tree = gbm_0$n.trees) # Gives the fitted tree of the last iteration


### Find optimal n.trees 
gbm_best_i_oob <- gbm.perf(gbm_0, method = "OOB", oobag.curve = TRUE, overlay = TRUE)
gbm_best_i_oob   # Warning message !
# interaction.depth = 2
  # CV = 5
  # n.tree = 500  -> 500
  # n.tree = 1000 -> 681 (!)
  # n.tree = 1500 -> 692
  # n.tree = 2000 -> 718
  
  # CV = 10
  # n.tree = 500  -> 500
  # n.tree = 1000 -> 658
  # n.tree = 1500 -> 672
  # n.tree = 2000 -> 676

# interaction.depth = 3
  # CV = 5
  # n.tree = 500  -> 500
  # n.tree = 1000 -> 553
  # n.tree = 1500 -> 559
  # n.tree = 2000 -> 560
  
  # CV = 10
  # n.tree = 500  -> 500
  # n.tree = 1000 -> 538
  # n.tree = 1500 -> 550
  # n.tree = 2000 -> 564

gbm_best_i_cv <- gbm.perf(gbm_0, method = "cv")
gbm_best_i_cv 
  # CV = 5
  # n.tree = 500  -> 500
  # n.tree = 1000 -> 1000
  # n.tree = 1500 -> 1479
  # n.tree = 2000 -> 2000   

  # CV = 10
  # n.tree = 500  -> 500
  # n.tree = 1000 -> 1000
  # n.tree = 1500 -> 1500
  # n.tree = 2000 -> 1997

# interaction.depth = 3
  # CV = 5
  # n.tree = 500  -> 500
  # n.tree = 1000 -> 1000
  # n.tree = 1500 -> 1500
  # n.tree = 2000 -> 1922
  
  # CV = 10
  # n.tree = 500  -> 500
  # n.tree = 1000 -> 1000
  # n.tree = 1500 -> 1500
  # n.tree = 2000 -> 1960

# use max n.trees = 1000, with optimal n.trees = 681 and 5fold cv

n.trees_opt <- 681    # interaction.depth = 2, n.trees = 1000, CV = 5


##### Partial Dependence Plots (PDP)
plot(gbm_0, 1, n.trees_opt)
plot(gbm_0, 2, n.trees_opt)
plot(gbm_0, 3, n.trees_opt)
plot(gbm_0, 4, n.trees_opt)
plot(gbm_0, 5, n.trees_opt)
plot(gbm_0, 6, n.trees_opt)
plot(gbm_0, 7, n.trees_opt)  # Barely any difference, insignificant USE
plot(gbm_0, 8, n.trees_opt)  # Barely any difference, insignificant FLEET
plot(gbm_0, 9, n.trees_opt)  # Barely any difference, insignificant SPORTC
plot(gbm_0, 10, n.trees_opt)
plot(gbm_0, 11, n.trees_opt)

plot(gbm_0, 1, n.trees_opt, type = "response")
plot(gbm_0, 2, n.trees_opt, type = "response")
plot(gbm_0, 3, n.trees_opt, type = "response")
plot(gbm_0, 4, n.trees_opt, type = "response")
plot(gbm_0, 5, n.trees_opt, type = "response")
plot(gbm_0, 6, n.trees_opt, type = "response")
plot(gbm_0, 7, n.trees_opt, type = "response")  # Barely any difference, insignificant USE
plot(gbm_0, 8, n.trees_opt, type = "response")  # Barely any difference, insignificant FLEET
plot(gbm_0, 9, n.trees_opt, type = "response")  # Barely any difference, insignificant SPORTC
plot(gbm_0, 10, n.trees_opt, type = "response")
plot(gbm_0, 11, n.trees_opt, type = "response")

gbm_perf <- gbm(freq ~ offset(log(expo)) + ageph + geo + agecar + sexph + fuel 
             + split + use + fleet + sportc + cover + power,
             data = gbm_train,
             distribution = "poisson",
             n.trees = n.trees_opt,   # Use optimal number of trees
             interaction.depth = 2,   # Use optimal interaction depth
             shrinkage = 0.01,        
             bag.fraction = 0.75,     
             cv.folds = 5,            # Use 5-fold, this is sufficient, 10 only has a marginal increase
             verbose = F,             
             keep.data = T,
             train.fraction = 1,      
             n.cores = 1,             
             n.minobsinnode = 10000)  

pred_GBM_train <- predict(gbm_perf, gbm_train, type = "response")*gbm_train$expo

GBM_comp_train <- as.data.frame(cbind(gbm_train$freq,pred_GBM_train))
GBM_comp_train[,3] <- (GBM_comp_train[,1] - GBM_comp_train[,2])^2

MSE_GBM_train <- sum(GBM_comp_train[,3])/length(GBM_comp_train[,3])
MSE_GBM_train   # 0.133352850489098


### ---___---___---___---___---___---___---___---___---___---___---___---___---___---
### Predict based on test-data (Frequency)
### ---___---___---___---___---___---___---___---___---___---___---___---___---___---
pred_GBM <- predict(gbm_perf, gbm_test, type = "response")*gbm_test$expo

GBM_comp <- as.data.frame(cbind(gbm_test$freq,pred_GBM))
GBM_comp[,3] <- (GBM_comp[,1] - GBM_comp[,2])^2

MSE_GBM_test <- sum(GBM_comp[,3])/length(GBM_comp[,3])
MSE_GBM_test   # 0.132005928946107


