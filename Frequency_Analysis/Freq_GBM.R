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
             n.trees = 1000,          # First try 500 -> insufficient, then 1000 -> insufficient for cv, then 1500
             interaction.depth = 2,   # model up to 2-way interactions
             shrinkage = 0.01,        # 0.01 learning rate (low -> better performance, increase computation time)
             bag.fraction = 0.75,     # each step uses a subsample size delta*n
             cv.folds = 5,            # use 5-fold CV 
             verbose = F,             # do not print progress 
             keep.data = T,
             train.fraction = 1,      # use full training dataset 
             n.cores = 1,             # Number of CPU cores to use
             n.minobsinnode = 10000)  # minimum number of observations in the terminal nodes of the trees

# Tuning parameters: n.trees (T) and interaction.depth (d)
# Hyper Parameters: shrinkage (lambda = 0.01), bag.fraction (delta = 0.75) -> Values based on paper of Roel

gbm_0   # There were 11 predictors of which 8 had non-zero influence.
summary(gbm_0) # use, fleet, sportc do not have a non-zero influence 
    # ageph, split and geo are the most important ! 

plot(gbm_0$train.error, type = "l", main = "Training error per iteration",
     xlab = "Number of trees")

### Looking for best hyperparameters (max n.tree and cv.folds)

gbm_0$train.error[gbm_0$n.trees]
  # CV = 5
  # n.tree = 500  -> 0.7515610
  # n.tree = 1000 -> 0.7505098
  # n.tree = 1500 -> 0.7502090

  # CV = 10
  # n.tree = 500  -> 0.7515611
  # n.tree = 1000 -> 0.7504996
  # n.tree = 1500 -> 0.7501954

pretty.gbm.tree(gbm_0, i.tree = gbm_0$n.trees) # Gives the fitted tree of the last iteration


### Find optimal n.trees 
gbm_best_i_oob <- gbm.perf(gbm_0, method = "OOB", oobag.curve = TRUE, overlay = TRUE)
gbm_best_i_oob 
  # CV = 5
  # n.tree = 500  -> 500
  # n.tree = 1000 -> 681 (!)
  # n.tree = 1500 -> 692
  
  # CV = 10
  # n.tree = 500  -> 500
  # n.tree = 1000 -> 658
  # n.tree = 1500 -> 672

gbm_best_i_cv <- gbm.perf(gbm_0, method = "cv")
gbm_best_i_cv 
  # CV = 5
  # n.tree = 500  -> 500
  # n.tree = 1000 -> 1000
  # n.tree = 1500 -> 1479
  
  # CV = 10
  # n.tree = 500  -> 500
  # n.tree = 1000 -> 1000
  # n.tree = 1500 -> 1500


# use max n.trees = 1000, with optimal n.trees = 681 and 5fold cv






































