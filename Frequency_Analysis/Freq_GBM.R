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
             n.trees = 500,
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
# Hyperparameters: shrinkage (lambda = 0.01), bag.fraction (delta = 0.75) -> Values based on paper of Roel



















































