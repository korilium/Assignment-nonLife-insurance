### ---___---___---___---___---___---___---___---___---___---___---___---___---___---
### Generalized Linear Models (Frequency)
### ---___---___---___---___---___---___---___---___---___---___---___---___---___---

glm_opt$formula
summary(glm_opt)
anova(glm0, glm_opt, test = "Chisq")

# For claim frequency, suitable distributions are: Poisson and/or Negative Binomial.
# In this study, there will be opted to use the Poisson distribution.
# Dataset: original dataset with age and spatial binned in previous code -> use data_train

head(model_train)
str(model_train)
summary(model_train)
names(model_train) 
  # "expo"    "freq"    "agecar"  "sexph"   "fuel"    "split"   "use"     "fleet"  
  # "sportc"  "cover"   "power"   "geo"     "agephGR"

### Making a new dendogram 
library(ClustOfVar)
library(PCAmixdata)

Q <- model_train
Q$agecar <- factor(Q$agecar, ordered = F)
Q$split <- factor(Q$split, ordered = F)
Q$power <- factor(Q$power, ordered = F)
Q$geo <- factor(Q$geo, ordered = F)
Q$agephGR <- factor(Q$agephGR, ordered = F)
# Q = model_train, but without ordered factors, splitmix() does not work with ordered factors  

Data_group_new <- splitmix(Q)   
Data_quanti_new <- model_train[Data_group$col.quant]
Data_quali_new <- model_train[Data_group$col.qual]

tree <- hclustvar(X.quanti = Data_quanti_new, X.quali = Data_quali_new)
g_dendogram <- plot(tree)

tot_degr_free <- 4*2*2*4*2*2*2*3*3*11*11
  
  
# #### JUST SOME TESTING
# # without interactions
# summary(glm(freq ~ agephGR + geo + power + cover + sportc + fleet + use + split + fuel + sexph + agecar,
#     data = model_train, family = poisson, offset = log(expo)))
# 
# # with interaction (based on Dendogram)
# summary(glm(freq ~ agephGR + geo + power + cover + sportc + fleet + use + split + fuel + sexph + agecar
#             + agephGR:split + sportc:power + agecar:cover + use:fleet,
#             data = model_train, family = poisson, offset = log(expo)))
# 
# 
# # only intercept 
# glm(freq ~ 1, data = model_train, family = poisson, offset = log(expo))


### theory 
# DELTA deviance < cv → validate null hypothesis, do NOT add extra covariate
# DELTA deviance > cv → reject null hypothesis, DO add extra covariate

vars <- c('agephGR', 'geo', 'power', 'cover', 'sportc', 'fleet', 'use', 'split', 'fuel', 'sexph', 
          'agecar', 'agephGR:split', 'sportc:power', 'agecar:cover', 'use:fleet')

glm0 <- glm(freq ~ 1, data = model_train, family = poisson, offset = log(expo))

glm1.1 <- glm(freq ~ 1 + agephGR, data = model_train, family = poisson, offset = log(expo))
glm1.2 <- glm(freq ~ 1 + geo, data = model_train, family = poisson, offset = log(expo))
glm1.3 <- glm(freq ~ 1 + power, data = model_train, family = poisson, offset = log(expo))
glm1.4 <- glm(freq ~ 1 + cover, data = model_train, family = poisson, offset = log(expo))
glm1.5 <- glm(freq ~ 1 + sportc, data = model_train, family = poisson, offset = log(expo))
glm1.6 <- glm(freq ~ 1 + fleet, data = model_train, family = poisson, offset = log(expo))
glm1.7 <- glm(freq ~ 1 + use, data = model_train, family = poisson, offset = log(expo))
glm1.8 <- glm(freq ~ 1 + split, data = model_train, family = poisson, offset = log(expo))
glm1.9 <- glm(freq ~ 1 + fuel, data = model_train, family = poisson, offset = log(expo))
glm1.10 <- glm(freq ~ 1 + sexph, data = model_train, family = poisson, offset = log(expo))
glm1.11 <- glm(freq ~ 1 + agecar, data = model_train, family = poisson, offset = log(expo))

anova(glm0, glm1.1, test = "Chisq")   # (!)
anova(glm0, glm1.2, test = "Chisq")
anova(glm0, glm1.3, test = "Chisq")
anova(glm0, glm1.4, test = "Chisq")
anova(glm0, glm1.5, test = "Chisq")
anova(glm0, glm1.6, test = "Chisq")
anova(glm0, glm1.7, test = "Chisq")
anova(glm0, glm1.8, test = "Chisq")
anova(glm0, glm1.9, test = "Chisq")
anova(glm0, glm1.10, test = "Chisq")
anova(glm0, glm1.11, test = "Chisq")

# Critical values 
qchisq(0.95,10) # 18.30704
qchisq(0.95,3)  # 7.814728
qchisq(0.95,2)  # 5.991465
qchisq(0.95,1)  # 3.841459

glm2.1 <- glm(freq ~ 1 + agephGR + geo, data = model_train, family = poisson, offset = log(expo))
glm2.2 <- glm(freq ~ 1 + agephGR + power, data = model_train, family = poisson, offset = log(expo))
glm2.3 <- glm(freq ~ 1 + agephGR + cover, data = model_train, family = poisson, offset = log(expo))
glm2.4 <- glm(freq ~ 1 + agephGR + sportc, data = model_train, family = poisson, offset = log(expo))
glm2.5 <- glm(freq ~ 1 + agephGR + fleet, data = model_train, family = poisson, offset = log(expo))
glm2.6 <- glm(freq ~ 1 + agephGR + use, data = model_train, family = poisson, offset = log(expo))
glm2.7 <- glm(freq ~ 1 + agephGR + split, data = model_train, family = poisson, offset = log(expo))
glm2.8 <- glm(freq ~ 1 + agephGR + fuel, data = model_train, family = poisson, offset = log(expo))
glm2.9 <- glm(freq ~ 1 + agephGR + sexph, data = model_train, family = poisson, offset = log(expo))
glm2.10 <- glm(freq ~ 1 + agephGR + agecar, data = model_train, family = poisson, offset = log(expo))

anova(glm1.1, glm2.1, test = "Chisq") # (!)
anova(glm1.1, glm2.2, test = "Chisq")
anova(glm1.1, glm2.3, test = "Chisq")
anova(glm1.1, glm2.4, test = "Chisq")
anova(glm1.1, glm2.5, test = "Chisq")
anova(glm1.1, glm2.6, test = "Chisq")
anova(glm1.1, glm2.7, test = "Chisq")
anova(glm1.1, glm2.8, test = "Chisq")
anova(glm1.1, glm2.9, test = "Chisq")
anova(glm1.1, glm2.10, test = "Chisq")

glm3.1 <- glm(freq ~ 1 + agephGR + geo + power, data = model_train, family = poisson, offset = log(expo))
glm3.2 <- glm(freq ~ 1 + agephGR + geo + cover, data = model_train, family = poisson, offset = log(expo))
glm3.3 <- glm(freq ~ 1 + agephGR + geo + sportc, data = model_train, family = poisson, offset = log(expo))
glm3.4 <- glm(freq ~ 1 + agephGR + geo + fleet, data = model_train, family = poisson, offset = log(expo))
glm3.5 <- glm(freq ~ 1 + agephGR + geo + use, data = model_train, family = poisson, offset = log(expo))
glm3.6 <- glm(freq ~ 1 + agephGR + geo + split, data = model_train, family = poisson, offset = log(expo))
glm3.7 <- glm(freq ~ 1 + agephGR + geo + fuel, data = model_train, family = poisson, offset = log(expo))
glm3.8 <- glm(freq ~ 1 + agephGR + geo + sexph, data = model_train, family = poisson, offset = log(expo))
glm3.9 <- glm(freq ~ 1 + agephGR + geo + agecar, data = model_train, family = poisson, offset = log(expo))

anova(glm2.1, glm3.1, test = "Chisq")
anova(glm2.1, glm3.2, test = "Chisq")
anova(glm2.1, glm3.3, test = "Chisq")
anova(glm2.1, glm3.4, test = "Chisq")
anova(glm2.1, glm3.5, test = "Chisq")
anova(glm2.1, glm3.6, test = "Chisq") # (!)
anova(glm2.1, glm3.7, test = "Chisq")
anova(glm2.1, glm3.8, test = "Chisq")
anova(glm2.1, glm3.9, test = "Chisq")

glm4.1 <- glm(freq ~ 1 + agephGR + geo + split + power, data = model_train, family = poisson, offset = log(expo))
glm4.2 <- glm(freq ~ 1 + agephGR + geo + split + cover, data = model_train, family = poisson, offset = log(expo))
glm4.3 <- glm(freq ~ 1 + agephGR + geo + split + sportc, data = model_train, family = poisson, offset = log(expo))
glm4.4 <- glm(freq ~ 1 + agephGR + geo + split + fleet, data = model_train, family = poisson, offset = log(expo))
glm4.5 <- glm(freq ~ 1 + agephGR + geo + split + use, data = model_train, family = poisson, offset = log(expo))
glm4.6 <- glm(freq ~ 1 + agephGR + geo + split + fuel, data = model_train, family = poisson, offset = log(expo))
glm4.7 <- glm(freq ~ 1 + agephGR + geo + split + sexph, data = model_train, family = poisson, offset = log(expo))
glm4.8 <- glm(freq ~ 1 + agephGR + geo + split + agecar, data = model_train, family = poisson, offset = log(expo))

anova(glm3.6, glm4.1, test = "Chisq")
anova(glm3.6, glm4.2, test = "Chisq")
anova(glm3.6, glm4.3, test = "Chisq")
anova(glm3.6, glm4.4, test = "Chisq")
anova(glm3.6, glm4.5, test = "Chisq")
anova(glm3.6, glm4.6, test = "Chisq") # (!)
anova(glm3.6, glm4.7, test = "Chisq")
anova(glm3.6, glm4.8, test = "Chisq")

glm5.1 <- glm(freq ~ 1 + agephGR + geo + split + fuel + power, data = model_train, family = poisson, offset = log(expo))
glm5.2 <- glm(freq ~ 1 + agephGR + geo + split + fuel + cover, data = model_train, family = poisson, offset = log(expo))
glm5.3 <- glm(freq ~ 1 + agephGR + geo + split + fuel + sportc, data = model_train, family = poisson, offset = log(expo))
glm5.4 <- glm(freq ~ 1 + agephGR + geo + split + fuel + fleet, data = model_train, family = poisson, offset = log(expo))
glm5.5 <- glm(freq ~ 1 + agephGR + geo + split + fuel + use, data = model_train, family = poisson, offset = log(expo))
glm5.6 <- glm(freq ~ 1 + agephGR + geo + split + fuel + sexph, data = model_train, family = poisson, offset = log(expo))
glm5.7 <- glm(freq ~ 1 + agephGR + geo + split + fuel + agecar, data = model_train, family = poisson, offset = log(expo))

anova(glm4.6, glm5.1, test = "Chisq")
anova(glm4.6, glm5.2, test = "Chisq") # (!)
anova(glm4.6, glm5.3, test = "Chisq")
anova(glm4.6, glm5.4, test = "Chisq")
anova(glm4.6, glm5.5, test = "Chisq")
anova(glm4.6, glm5.6, test = "Chisq")
anova(glm4.6, glm5.7, test = "Chisq")

glm6.1 <- glm(freq ~ 1 + agephGR + geo + split + fuel + cover + power, data = model_train, family = poisson, offset = log(expo))
glm6.2 <- glm(freq ~ 1 + agephGR + geo + split + fuel + cover + sportc, data = model_train, family = poisson, offset = log(expo))
glm6.3 <- glm(freq ~ 1 + agephGR + geo + split + fuel + cover + fleet, data = model_train, family = poisson, offset = log(expo))
glm6.4 <- glm(freq ~ 1 + agephGR + geo + split + fuel + cover + use, data = model_train, family = poisson, offset = log(expo))
glm6.5 <- glm(freq ~ 1 + agephGR + geo + split + fuel + cover + sexph, data = model_train, family = poisson, offset = log(expo))
glm6.6 <- glm(freq ~ 1 + agephGR + geo + split + fuel + cover + agecar, data = model_train, family = poisson, offset = log(expo))

anova(glm5.2, glm6.1, test = "Chisq") # (!)
anova(glm5.2, glm6.2, test = "Chisq")
anova(glm5.2, glm6.3, test = "Chisq")
anova(glm5.2, glm6.4, test = "Chisq")
anova(glm5.2, glm6.5, test = "Chisq")
anova(glm5.2, glm6.6, test = "Chisq")

glm7.1 <- glm(freq ~ 1 + agephGR + geo + split + fuel + cover + power + sportc, data = model_train, family = poisson, offset = log(expo))
glm7.2 <- glm(freq ~ 1 + agephGR + geo + split + fuel + cover + power + fleet, data = model_train, family = poisson, offset = log(expo))
glm7.3 <- glm(freq ~ 1 + agephGR + geo + split + fuel + cover + power + use, data = model_train, family = poisson, offset = log(expo))
glm7.4 <- glm(freq ~ 1 + agephGR + geo + split + fuel + cover + power + sexph, data = model_train, family = poisson, offset = log(expo))
glm7.5 <- glm(freq ~ 1 + agephGR + geo + split + fuel + cover + power + agecar, data = model_train, family = poisson, offset = log(expo))

anova(glm6.1, glm7.1, test = "Chisq")
anova(glm6.1, glm7.2, test = "Chisq")
anova(glm6.1, glm7.3, test = "Chisq")
anova(glm6.1, glm7.4, test = "Chisq")
anova(glm6.1, glm7.5, test = "Chisq") # (!)

glm8.1 <- glm(freq ~ 1 + agephGR + geo + split + fuel + cover + power + agecar + sportc, data = model_train, family = poisson, offset = log(expo))
glm8.2 <- glm(freq ~ 1 + agephGR + geo + split + fuel + cover + power + agecar + fleet, data = model_train, family = poisson, offset = log(expo))
glm8.3 <- glm(freq ~ 1 + agephGR + geo + split + fuel + cover + power + agecar + use, data = model_train, family = poisson, offset = log(expo))
glm8.4 <- glm(freq ~ 1 + agephGR + geo + split + fuel + cover + power + agecar + sexph, data = model_train, family = poisson, offset = log(expo))

anova(glm7.5, glm8.1, test = "Chisq")
anova(glm7.5, glm8.2, test = "Chisq")
anova(glm7.5, glm8.3, test = "Chisq")    # No extra terms found
anova(glm7.5, glm8.4, test = "Chisq")


# Adding interaction terms to see whether they add value to model
glm8.5 <- glm(freq ~ 1 + agephGR + geo + split + fuel + cover + power + agecar + agephGR:split, data = model_train, family = poisson, offset = log(expo))
glm8.6 <- glm(freq ~ 1 + agephGR + geo + split + fuel + cover + power + agecar + sportc:power, data = model_train, family = poisson, offset = log(expo))
glm8.7 <- glm(freq ~ 1 + agephGR + geo + split + fuel + cover + power + agecar + agecar:cover, data = model_train, family = poisson, offset = log(expo))
glm8.8 <- glm(freq ~ 1 + agephGR + geo + split + fuel + cover + power + agecar + use:fleet, data = model_train, family = poisson, offset = log(expo))

anova(glm7.5, glm8.5, test = "Chisq")
anova(glm7.5, glm8.6, test = "Chisq")
anova(glm7.5, glm8.7, test = "Chisq") # (!)
anova(glm7.5, glm8.8, test = "Chisq")


glm_opt <- glm8.7


summary(glm_opt)
anova(glm0, glm_opt, test = "Chisq") # ***, p-value: < 2.2e-16

### Results are summarized in the added Excel-file for convenience sake 
# Model summary
summ(glm_opt)


### Trying to predict the number of claims of the training set 
pred_GLM_train <- predict.glm(glm_opt, model_train, type = "response")
pred_GLM_train

GLM_comp_train <- as.data.frame(cbind(model_train$freq,pred_GLM_train))
GLM_comp_train[,3] <- (GLM_comp_train[,1] - GLM_comp_train[,2])^2

MSE_GLM_train <- sum(GLM_comp_train[,3])/length(GLM_comp_train[,3])
MSE_GLM_train   # 0.133131659333354

### ---___---___---___---___---___---___---___---___---___---___---___---___---___---
### Predict based on test-data (Frequency)
### ---___---___---___---___---___---___---___---___---___---___---___---___---___---
pred_GLM <- predict.glm(glm_opt, model_test, type = "response")
pred_GLM

GLM_comp <- as.data.frame(cbind(model_test$freq,pred_GLM))
GLM_comp[,3] <- (GLM_comp[,1] - GLM_comp[,2])^2

MSE_GLM_test <- sum(GLM_comp[,3])/length(GLM_comp[,3])
MSE_GLM_test   # 0.131994852894684




