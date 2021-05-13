# Students: Mathijs Gerits, Dries Maes and Ignace Decocq
# student numbers: r0811036
# libraries 
library(ggplot2)
library(dplyr)
library(httpgd)
library(sf)
library(moments)
library(mgcv)
library(caTools)
library(caret)
library(tmap)
library(rbin)
library(data.table)
library(readxl)
library(classInt)
library(evtree)
library(glmulti)
library(distRforest)

#create plot image 
hgd()
hgd_browse()
KULbg <- "#116e8a"

#getting claimAm
toledo <- read.csv("toledo files//Assignment.csv", ",", header = T)

#loading data of Mathijs 

Mdata <- as.data.frame(read.csv("Frequency_Analysis/Data.csv", ",", header = T))
Mdata$claimAm <- toledo$chargtot
#setting into right type 

data <- as.data.frame(Mdata)
Data <- data %>%
  mutate(across(c(ageph, expo, postal), as.numeric)) %>%
  mutate(across(c(agecar, sexph, power, split, fuel, use, fleet, sportc, cover, geo, agephGR), as.factor)) %>%
  select( -expo)


# remove elements with no claimamount
data0 <- subset(Data, Data$claimAm > 1)

train_index <- unlist(read.csv("Frequency_Analysis/trainIndex.csv", ",", header = T))


train_geo <- data0[train_index, ]
train_geo <- train_geo[complete.cases(train_geo),]
test_geo <- data0[train_index, ]

# save new dataset
write.csv(train_geo, file = "Severity_Analysis/train_geo.csv")


# Variable selection procedure 

GammaAIC <- function(fit) {
  disp <- MASS::gamma.dispersion(fit)
  mu <- fit$fitted.values
  p <- fit$rank
  y <- fit$y
  -2 * sum(dgamma(y, 1 / disp, scale = mu * disp, log = TRUE)) + 2 * p
}


GammaBIC <- function(fit) {
  disp <- MASS::gamma.dispersion(fit)
  mu <- fit$fitted.values
  p <- fit$rank
  y <- fit$y
  n <- length(y)
  -2 * sum(dgamma(y, 1 / disp, scale = mu * disp, log = TRUE)) + p * log(n)
}

# aic selection

model_selection_aic <- glmulti(claimAm ~agecar + sexph 
+ fuel + split + use + fleet + sportc + cover + power + geo +agephGR+
 sportc:power + use:fleet + split:agephGR + split:geo + sexph:fuel +agecar:cover, data = train_geo, 
 name="aic",  crit = GammaAIC, family = Gamma("log"),  method = "h",
  fitfunction = glm, level =2,  marginality = TRUE, report = TRUE,  
  includeobjects=FALSE, confsetsize = 100,   popsize = 100, mutrate = 0.05,
   sexrate = 0.3, imm=0.5 , deltaM = 1, conseq =  2)

model_selection_bic <- glmulti(claimAm ~agecar + sexph 
+ fuel + split + use + fleet + sportc + cover + power + geo +agephGR+
 sportc:power + use:fleet + split:agephGR + split:geo + sexph:fuel +agecar:cover, 
data = train_geo, name="bic", crit = GammaBIC, family = Gamma("log"),
  method = "h", fitfunction = glm, level =2,  marginality = TRUE, report = TRUE,
  includeobjects=FALSE, confsetsize = 20, popsize = 50, mutrate = 0.05, sexrate = 0.3, imm=0.5 , deltaM = 1, conseq = 2 )





plot(model_selection_aic, type="p")
#Best model: claimAm~1+group_ageph+agecar+sexph+fuel+split+use+fleet+cover+power+geo+split:group_ageph+fleet:use+cover:agecar

plot(model_selection_aic, type="p")

# bic selection


model_selection_bic <- glmulti(claimAm ~ group_ageph + agecar +
  sexph + fuel + split + use
  + fleet + sportc + cover + power +
  geo + agecar * cover + use * fleet +
  sexph * fleet + sportc * power + group_ageph* split,
data = train_geo, family = Gamma(link ="log"), bunch = 50, crit = GammaBIC, method = "h"
)

plot(model_selection_bic, type="p")

#Best model: claimAm~1+group_ageph+agecar+sexph+fuel+split+use+fleet+cover+power+geo+fleet:use+cover:agecar

opt_model <- glm( claimAm~1+agephGR+agecar+sexph+fuel+split+use+fleet+cover+power+geo+fleet:use+cover:agecar, 
                  data = train_geo, family = Gamma(link = "log"))

pred_glm <- predict(opt_model, newdata=test_geo, type= "response")

#see fit 

diff_glm <- sqrt(sum((test_geo$claimAm - pred_glm)^2)/length(pred_glm))


##### random forest ###

forest <- rforest(claimAm ~ ageph + agecar + 
                   sexph + fuel + split + use + fleet + 
                   sportc + cover + power + geo, 
                   data = train_geo, method = 'gamma', ncand = 5, 
                   ntrees = 500, subsample = 0.8, track_oob = TRUE )


pred <- predict.rforest(forest,test_geo)

diff_forest <- sqrt(sum((test_geo$claimAm - pred)^2)/length(pred))

oob_df <- data.frame('iteration' = seq_len(length(forest[['oob_error']])),
                     'oob_error' = forest[['oob_error']])
ggplot(oob_df, aes(x = iteration, y = oob_error)) + geom_point()


forest %>% importance_rforest
