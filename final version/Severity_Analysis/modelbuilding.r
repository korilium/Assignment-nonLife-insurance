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
library(pdp)

# create plot image
hgd()
hgd_browse()
KULbg <- "#116e8a"

### loading in data and setting up names and types###
data <- read.csv2(file = "toledo files/Assignment.csv", sep = ",")
dataM <- read.csv2(file = "Frequency_Analysis/Data.csv", sep = ",")
data$geo <- dataM$geo
data$group_ageph <- dataM$agephGR
names(data) <- c("ageph", "postal", "expo", "lnexpo", "freq", 
                "annfreq", "claimAm", "agecar", "sexph", "fuel",
                 "split", "use", "fleet", "sportc", "cover", "power",
                 "geo", "group_ageph"
)


#### setting to the right type ###
Data <- as.data.frame(data)
train <- Data %>%mutate(across(c(claimAm,  ageph), as.numeric)) %>%
 mutate(across(c(agecar, sexph, power, split, fuel, use, fleet, sportc, cover, group_ageph, geo), as.factor))

#### take elements with claimam and create logclaimam variable #### 
train_nozero <- subset(train, Data$claimAm != 0)

str(train_nozero)
dim(train_nozero)

# split data in test and train data with stratified sampling
# the dependend variable is factorized to do the split
set.seed(666)
trainIndex <- createDataPartition(train_nozero$claimAm, p = 0.8, list = FALSE, times = 1, group = 10)

train_geo <- data_geo[trainIndex, ]
test_geo <- data_geo[-trainIndex, ]



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

model_selection_aic <- glmulti(claimAm ~ group_ageph + agecar + sexph
  + fuel + split + use + fleet + sportc + cover + power + geo +
  sportc:power + use:fleet + split:group_ageph + split:geo + sexph:fuel + agecar:cover,
data = train_geo,
name = "aic", crit = GammaAIC, family = Gamma("log"), method = "h",
fitfunction = glm, level = 2, report = TRUE,
includeobjects = FALSE, confsetsize = 100, popsize = 100, 
)
bestmodel <- model_selection_aic@formulas[1]
secondbestmodel <- model_selection_aic@formulas[2]
thirdbestmodel <- model_selection_aic@formulas[3]
plot(model_selection_aic, type = "p")
plot(model_selection_aic, type = "w")
plot(model_selection_aic, type = "s")

# bic selection
model_selection_bic <- glmulti(claimAm ~ group_ageph + agecar + sexph
  + fuel + split + use + fleet + sportc + cover + power + geo +
  sportc:power + use:fleet + split:group_ageph + split:geo + sexph:fuel + agecar:cover,
data = train_geo, name = "bic", crit = GammaBIC, family = Gamma("log"),
method = "h", fitfunction = glm, level = 2, marginality = TRUE, report = TRUE,
includeobjects = FALSE, confsetsize = 100, popsize = 100, 
)
bestmodel <- model_selection_bic@formulas[1]
secondbestmodel <- model_selection_bic@formulas[2]
thirdbestmodel <- model_selection_bic@formulas[3]

plot(model_selection_bic, type = "p")
plot(model_selection_bic, type = "w")
plot(model_selection_bic, type = "s")




# Best model: claimAm~1+group_ageph+agecar+sexph+fuel+split+use+fleet+cover+power+geo+split:group_ageph+fleet:use+cover:agecar

plot(model_selection_aic, type = "p")


plot(model_selection_bic, type = "p")

# Best model: claimAm~1+group_ageph+agecar+sexph+fuel+split+use+fleet+cover+power+geo+fleet:use+cover:agecar

opt_model <- glm(claimAm ~ group_ageph + agecar + sexph + fuel + split + use + fleet + cover + 
power + geo + fleet:use + cover:agecar + split:group_ageph,
  data = train_geo, family = Gamma(link = "log")
)
#predict on train 

pred_glm <- predict(opt_model, newdata = train_geo, type = "response")

diff_glm <- sqrt(sum((train_geo$claimAm - pred_glm)^2) / length(pred_glm))




#predict on test 
pred_glm <- predict(opt_model, newdata = test_geo, type = "response")

diff_glm <- sqrt(sum((test_geo$claimAm - pred_glm)^2) / length(pred_glm))


##### random forest ###
set.seed(666)
forest <- rforest(claimAm ~ ageph + agecar +
  sexph + fuel + split + use + fleet +
  sportc + cover + power + geo,
data = train_geo, method = "gamma", ncand = 5,
ntrees = 500, track_oob = TRUE, 
control = rpart.control(minsplit=20, cp = 0, xval = 0, maxdepth = 5, minbucket = 20), 
subsample= 0.8, red_mem =  TRUE)

forest[['trees']] [[1]]


importance <- forest %>% importance_rforest
importance <- importance[order(importance$importance, decreasing =TRUE),]
ggplot(importance, aes(x = variable , y = importance))+
geom_bar(stat = "identity", fill = "#116e8a" )

#predict on train 
pred <- predict.rforest(forest, train_geo)

diff_forest <- sqrt(sum((train_geo$claimAm - pred)^2) / length(pred))
#predict on test 
pred <- predict.rforest(forest, test_geo)

diff_forest <- sqrt(sum((test_geo$claimAm - pred)^2) / length(pred))

#OOB plot 
oob_df <- data.frame(
  "iteration" = seq_len(length(forest[["oob_error"]])),
  "oob_error" = forest[["oob_error"]]
)
ggplot(oob_df, aes(x = iteration, y = oob_error)) +
  geom_point()



