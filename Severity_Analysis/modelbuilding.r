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
toledo <- read.csv("toledo files//Assignment.csv", ",", header = T)
Mdata <- read.csv("data.csv", ",", header = T)
belgium_shape_sf <- st_read("Severity_Analysis//shape file Belgie postcodes//npc96_region_Project1.shp", quiet = TRUE)
belgium_shape_sf <- st_transform(belgium_shape_sf, "+proj=longlat +datum=WGS84")
class(belgium_shape_sf)
inspost <- read_excel("toledo files//inspost.xls")
# name change
Mdata$claimAm <- toledo$chargtot
# ann freq delete
data <- Mdata %>% select(-freq)
# extracting long and lat
post_dt <- st_centroid(belgium_shape_sf)
post_dt <- st_make_valid(post_dt)
post_dt$LONG <- do.call(rbind, post_dt$geometry)[, 1]
post_dt$LAT <- do.call(rbind, post_dt$geometry)[, 2]

# adding long an lat from special dataframe post_dt (can take 5 min to load )


for (i in 1:length(data$postal)) {
  data$LONG[i] <- inspost$LONG[data$postal[i] == inspost$CODPOSS]
  data$LAT[i] <- inspost$LAT[data$postal[i] == inspost$CODPOSS]
}

# factor as rdataset
data <- as.data.frame(data)
Data <- data %>%
  mutate(across(c(X, ageph, expo, lnexpo, postal), as.numeric)) %>%
  mutate(across(c(agecar, sexph, power, split, fuel, use, fleet, sportc, cover), as.factor)) %>%
  select(-X, -expo, -lnexpo)


# remove elements with no claimamount
data_nozero <- subset(Data, Data$claimAm != 0)
dim(data_nozero)


################# binning age and spatial  ####################################

# finding optimal GAM for binning spatial variable by hand 
gam1 <- gam(claimAm ~ cover + fuel + power + s(ageph)+
agecar + sexph + split + use + fleet + sportc 
+ s(LONG,LAT) ,  family = Gamma(link = "log"), 
data = data_nozero, method = 'REML'
)
summary(gam1)

#remove use 
gam2 <- gam(claimAm ~ cover + fuel + power + s(ageph)+
agecar + sexph + split + fleet + sportc 
+ s(LONG,LAT) ,  family = Gamma(link = "log"), 
data = data_nozero, method = 'REML'
)
summary(gam2)
#remove fleet 

gam3 <- gam(claimAm ~ cover + fuel + power + s(ageph)+
agecar + sexph + split + sportc 
+ s(LONG,LAT) ,  family = Gamma(link = "log"), 
data = data_nozero, method = 'REML'
)
summary(gam3)

#remove power 
gam4 <- gam(claimAm ~ cover + fuel + s(ageph)+
agecar + sexph + split + sportc 
+ s(LONG,LAT) ,  family = Gamma(link = "log"), 
data = data_nozero, method = 'REML'
)
summary(gam4)

#remove sexph 
gam5 <- gam(claimAm ~ cover + fuel + s(ageph)+
agecar  + split + sportc 
+ s(LONG,LAT) ,  family = Gamma(link = "log"), 
data = data_nozero, method = 'REML'
)
summary(gam5)

#return to GAM2 add ageph by power 
gam6 <- gam(claimAm ~ cover + fuel + power + s(ageph, by=power)+
agecar + sexph + split + fleet + sportc 
+ s(LONG,LAT) ,  family = Gamma(link = "log"), 
data = data_nozero, method = 'REML'
)
summary(gam6)
#remove sportc 
gam7 <- gam(claimAm ~ cover + fuel + power + s(ageph, by=power)+
agecar + sexph + split + fleet 
+ s(LONG,LAT) ,  family = Gamma(link = "log"), 
data = data_nozero, method = 'REML'
)
summary(gam7)

#change ageph by split 
gam8 <- gam(claimAm ~ cover + fuel + power + s(ageph, by=split)+
agecar + sexph + split + fleet 
+ s(LONG,LAT) ,  family = Gamma(link = "log"), 
data = data_nozero, method = 'REML'
)
summary(gam8)

## comparing models 

logLik(gam1) # -50251.48 (!)
logLik(gam2) # -50252.16 (!)
logLik(gam3) # -50514.78
logLik(gam4) # -50254.22 (!)
logLik(gam5) # -50256.22
logLik(gam6) # -50528.10
logLik(gam7) # -50535.65
logLik(gam8)

AIC(gam1) #302638.3
AIC(gam2) #302636.5 (!)
AIC(gam3) #302640.4
AIC(gam4) #302663.4
AIC(gam5) #302675.9 
#AIC is only increasing start from gam2 
AIC(gam6) #302555.3
AIC(gam7) #302554.4
AIC(gam8) #



post_dt["cover"] <- as.factor(data_nozero$cover[1])
post_dt["fuel"] <- as.factor(data_nozero$fuel[1])
post_dt["power"] <- as.factor(data_nozero$power[1])

# new predict based on binning
pred <- predict(gam_spatial,
  newdata = post_dt, type = "terms",
  terms = "s(LONG,LAT)"
)


pred_dt <- data.frame(
  pc = post_dt$POSTCODE,
  LONG = post_dt$LONG,
  LAT = post_dt$LAT, pred
)
names(pred_dt)[4] <- "fit_spatial"

belgium_shape_sf <- left_join(belgium_shape_sf, pred_dt, by = c("POSTCODE" = "pc"))

# creating dataframe with binned spatial effect
data_geo <- data_nozero
data_geo <- left_join(data_geo, pred_dt, by = c("postal" = "pc"))

# find optimal number of bins
AIC_comp <- c()
BIC_comp <- c()
breaks <- list()
crp <- colorRampPalette(c("#99CCFF", "#003366"))

for (j in 2:15) {
  num_bins <- j
  classint_fisher <- classIntervals(pred_dt$fit_spatial,
    num_bins,
    style = "fisher"
  )

  breaks[[j]] <- classint_fisher$brks


  belgium_shape_sf$classint_fisher <- cut(pred_dt$fit_spatial,
    breaks = classint_fisher$brks, right = FALSE,
    include.lowest = TRUE, dig.lab = 2
  )



  data_geo$geo <- as.factor(cut(data_geo$fit_spatial,
    breaks = classint_fisher$brks, right = FALSE,
    include.lowest = TRUE, dig.lab = 2
  ))

  AIC_comp[j] <- gam(claimAm ~ s(ageph) + geo + power + cover + fleet + split +
    fuel + sexph + agecar + use + sportc,
  method = "REML", data = data_geo,
  family = Gamma(link = "log")
  )$aic
  BIC_comp[j] <- BIC(gam(claimAm ~ s(ageph) + geo + power + cover + fleet + split +
    fuel + sexph + agecar + use + sportc,
  method = "REML", data = data_geo,
  family = Gamma(link = "log")
  ))
}

plot(1:15, AIC_comp, type = "l")
plot(1:15, BIC_comp, type = "l")

# create bins for spatial data using optimal bins = 12 following BIC
num_bins <- 12
classint_fisher <- classIntervals(pred_dt$fit_spatial,
  num_bins,
  style = "fisher"
)

breaks <- classint_fisher$brks


belgium_shape_sf$classint_fisher <- cut(belgium_shape_sf$fit_spatial,
  breaks = classint_fisher$brks, right = FALSE,
  include.lowest = TRUE, dig.lab = 2
)



data_geo$geo <- as.factor(cut(data_geo$fit_spatial,
  breaks = classint_fisher$brks, right = FALSE,
  include.lowest = TRUE, dig.lab = 2
))


crp <- colorRampPalette(c("#99CCFF", "#003366"))
plot(classint_fisher, crp(num_bins),
  xlab = expression(hat(f)(long, lat)),
  main = "Fisher"
)

belgium_shape_sf$class_fisher <- cut(belgium_shape_sf$fit_spatial,
  breaks = classint_fisher$brks, right = FALSE, include.lowest = TRUE, dig.lab = 2
)


crp <- colorRampPalette(c("#99CCFF", "#003366"))

colourCount <- length(unique(belgium_shape_sf$class_fisher))
getPalette <- colorRampPalette(brewer.pal(9, "Blues"))(length(unique(belgium_shape_sf$class_fisher)))

ggplot(belgium_shape_sf) +
  theme_bw() +
  labs(fill = "Fisher") +
  geom_sf(aes(fill = class_fisher), colour = NA) +
  ggtitle("MTPLclaimseveritydata") +
  scale_fill_manual(values = getPalette, na.value = "white") +
  theme_bw()

# test
freq_glm_geo <- glm(claimAm ~ cover + fuel + geo, data = data_geo, family = Gamma(link = "log"))

# binning age

claimAm_gam_geo <- gam(claimAm ~ cover + fuel + s(ageph) + geo, data = data_geo, family = Gamma(link = "log"))
# getting the dataset for age
getGAMdata_single <- function(model, term, var, varname) {
  pred <- predict(model, type = "terms", terms = term)
  dt_pred <- tibble("x" = var, pred)
  dt_pred <- arrange(dt_pred, x)
  names(dt_pred) <- c("x", "s")
  dt_unique <- unique(dt_pred)
  dt_exp <- dt_pred %>%
    group_by(x) %>%
    summarize(tot = n())
  dt_exp <- dt_exp[c("x", "tot")]
  GAM_data <- left_join(dt_unique, dt_exp)
  names(GAM_data) <- c(varname, "s", "tot")
  GAM_data <- GAM_data[which(GAM_data$tot != 0), ]
  return(GAM_data)
}

gam_ageph <- getGAMdata_single(claimAm_gam_geo, "s(ageph)", data_geo$ageph, "ageph")

ctrl.freq <- evtree.control(alpha = 77, maxdepth = 5)

evtree_claimAm_ageph <- evtree(s ~ ageph,
  data = gam_ageph,
  weights = tot,
  control = ctrl.freq
)

plot(evtree_claimAm_ageph)


# extract the points from the tree model
splits_evtree <- function(evtreemodel, GAMvar, DTvar) {
  preds <- predict(evtreemodel, type = "node")
  nodes <- data.frame("x" = GAMvar, "nodes" = preds)
  nodes$change <- c(0, pmin(1, diff(nodes$nodes)))
  splits_evtree <- unique(c(
    min(DTvar),
    nodes$x[which(nodes$change == 1)],
    max(DTvar)
  ))
  return(splits_evtree)
}

claimAm_splits_ageph <- splits_evtree(
  evtree_claimAm_ageph,
  gam_ageph$ageph,
  data_nozero$ageph
)
claimAm_splits_ageph

breaks <- claimAm_splits_ageph
group_ageph <- cut(data_nozero$ageph,
  breaks = breaks, include.lowest = T,
  right = FALSE
)

data_nozero$group_ageph <- group_ageph

data_geo$group_ageph <- group_ageph
# test
freq_glm_geo <- glm(claimAm ~ cover + fuel + geo + group_ageph, data = data_geo, family = Gamma(link = "log"))



# split data in test and train data with stratified sampling
# the dependend variable is factorized to do the split
set.seed(666)
trainIndex <- createDataPartition(data_geo$claimAm, p = 0.8, list = FALSE, times = 1, group = 10)

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



