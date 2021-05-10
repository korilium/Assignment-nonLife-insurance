# Students: Mathijs Gerits, Dries Maes and Ignace Decocq
# student numbers: r0811036
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
# xtracting long and lat
post_dt <- st_centroid(belgium_shape_sf)
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


gam_spatial <- gam(claimAm ~ s(LONG, LAT, bs = "tp"), family = Gamma(link = "log"), data = data_nozero)



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
    fuel + sexph + agecar,
  method = "REML", data = data_geo,
  family = Gamma(link = "log")
  )$aic
  BIC_comp[j] <- BIC(gam(claimAm ~ s(ageph) + geo + power + cover + fleet + split +
    fuel + sexph + agecar,
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
model_selection_aic <- glmulti(claimAm ~ group_ageph + agecar +
  sexph + fuel + split + use + fleet + 
  sportc + cover + power + geo + agecar * cover +
  use * fleet +  sexph * fleet + sportc * power +
  group_ageph * split, data = train_geo, family = Gamma(link ="log"), bunch = 50, 
  crit = GammaAIC, method = "h"
)


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

opt_model <- glm( claimAm~1+group_ageph+agecar+sexph+fuel+split+use+fleet+cover+power+geo+fleet:use+cover:agecar, 
                  data = train_geo, family = Gamma(link = "log"))

pred_glm <- predict(opt_model, newdata=test_geo, type= "response")

#see fit 

diff <- test_geo$claimAm - pred_glm
tot_diff <- sum(test_geo$claimAm) - sum(pred_glm)

x <- seq(1:length(pred_glm))
df_pred <- as.data.frame(cbind(x, pred_glm, test_geo$claimAm, diff))


ggplot(df_pred, aes(x = x))+
geom_line(aes(y = diff))


##### random forest ###

forest <- rforest(claimAm ~ ageph + agecar + 
                   sexph + fuel + split + use + fleet + 
                   sportc + cover + power + geo, 
                   data = train_geo, method = 'gamma', ncand = 5, 
                   ntrees = 500, subsample = 0.8, track_oob = TRUE )


pred <- predict.rforest(forest,test_geo)

oob_df <- data.frame('iteration' = seq_len(length(forest[['oob_error']])),
                     'oob_error' = forest[['oob_error']])
ggplot(oob_df, aes(x = iteration, y = oob_error)) + geom_point()


forest %>% importance_rforest
