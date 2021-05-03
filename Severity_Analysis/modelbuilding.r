#Students: Mathijs Gerits, Dries Maes and Ignace Decocq
#student numbers: r0811036
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
library(glmnet)
library(readxl)
library(classInt)
library(evtree)

hgd()
hgd_browse()
KULbg <- "#116e8a"



###loading in data and setting up names and types###
toledo <- read.csv("toledo files//Assignment.csv", ",", header = T)
Mdata <- read.csv("data.csv", ",", header = T)
belgium_shape_sf <- st_read('Severity_Analysis//shape file Belgie postcodes//npc96_region_Project1.shp', quiet = TRUE)
belgium_shape_sf <- st_transform(belgium_shape_sf, "+proj=longlat +datum=WGS84")
class(belgium_shape_sf)
inspost <- read_excel("toledo files//inspost.xls")
#name change
Mdata$claimAm <- toledo$chargtot
#ann freq delete
data <- Mdata %>% select(-freq)


# adding long an lat from special dataframe post_dt (can take 5 min to load )


for( i in 1:length(data$postal)){
  data$LONG[i] <- inspost$LONG[data$postal[i] == inspost$CODPOSS]
  data$LAT[i] <- inspost$LAT[data$postal[i] == inspost$CODPOSS]
}

# factor as rdataset
data <- as.data.frame(data)
Data <- data %>%
 mutate(across(c(X, ageph, expo, lnexpo, postal), as.numeric)) %>%
 mutate(across(c(agecar, sexph, power, split, fuel, use, fleet, sportc, cover), as.factor)) %>% 
 select(-X, - expo, - lnexpo)


 #split data in test and train data with stratified sampling 
# the dependend variable is factorized to do the split 
set.seed(666)
trainIndex <- createDataPartition(Data$claimAm, p = 0.8, list = FALSE, times = 1, group = 10)

train <- Data[trainIndex,]
test <- Data[-trainIndex,]

#logclaims
train$logclaimAm <- log(train$claimAm)


#save dataset
write.csv(train, file = "Severity_Analysis/train.csv")

#remove elements with no claimamount
train_nozero <- subset(train, train$claimAm != 0)
dim(train_nozero)


################# binning age and spatial  ####################################

gam_spatial <- gam(claimAm ~s(LONG, LAT, bs = "tp"), family = Gamma(link="log"), data = train_nozero)

#new predict based on binning 
pred  <- predict(gam_spatial, newdata = post_dt, type= "terms", 
                 terms = 's(LONG,LAT)')
pred_dt <- data.frame(pc = post_dt$POSTCODE, 
                    LONG= post_dt$LONG, 
                      LONG= post_dt$LONG, 
                    LONG= post_dt$LONG, 
                    LAT = post_dt$LAT, pred)
names(pred_dt)[4] <- "fit_spatial"

# creating dataframe with binned spatial effect 
train_geo <- train_nozero
train_geo <- left_join(train_geo, pred_dt, by = c("postal" = "pc"))

#find optimal number of bins 
AIC_comp <- c()
BIC_comp <- c()
breaks <- list()
crp <- colorRampPalette(c("#99CCFF", "#003366"))

for( j in 2:15){
     num_bins <- j
     classint_fisher <- classIntervals(pred_dt$fit_spatial, 
                num_bins, style= "fisher")

     breaks[[j]] <- classint_fisher$brks


     belgium_shape_sf$classint_fisher <- cut(belgium_shape_sf$fit_spatial, 
     breaks = classint_fisher$brks, right= FALSE, 
     include.lowest = TRUE, dig.lab = 2)



     train_geo$geo <- as.factor(cut(train_geo$fit_spatial, 
     breaks = classint_fisher$brks, right=FALSE, 
     include.lowest=TRUE, dig.lab=2))

     AIC_comp[j] <- gam(claimAm ~s(ageph) + geo +power + cover + fleet + split +
                    fuel + sexph + agecar, method = "REML", data = train_geo, 
                    family = Gamma(link = "log"))$aic
     BIC_comp[j] <- BIC(gam(claimAm ~s(ageph) + geo +power + cover + fleet + split +
                    fuel + sexph + agecar, method = "REML", data = train_geo, 
                    family = Gamma(link = "log")))
}

plot(1:15, AIC_comp, type="l")
plot(1:15, BIC_comp, type="l")

#create bins for spatial data using optimal bins = 12 following BIC 
     num_bins <- 12
     classint_fisher <- classIntervals(pred_dt$fit_spatial, 
                num_bins, style= "fisher")

     breaks <- classint_fisher$brks


     belgium_shape_sf$classint_fisher <- cut(belgium_shape_sf$fit_spatial, 
     breaks = classint_fisher$brks, right= FALSE, 
     include.lowest = TRUE, dig.lab = 2)



     train_geo$geo <- as.factor(cut(train_geo$fit_spatial, 
     breaks = classint_fisher$brks, right=FALSE, 
     include.lowest=TRUE, dig.lab=2))
     #test 
     freq_glm_geo <- glm(claimAm ~ cover + fuel + geo, data = train_geo, family = Gamma(link = "log") )

#binning age 

claimAm_gam_geo <- gam(claimAm ~ cover + fuel + s(ageph) + geo, data = train_geo, family = Gamma(link = "log") )
#getting the dataset for age 
getGAMdata_single = function(model, term, var, varname){
     pred <- predict(model, type= "terms", terms =term)
     dt_pred <- tibble("x" = var, pred)
     dt_pred <- arrange(dt_pred, x)
     names(dt_pred) <- c("x", "s")
     dt_unique <- unique(dt_pred)
     dt_exp <- dt_pred%>% group_by(x) %>% summarize(tot=n())
     dt_exp <- dt_exp[c("x", "tot")]
     GAM_data <- left_join(dt_unique, dt_exp)
     names(GAM_data) <- c(varname, "s", "tot")
     GAM_data <- GAM_data[which(GAM_data$tot !=0), ]
     return(GAM_data)
}

gam_ageph <- getGAMdata_single(claimAm_gam_geo, "s(ageph)", train_geo$ageph, "ageph")

ctrl.freq <- evtree.control(alpha = 77, maxdepth = 5 )

evtree_claimAm_ageph <- evtree(s ~ ageph,
                            data = gam_ageph, 
                            weights = tot, 
                            control = ctrl.freq )

plot(evtree_claimAm_ageph)


#extract the points from the tree model 
splits_evtree = function(evtreemodel, GAMvar, DTvar){
     preds <- predict(evtreemodel, type= "node")
     nodes <- data.frame("x"= GAMvar, "nodes" = preds)
     nodes$change <- c(0, pmin(1,diff(nodes$nodes)))
     splits_evtree<- unique(c(min(DTvar), 
     nodes$x[which(nodes$change == 1)], 
     max(DTvar)))
     return(splits_evtree)
}

claimAm_splits_ageph <- splits_evtree(evtree_claimAm_ageph, 
                                      gam_ageph$ageph, 
                                      train_nozero$ageph)
claimAm_splits_ageph

breaks <- claimAm_splits_ageph
group_ageph <- cut(train_nozero$ageph,
                    breaks = breaks, include.lowest = T,
                    right = FALSE)

train_nozero$group_ageph <- group_ageph

train_geo$group_ageph <- group_ageph
# test 
 freq_glm_geo <- glm(claimAm ~ cover + fuel + geo + group_ageph, data = train_geo, family = Gamma(link = "log") )


#created weighted claimamount 
train_geo$weighted_claimAm <- train_geo$claimAm/train_geo$freq

# save new dataset
write.csv(train_geo, file = "Severity_Analysis/train_geo.csv")


### using lasso and ridge regresion with carret 

myControl <- trainControl(method = "cv", number = 10,   allowParallel = TRUE,
                          savePredictions = "all")



n <- dim(train_geo)[1]
glm1 <- glm(weighted_claimAm ~  group_ageph + agecar +
                sexph + fuel + split + use 
                +fleet + sportc +cover + power + 
                geo + agecar*cover + use*fleet + 
                sexph*fleet + sportc*power + geo*split , 
               data = train_geo,
               family = Gamma(link = "log"))

library(MASS)
#AIC
stepAIC(glm1, direcion = "forward", k=2,scope = list(upper=~group_ageph + agecar +
                sexph + fuel + split + use 
                +fleet + sportc +cover + power + 
                geo + agecar*cover + use*fleet + 
                sexph*fleet + sportc*power + geo*split , lower =~1), trace =TRUE)



plot(glm_net)
