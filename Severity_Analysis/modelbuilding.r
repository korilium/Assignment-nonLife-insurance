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
data <- Mdata %>% select(-freq_ann)


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

#binning age
#binning age 

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

gam_ageph <- getGAMdata_single(freq_gam_geo, "s(ageph)", train_geo$ageph, "ageph")

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
group_ageph <- cut(train$ageph,
                    breaks = breaks, include.lowest = T,
                    right = FALSE)

train$group_ageph <- group_ageph

#save dataset
write.csv(train, file = "Severity_Analysis/train.csv")

#remove elements with no claimamount
train_nozero <- subset(train, train$claimAm != 0)
dim(train_nozero)


levels(train$group_ageph)
levels(train_nozero$group_ageph)
#####linear model  ####
lm <- lm(claimAm ~ agecar + sexph + fuel + split + 
          use + fleet + sportc + cover + power, data = train)
summary(lm)


### test glm and gam with Age 
a <- seq(min(train_nozero$ageph), max(train_nozero$ageph))

#glm age not binned 
glm_age <- glm(claimAm ~ ageph,
               data = train_nozero, family = Gamma(link = "log"))

pred_glm_age <- predict(glm_age, newdata = data.frame(ageph = a),
                          type = "terms", se.fit = TRUE)
b_glm_age <- pred_glm_age$fit
l_glm_age <- pred_glm_age$fit - qnorm(0.975) * pred_glm_age$se.fit
u_glm_age <- pred_glm_age$fit + qnorm(0.975) * pred_glm_age$se.fit
df <- data.frame(a, b_glm_age, l_glm_age, u_glm_age)

ggplot(df) +
geom_line(aes(x = a, y = b_glm_age)) +
geom_line(aes(x = a, y = l_glm_age), linetype = 3) +
geom_line(aes(x = a, y = u_glm_age), linetype = 3)

#glm age binned
# note the cut is done on A  as it needs the same factorization as age 
glm_grouped_age <- glm(claimAm ~ group_ageph,
                       data = train_nozero, family = Gamma(link = "log"))

pred_glm_grouped_age <- predict(glm_grouped_age,
                    newdata = data.frame(group_ageph = 
                    cut(a, breaks = breaks, include.lowest = T,
                    right = FALSE)), type = "terms", se.fit = TRUE)

b_glm_grouped_age <- pred_glm_grouped_age$fit
l_glm_grouped_age <- pred_glm_grouped_age$fit - qnorm(0.975) * pred_glm_grouped_age$se.fit
u_glm_grouped_age <- pred_glm_grouped_age$fit + qnorm(0.975) * pred_glm_grouped_age$se.fit
df <- data.frame(a, b_glm_grouped_age, l_glm_grouped_age, u_glm_grouped_age)

ggplot(df) +
geom_line(aes(x = a, y = b_glm_grouped_age)) +
geom_line(aes(x = a, y = l_glm_grouped_age), linetype = 3) +
geom_line(aes(x = a, y = u_glm_grouped_age), linetype = 3)

#gam age 
gam_age <- gam(claimAm ~ s(ageph), data = train_nozero, family = Gamma(link = "log"))
plot(gam_age, scheme = 1)


# estimate expected claim amount with spatial effects 
gam_spatial <- gam(claimAm ~s(LONG, LAT, bs = "tp"), family = Gamma(link="log"), data = train_nozero)
plot(gam_spatial, scheme = 2)



post_dt <- st_centroid(belgium_shape_sf)
post_dt$LONG<- do.call(rbind, post_dt$geometry)[,1]
post_dt$LAT <- do.call(rbind, post_dt$geometry)[,2]

pred  <- predict(gam_spatial, newdata = post_dt, type= "terms", 
                 terms = 's(LONG,LAT)')



pred_dt <- data.frame(pc = post_dt$POSTCODE, 
                      LONG= post_dt$LONG, 
                      LAT = post_dt$LAT, pred)
names(pred_dt)[4] <- "fit_spatial"


belgium_shape_sf <- left_join(belgium_shape_sf, 
                              pred_dt, by = c("POSTCODE" = "pc"))

tm_shape(belgium_shape_sf) + 
  tm_borders(col = 'white', lwd = .1 )+
  tm_fill("fit_spatial", style = "cont",
  palette = "RdBu", legend.reverse = TRUE, 
  midpoint = TRUE) + 
  tm_layout(legend.title.size = 1.0 , 
            legend.text.size = 1.0 )

#binning spatial effect 


classint_fisher <- classIntervals(
                    pred_dt$fit_spatial,5, 
                   style= "fisher")

classint_fisher$brks
crp <- colorRampPalette(c("#99CCFF", "#003366"))
plot(classint_fisher, crp(5),
     xlab = expression(hat(f)(long,lat)), 
     main = "fisher")
belgium_shape_sf$classint_fisher <- cut(belgium_shape_sf$fit_spatial, 
     breaks = classint_fisher$brks, right= FALSE, 
     include.lowest = TRUE, dig.lab = 2)


ggplot(belgium_shape_sf)+ theme_bw()+
labs(fill = "Fisher") + geom_sf(aes(fill = classint_fisher), colour = NA) + 
ggtitle("MTPL claim amount") + 
scale_fill_brewer(palette = "Blues", na.value = "white") + 
theme_bw()


# creating dataframe with binned spatial effect 
names(pred_dt)[4] <- "fit_spatial"
train_geo <- train_nozero
train_geo <- left_join(train_geo, pred_dt, by = c("postal" = "pc"))
train_geo$geo <- as.factor(cut(train_geo$fit_spatial, 
     breaks = classint_fisher$brks, right=FALSE, 
     include.lowest=TRUE, dig.lab=2))

freq_gam_geo <- gam(claimAm ~ cover + fuel + s(ageph) + geo, data = train_geo, family = Gamma(link = "log") )

#binning age 

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

gam_ageph <- getGAMdata_single(freq_gam_geo, "s(ageph)", train_geo$ageph, "ageph")

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

### using lasso and ridge regresion with carret 
