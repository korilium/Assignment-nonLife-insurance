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
hgd()
hgd_browse()
KULbg <- "#116e8a"



###loading in data and setting up names and types###
toledo <- read.csv("toledo files//Assignment.csv", ",", header = T)
Mdata <- read.csv("data.csv", ",", header = T)
belgium_shape_sf <- st_read('Severity_Analysis//shape file Belgie postcodes//npc96_region_Project1.shp', quiet = TRUE)
belgium_shape_sf <- st_transform(belgium_shape_sf, "+proj=longlat +datum=WGS84")
class(belgium_shape_sf)
#name change
Mdata$claimAm <- toledo$chargtot
#ann freq delete
data <- Mdata %>% select(-freq_ann)


# adding long an lat from special dataframe post_dt (can take 5 min to load )


for( i in 1:length(data$postal)){
  data$long[i] <- post_dt$long[data$postal[i] == post_dt$POSTCODELA]
  data$lat[i] <- post_dt$lat[data$postal[i] == post_dt$POSTCODELA]
}

# factor as rdataset
data <- as.data.frame(data)
Data <- data %>%
 mutate(across(c(X, ageph, expo, lnexpo, postal), as.numeric)) %>%
 mutate(across(c(agecar, sexph, power, split, fuel, use, fleet, sportc, cover), as.factor))
Data_nozero <- Data %>% select(-X, - expo, - lnexpo)

Data_nozero <- subset(Data_nozero, Data$claimAm != 0)
dim(Data_nozero)

# do we need log or not??? 
Data_nozero$logclaimAm <- log(Data_nozero$claimAm)

# negative claim amount??? 
Data_nozero$claimAm[Data_nozero$claimAm < 1]
Data_nozero$logclaimAm[Data_nozero$claimAm < 1]

#binning age 
bins <- rbin_quantiles(Data_nozero, claimAm, ageph, 10)$bins
bins$cut_point
breaks <- c(0, 25, 29, 33, 37, 42, 46, 50, 56, 65, 100)
group_ageph <- cut(Data_nozero$ageph,
                    breaks = breaks, include.lowest = T,
                    right = FALSE)

Data_nozero$group_ageph <- group_ageph

#split data in test and train data with stratified sampling 
# the dependend variable is factorized to do the split 
set.seed(666)
trainIndex <- createDataPartition(Data_nozero$claimAm, p = 0.8, list = FALSE, times = 1, group = 10)

train <- Data_nozero[trainIndex,]
test <- Data_nozero[-trainIndex,]

class(train)

write.csv(train, file = "Severity_Analysis/train.csv")

levels(train$group_ageph)
levels(Data_nozero$group_ageph)
#####linear model  ####
lm <- lm(claimAm ~ agecar + sexph + fuel + split + use + fleet + sportc + cover + power, data = train)
summary(lm)


### test glm and gam with Age 
a <- seq(min(train$ageph), max(train$ageph))

#glm age not binned 
glm_age <- glm(claimAm ~ ageph, data = train, family = Gamma(link = "log"))
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
glm_grouped_age <- glm(claimAm ~ group_ageph, data = train, family = Gamma(link = "log"))

pred_glm_grouped_age <- predict(glm_grouped_age, newdata = data.frame(group_ageph = cut(a,
                    breaks = breaks, include.lowest = T,
                    right = FALSE)),
                          type = "terms", se.fit = TRUE)
b_glm_grouped_age <- pred_glm_grouped_age$fit
l_glm_grouped_age <- pred_glm_grouped_age$fit - qnorm(0.975) * pred_glm_grouped_age$se.fit
u_glm_grouped_age <- pred_glm_grouped_age$fit + qnorm(0.975) * pred_glm_grouped_age$se.fit
df <- data.frame(a, b_glm_grouped_age, l_glm_grouped_age, u_glm_grouped_age)

ggplot(df) +
geom_line(aes(x = a, y = b_glm_grouped_age)) +
geom_line(aes(x = a, y = l_glm_grouped_age), linetype = 3) +
geom_line(aes(x = a, y = u_glm_grouped_age), linetype = 3)

#gam age 
gam_age <- gam(claimAm ~ s(ageph), data = train, family = Gamma(link = "log"))
plot(gam_age, scheme = 1)


# estimate expected claim amount with spatial effects 
post_dt <- st_centroid(belgium_shape_sf)
post_dt$long <- do.call(rbind, post_dt$geometry)[, 1]
post_dt$lat <- do.call(rbind, post_dt$geometry)[, 2]
gam_spatial <- gam(claimAm ~s(long, lat, bs = "tp"), family = Gamma(link="log"), data = train)
plot(gam_spatial, scheme = 2)

pred  <- predict(gam_spatial, newdata = post_dt, type= "terms", terms = 's(long,lat)')
pred_dt <- data.frame(pc = post_dt$POSTCODELA, 
                      long = post_dt$long, 
                      lat = post_dt$lat, pred)
names(pred_dt)[4] <- "fit_spatial"


belgium_shape_sf <- left_join(belgium_shape_sf, pred_dt, by = c("POSTCODELA" = "pc"))

ggplot(belgium_shape_sf)+ 
geom_sf(aes(fill = fit_spatial), colour = NA) + 
ggtitle("claim amount")+
scale_fill_gradient(low = "#99CCFF", 
                    high = "#003366")+
                    theme_bw()

#binning spatial effect 




#setting op the dataframe to predict it with spatial effects 
post_dt$fuel <- train$fuel[1]
post_dt$use <- train$use[1]
post_dt$cover <- train$cover[1]
post_dt$sportc <- train$sportc[1]
post_dt$agecar <- train$agecar[1]
post_dt$sexph <- train$sexph[1]
post_dt$split <- train$split[1]
post_dt$power <- train$power[1]
post_dt$group_ageph <- train$group_ageph [1]


### using lasso and ridge regresion with carret 
