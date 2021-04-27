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
bins <- rbin_quantiles(train, claimAm, ageph, 10)$bins
bins$cut_point
breaks <- c(0, 25, 29, 33, 37, 42, 46, 50, 56, 65, 100)
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
pred  <- predict(gam_spatial, newdata = post_dt, type= "terms", terms = 's(LONG,LAT)')
pred_dt <- data.frame(pc = post_dt$POSTCODE, 
                      long = post_dt$LONG, 
                      lat = post_dt$LAT, pred)
names(pred_dt)[4] <- "fit_spatial"


belgium_shape_sf <- left_join(belgium_shape_sf, pred_dt, by = c("POSTCODE" = "pc"))

tm_shape(belgium_shape_sf) + 
  tm_borders(col = 'white', lwd = .1 )+
  tm_fill("fit_spatial", style = "cont",
  palette = "RdBu", legend.reverse = TRUE, 
  midpoint = TRUE) + 
  tm_layout(legend.title.size = 1.0 , 
            legend.text.size = 1.0 )

#binning spatial effect 
classint_fisher <- classIntervals(
                    pred_dt$fit_spatial, num_bins, 
                   style= "fisher")

classint_fisher$brks



#setting op the dataframe to predict it with spatial effects 
inspost$fuel <- train$fuel[1]
inspost$use <- train$use[1]
inspost$cover <- train$cover[1]
inspost$sportc <- train$sportc[1]
inspost$agecar <- train$agecar[1]
inspost$sexph <- train$sexph[1]
inspost$split <- train$split[1]
inspost$power <- train$power[1]
inspost$group_ageph <- train$group_ageph [1]


### using lasso and ridge regresion with carret 
