#Students: Mathijs Gerits, Dries Maes and Ignace Decocq
#student numbers: r0811036
library(ggplot2)
library(dplyr)
library(visdat)
library(httpgd)
library(skimr)
library(DataExplorer)
library(ClustOfVar)
library(PCAmixdata)
library(sf)
library(moments)
library(mgcv)
library(caTools)
library(caret)
library(kable)
library(tmap)
library(rbin)
library(data.table)
library(glmnet)
hgd()
hgd_browse()
KULbg <- "#116e8a"



###loading in data and setting up names and types###
toledo <- read.csv("toledo files//Assignment.csv", ",", header = T)
Mdata <- read.csv("data.csv", ",", header = T)
belgium_shape_sf <- st_read('Severity_Analysis//shape file Belgie postcodes//npc96_region_Project1.shp', quiet = TRUE)

belgium_shape_sf <- st_transform(belgium_shape_sf, "+proj=longlat +datum=WGS84")
class(belgium_shape_sf)
Mdata$claimAm <- toledo$chargtot
data <- Mdata %>% select(-freq_ann)

# factor as rdataset
data <- as.data.frame(data)
Data <- data %>%
 mutate(across(c(X, ageph, expo, lnexpo, postal), as.numeric)) %>%
 mutate(across(c(agecar, sexph, power, split, fuel, use, fleet, sportc, cover), as.factor))
Data_nozero <- Data %>% select(-X, - expo, - lnexpo)

Data_nozero <- subset(Data_nozero, Data$claimAm != 0)
dim(Data_nozero)

Data_nozero$claimAm <- Data_nozero$claimAm

#binning postalcode and age 


bins <- rbin_quantiles(Data_nozero, claimAm, ageph, 10)$bins
bins$cut_point
breaks <- c(0, 25, 29, 33, 37, 42, 46, 50, 56, 65, 100)
group_ageph <- cut(Data_nozero$ageph,
                    breaks = breaks, include.lowest = T,
                    right = FALSE, labels = bins$cut_point)

Data_nozero$ageph <- group_ageph

#split data in test and train data with stratified sampling 
# the dependend variable is factorized to do the split 
set.seed(666)
trainIndex <- createDataPartition(Data_nozero$claimAm, p = 0.8, list = FALSE, times = 1, group = 10)

train <- Data_nozero[trainIndex,]
test <- Data_nozero[-trainIndex,]

class(train)

write.csv(train, file = "Severity_Analysis/train.csv" )



# GLM model to fit the data using gamma distribution

#linear model 
lm <- lm(claimAm ~ ., data = train)

myGrid <- expand.grid(
  alpha=0:1,
  lambda = seq(0.0001, 0.5, length = 10)
)

myControl <- trainControl(
  method = "cv", 
  number = 5, 
  verboseIter = TRUE
)

glm1 <- train(claimAm~ cover + power+sportc+fleet + use + split +  sexph + agecar + ageph,
              data = train, family = Gamma(link = "log"),
            method = "glmnet", 
            tuneGrid = myGrid, 
            trControl = myControl)
plot(glm1$finalModel)

pred_glm1 <- predict(glm1)
gam1 <- train(claimAm~ s(split, sp=1.2, k=5, bs="cr"), 
              data = train, faimy= Gamma(link = "log"), 
              method = "gam")

gam1 <- gam(claimAm ~ s(ageph, bs = "cr"), data = train, family = Gamma(link = "log"), method = "REML")
print(gam1)
gam1$sp 


plot(gam1, pages =1 , scheme=1)
a <- min(train$ageph):max(train$ageph)

