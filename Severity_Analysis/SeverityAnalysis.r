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


# map of belgium
ggplot(belgium_shape_sf) +
geom_sf() +
ggtitle("WelcometoBelgium!") +
theme_bw()

simple_shp <- st_simplify(belgium_shape_sf, dTolerance = 0.00001)
qtm(simple_shp)


#binning postalcode and age 

post_expo <- Data_nozero %>% group_by(postal) %>% summarize(num = n(), tot_freq = sum(freq))
post_expo %>% slice(1:5)

belgium_shape_sf <- left_join(belgium_shape_sf, post_expo, by = c("POSTCODE" = "postal"))

belgium_shape_sf$claimAm <- belgium_shape_sf$tot_freq / belgium_shape_sf$Shape_Area

belgium_shape_sf$claimAm_class <- cut(belgium_shape_sf$claimAm,
                                   breaks = quantile(belgium_shape_sf$claimAm,
                                   c(0, 0.2, 0.8, 1), na.rm = TRUE),
                                   right = FALSE, include.lowest = TRUE,
                                   labels = c("low", "average", "high"))

ggplot(belgium_shape_sf) +
geom_sf(aes(fill = claimAm_class), colour = "black", size = 0.1) +
ggtitle("MTPLclaimamountcydata") +
labs(fill = "Relative\nclaimamount") +
scale_fill_brewer(palette = "Blues", na.value = "white") +
theme_bw()

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


# exploratory data analysis 
ggplot(train, aes(x = claimAm)) +
geom_density() +
scale_x_continuous(trans = 'log2') +
geom_histogram(aes(y = ..density..), fill = "#bd5b4e", color = "white", alpha = 0.7) +
geom_rug()

ggplot(data = train, mapping = aes(x = cover, y = log(claimAm))) +
  geom_jitter(aes(color = 'red'), alpha = 0.2) +
  geom_boxplot(fill = "bisque", color = "black", alpha = 0.3) +
    labs(x = 'claim amount per age cat') +
  guides(color = FALSE) +
  theme_minimal()

ggplot(data = train, mapping = aes(x = split, y = log(claimAm))) +
  geom_jitter(aes(color = 'red'), alpha = 0.2) +
  geom_boxplot(fill = "bisque", color = "black", alpha = 0.3) +
    labs(x = 'claim amount per age cat') +
  guides(color = FALSE) +
  theme_minimal()

ggplot(data = train, mapping = aes(x = agecar, y = log(claimAm))) +
  geom_jitter(aes(color = 'red'), alpha = 0.2) +
  geom_boxplot(fill = "bisque", color = "black", alpha = 0.3) +
    labs(x = 'claim amount per age cat') +
  guides(color = FALSE) +
  theme_minimal()

ggplot(data = train, mapping = aes(x = sexph, y = log(claimAm))) +
  geom_jitter(aes(color = 'red'), alpha = 0.2) +
  geom_boxplot(fill = "bisque", color = "black", alpha = 0.3) +
    labs(x = 'claim amount per age cat') +
  guides(color = FALSE) +
  theme_minimal()


train %>% plot_bar()
train %>% plot_histogram()


Data_group <- splitmix(Data_nozero)
Data_quanti <- Data_nozero[Data_group$col.quant]
Data_quali <- Data_nozero[Data_group$col.qual]

tree <- hclustvar(X.quanti = Data_quanti, X.quali = Data_quali)
plot(tree)

# mean, variance skewnnes and kurtosis
sapply(Data_quanti, mean)
sapply(Data_quanti, var)
sapply(Data_quanti, skewness)
sapply(Data_quanti, kurtosis)


#split data in test and train data with stratified sampling 
# the dependend variable is factorized to do the split 
set.seed(666)
trainIndex <- createDataPartition(Data_nozero$claimAm, p = 0.8, list = FALSE, times = 1, group = 10)

train <- Data_nozero[trainIndex,]
test <- Data_nozero[-trainIndex,]






#order the power and split
#Data$power <- ordered(data$power, levels = c("<66", "66-110", ">110"))
#Data$split <- ordered(data$split, levels = c("Once", "Twice", "Thrice", "Monthly"))



# GLM model to fit the data using gamma distribution


lm <- lm(claimAm ~ ., data = train)

glm1 <- train(claimAm ~ fleet + sportc + cover + power + use + split + fuel + sexph, data = train, family = Gamma("log"),
            method = "glm")
glm2 <- train()

summary(glm)
plot(train$claimAm, train$sexph)
