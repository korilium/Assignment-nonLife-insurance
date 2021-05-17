
#students: Mathijs Gerits, Dieter Maes, Ignace Decocq
#student numbers: r848304, r0853946, r0811036



### ---___---___---___---___---___---___---___---___---___---___---___---___---___---
### Exploratory data analysis (Frequency)
### ---___---___---___---___---___---___---___---___---___---___---___---___---___---

library(ggplot2)
library(tidyverse)
library(rgdal)
library(sf)
library(tmap)
library(rgeos)
library(mapview)
library(leaflet)
library(mgcv)
library(rstudioapi)
library(gridExtra)
library(mgcv)
library(classInt)
library(evtree)
library(rbin)
library(caret)
library(RColorBrewer)
library(jtools)

KUL <- "#116E8A"

# Making the data ready 
# Data$chargtot <- NULL   # This analysis will only focus on frequency data
# Data[,15] <- ordered(Data[,15], levels = c("<66","66-110",">110"))
# colnames(Data) <- c("ageph","postal","expo","lnexpo","freq","freq_ann","agecar","sexph","fuel",
#                     "split","use","fleet","sportc","cover","power")

# Removing unnecessary variables 
Data$lnexpo <- NULL
Data$freq_ann <- NULL

# Getting a feel for the data
str(Data)
names(Data)
head(Data)
summary(Data)

str(data_train)
names(data_train)
head(data_train)
summary(data_train)


# Plot relative frequency of variates
g_freq <- ggplot(data_train, aes(freq))+
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = KUL, col = KUL, alpha = 0.7)+
  ggtitle("Relative Frequency - Number of claims")+
  labs(y = "Relative frequency", x = "freq")

g_expo <- ggplot(data_train, aes(expo))+
  geom_histogram(aes(y = (..count..)/sum(..count..)), fill = KUL, col = KUL, alpha = 0.7, bins = 20)+
  ggtitle("Relative Frequency - Exposure")+
  labs(y = "Relative frequency", x = "expo")

g_ageph <- ggplot(data_train, aes(ageph))+
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = KUL, col = KUL, alpha = 0.7)+
  ggtitle("Relative Frequency - Age policyholder")+
  labs(y = "Relative frequency", x = "ageph")

g_agecar <- ggplot(data_train, aes(agecar))+
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = KUL, col = KUL, alpha = 0.7)+
  ggtitle("Relative Frequency - Age vehicle")+
  labs(y = "Relative frequency", x = "agecar")

g_sexph <- ggplot(data_train, aes(sexph))+
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = KUL, col = KUL, alpha = 0.7)+
  ggtitle("Relative Frequency - Sex policyholder")+
  labs(y = "Relative frequency", x = "sexpg")

g_fuel <- ggplot(data_train, aes(fuel))+
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = KUL, col = KUL, alpha = 0.7)+
  ggtitle("Relative Frequency - Fueltype")+
  labs(y = "Relative frequency", x = "fuel")

g_split <- ggplot(data_train, aes(split))+
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = KUL, col = KUL, alpha = 0.7)+
  ggtitle("Relative Frequency - Payment split")+
  labs(y = "Relative frequency", x = "split")

g_use <- ggplot(data_train, aes(use))+
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = KUL, col = KUL, alpha = 0.7)+
  ggtitle("Relative Frequency - Utilization of the vehicle")+
  labs(y = "Relative frequency", x = "use")

g_fleet <- ggplot(data_train, aes(fleet))+
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = KUL, col = KUL, alpha = 0.7)+
  ggtitle("Relative Frequency - Part of fleet or not")+
  labs(y = "Relative frequency", x = "fleet")

g_sportc <- ggplot(data_train, aes(sportc))+
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = KUL, col = KUL, alpha = 0.7)+
  ggtitle("Relative Frequency - Sportscar or not")+
  labs(y = "Relative frequency", x = "sportc")

g_cover <- ggplot(data_train, aes(cover))+
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = KUL, col = KUL, alpha = 0.7)+
  ggtitle("Relative Frequency - Type of cover")+
  labs(y = "Relative frequency", x = "cover")

g_power <- ggplot(data_train, aes(power))+
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = KUL, col = KUL, alpha = 0.7)+
  ggtitle("Relative Frequency - Power of car")+
  labs(y = "Relative frequency", x = "power")

grid.arrange(g_freq,g_expo,g_agecar,g_ageph,g_cover,g_fleet,g_fuel,g_power,g_sexph,g_split,g_sportc,g_use, ncol=4)

# Empirical frequency 
Data %>% summarize(emp_freq = sum(freq) / sum(expo))   # 0.1393355
data_train %>% summarize(emp_freq = sum(freq) / sum(expo))   # 0.1397242

Data %>% group_by(ageph) %>% summarize(tot_freq = sum(freq), tot_expo = sum(expo), emp_freq = sum(freq) / sum(expo))
Data %>% group_by(agecar) %>% summarize(tot_freq = sum(freq), tot_expo = sum(expo), emp_freq = sum(freq) / sum(expo))
Data %>% group_by(sexph) %>% summarize(tot_freq = sum(freq), tot_expo = sum(expo), emp_freq = sum(freq) / sum(expo))
Data %>% group_by(fuel) %>% summarize(tot_freq = sum(freq), tot_expo = sum(expo), emp_freq = sum(freq) / sum(expo))
Data %>% group_by(split) %>% summarize(tot_freq = sum(freq), tot_expo = sum(expo), emp_freq = sum(freq) / sum(expo))
Data %>% group_by(use) %>% summarize(tot_freq = sum(freq), tot_expo = sum(expo), emp_freq = sum(freq) / sum(expo))
Data %>% group_by(fleet) %>% summarize(tot_freq = sum(freq), tot_expo = sum(expo), emp_freq = sum(freq) / sum(expo))
Data %>% group_by(sportc) %>% summarize(tot_freq = sum(freq), tot_expo = sum(expo), emp_freq = sum(freq) / sum(expo))
Data %>% group_by(cover) %>% summarize(tot_freq = sum(freq), tot_expo = sum(expo), emp_freq = sum(freq) / sum(expo))
Data %>% group_by(power) %>% summarize(tot_freq = sum(freq), tot_expo = sum(expo), emp_freq = sum(freq) / sum(expo))

data_train %>% group_by(ageph) %>% summarize(tot_freq = sum(freq), tot_expo = sum(expo), emp_freq = sum(freq) / sum(expo))
data_train %>% group_by(agecar) %>% summarize(tot_freq = sum(freq), tot_expo = sum(expo), emp_freq = sum(freq) / sum(expo))
data_train %>% group_by(sexph) %>% summarize(tot_freq = sum(freq), tot_expo = sum(expo), emp_freq = sum(freq) / sum(expo))
data_train %>% group_by(fuel) %>% summarize(tot_freq = sum(freq), tot_expo = sum(expo), emp_freq = sum(freq) / sum(expo))
data_train %>% group_by(split) %>% summarize(tot_freq = sum(freq), tot_expo = sum(expo), emp_freq = sum(freq) / sum(expo))
data_train %>% group_by(use) %>% summarize(tot_freq = sum(freq), tot_expo = sum(expo), emp_freq = sum(freq) / sum(expo))
data_train %>% group_by(fleet) %>% summarize(tot_freq = sum(freq), tot_expo = sum(expo), emp_freq = sum(freq) / sum(expo))
data_train %>% group_by(sportc) %>% summarize(tot_freq = sum(freq), tot_expo = sum(expo), emp_freq = sum(freq) / sum(expo))
data_train %>% group_by(cover) %>% summarize(tot_freq = sum(freq), tot_expo = sum(expo), emp_freq = sum(freq) / sum(expo))
data_train %>% group_by(power) %>% summarize(tot_freq = sum(freq), tot_expo = sum(expo), emp_freq = sum(freq) / sum(expo))
# By comparing Data and data_train, we can see whether the training data is a good sample for the full dataset


# Frequency variable - descriptives 
freq_emp_mean <- sum(Data$freq) / sum(Data$expo)
freq_emp_var <- sum((Data$freq - freq_emp_mean*Data$expo)^2)/sum(Data$expo)

freq_emp_mean_train <- sum(data_train$freq) / sum(data_train$expo)
freq_emp_var_train <- sum((data_train$freq - freq_emp_mean_train*data_train$expo)^2)/sum(data_train$expo)

freq_emp_mean          # 0.1393355
freq_emp_mean_train    # 0.1397242

freq_emp_var           # 0.1516978
freq_emp_var_train     # 0.1520487
  # GOOD SAMPLE, first two moments are comparable 


## Representing spatial data

# shape_Belgium <-st_read(file.choose(), quiet = TRUE)
# shape_Belgium<-st_transform(shape_Belgium, "+proj=longlat +datum=WGS84")
# 
# post_expo <- Data %>% group_by(postal) %>% summarize(num = n(), total_expo = sum(expo))
# shape_Belgium <- left_join(shape_Belgium, post_expo, by = c("POSTCODE" = "postal"))
# shape_Belgium$freq_area <- shape_Belgium$total_expo/shape_Belgium$Shape_Area
# shape_Belgium$freq_class <- cut(shape_Belgium$freq_area,
#                           breaks = quantile(shape_Belgium$freq_area, c(0,0.2,0.8,1), na.rm = TRUE),
#                           right = FALSE, include.lowest = TRUE,
#                           labels = c("low", "average", "high"))
# 
# g_shapefile <- ggplot(shape_Belgium) + 
#   geom_sf(aes(fill = freq_class), colour = KUL, size = 0.1) +
#   ggtitle("Claim frequency data") +
#   labs(fill = "Relative\nexposure") +
#   scale_fill_brewer(palette = "Reds", na.value = "white")

g_shapefile

# Checking relations / dependencies between covariates
library(ClustOfVar)
library(PCAmixdata)

Data_group <- splitmix(Q)   # Q = Data, but without ordered factors, splitmix() does not work with ordered factors  
Data_quanti <- Data[Data_group$col.quant]
Data_quali <- Data[Data_group$col.qual]

tree <- hclustvar(X.quanti = Data_quanti, X.quali = Data_quali)
g_dendogram <- plot(tree)


### ---___---___---___---___---___---___---___---___---___---___---___---___---___---
### Making train indices for data_train and model_train (Frequency)
### ---___---___---___---___---___---___---___---___---___---___---___---___---___---
# Split data into train and test subsets 

set.seed(1729)
trainIndex <- createDataPartition(Data$freq, times = 1, p = 0.8, list = FALSE, group = 10)

data_train <- Data[trainIndex,]
data_test <- Data[-trainIndex,]

# data_train is used to calibrate and bin the continuous variables, later
# model_train will be made with the same indices, but with binned variables 


### ---___---___---___---___---___---___---___---___---___---___---___---___---___---
### Binning spatial data (Frequency)
### ---___---___---___---___---___---___---___---___---___---___---___---___---___---

##### PART 1: adjusting data for use coordinates 
post_dt <- st_centroid(shape_Belgium)
post_dt$long <- do.call(rbind, post_dt$geometry)[,1]
post_dt$lat <- do.call(rbind, post_dt$geometry)[,2]

for(i in 1:length(Data$postal)){
  Data$long[i] <- post_dt$long[Data$postal[i]==post_dt$POSTCODELA]
  Data$lat[i] <- post_dt$lat[Data$postal[i]==post_dt$POSTCODELA]
}   # this adds long-and-lat-coordinates to the Dataset 

gam_spatial <- gam(freq ~ s(long, lat, bs = "tp"), offset = log(expo), 
                        family = poisson(link = "log"), data = Data)

##### PART 2: finding the optimal GAM
# NOTE: use the training set !! 

gam1 <- gam(freq ~ s(ageph) + s(long,lat) + power + cover + sportc + fleet + use + split 
            + fuel + sexph + agecar, offset = log(expo), data = data_train,
            family = poisson(link = "log"))

gam2 <- gam(freq ~ s(ageph) + s(long,lat) + power + cover + sportc + fleet + use + split 
            + fuel + sexph + agecar, offset = log(expo), method = "REML", data = data_train,
            family = poisson(link = "log"))

gam3 <- gam(freq ~ s(ageph, by = power) + s(long,lat) + power + cover + sportc + fleet + use + split 
            + fuel + sexph + agecar, offset = log(expo), method = "REML", data = data_train,
            family = poisson(link = "log"))

summary(gam1)
summary(gam2)
summary(gam3)
# From gam1-3 we note that 'sportc' and 'use' are rather insignificant in explaining frequency 
# AND note that gam2 is best in explaining based on R2 and deviance explained 

gam4 <- gam(freq ~ s(ageph) + s(long,lat) + power + cover + fleet + split 
            + fuel + sexph + agecar, offset = log(expo), method = "REML", data = data_train,
            family = poisson(link = "log"))

summary(gam4) # Gives a better R2-adj and a better deviance explained 
# But, still note that 'sexph' is only significant at 1% level, so try and dropping it 

gam5 <- gam(freq ~ s(ageph) + s(long,lat) + power + cover + fleet + split 
            + fuel + agecar, offset = log(expo), method = "REML", data = data_train,
            family = poisson(link = "log"))

summary(gam5) # until now, best fit based upon R2 and deviance explained 

gam6 <- gam(freq ~ s(ageph, by = power) + s(long,lat) + power + cover + fleet + split 
            + fuel + agecar, offset = log(expo), method = "REML", data = data_train,
            family = poisson(link = "log"))

summary(gam6) # GAM with insignificant variables removed, but factor-smooth interaction

gam7 <- gam(freq ~ s(ageph, by = power) + s(long,lat) + cover + fleet + split 
            + fuel + agecar, offset = log(expo), method = "REML", data = data_train,
            family = poisson(link = "log"))

summary(gam7) 

# Comparing models 
logLik(gam1) # -50251.48 (!)
logLik(gam2) # -50252.16 (!)
logLik(gam3) # -50514.78
logLik(gam4) # -50254.22 (!)
logLik(gam5) # -50256.22
logLik(gam6) # -50528.10
logLik(gam7) # -50535.65

AIC(gam1) # 100595.9 (!)
AIC(gam2) # 100597.7 (!)
AIC(gam3) # 101124.2
AIC(gam4) # 100597.9 (!)
AIC(gam5) # 100599.9
AIC(gam6) # 101145.0
AIC(gam7) # 101159.8
# CONCLUSION: choose model with lowest AIC and minimal log-Likelihood -> gam 1-2-4

# SO, we take gam4, because it has the best characteristics on different measurements
gam_opt <- gam4

post_dt$cover <- Data$cover[1]
post_dt$ageph <- Data$ageph[1]
post_dt$power <- Data$power[1]
post_dt$fleet <- Data$fleet[1]     # Add variables to post_dt to run GAM_opt
post_dt$split <- Data$split[1]
post_dt$fuel <- Data$fuel[1]
post_dt$sexph <- Data$sexph[1]
post_dt$agecar <- Data$agecar[1]

pred_gam_spatial <- predict(gam_opt, newdata = post_dt, type = "terms", terms = "s(long,lat)")
# gives predicted frequency, based on gam_opt per city, based on its coordinates 

dt_pred <- data.frame(pc = post_dt$POSTCODE, long = post_dt$long,
                      lat = post_dt$lat, pred = pred_gam_spatial)
names(dt_pred)[4] <- "spatial_pred_freq"
shape_Belgium <- left_join(shape_Belgium, dt_pred, by = c("POSTCODE" = "pc"))
# adds long and lat coordinates per postal code to shape file 

ggplot(shape_Belgium) +
  geom_sf(aes(fill = spatial_pred_freq), colour = NA) +
  ggtitle("Claim frequency data - spatial - UNbinned") +
  scale_fill_gradient(low="#99CCFF", high="#003366") +
  theme_bw()

dt_pred <- dplyr::arrange(dt_pred, pc) # order based on numerical value of postal code

# PART 2: actually binning spatial factor in 'geo' in for-loop
AIC_comp <- c()
BIC_comp <- c()
breaks <- list()
crp <- colorRampPalette(c("#99CCFF", "#003366"))  

for(j in 2:20){
  num_bins <- j
  classint_fisher <- classIntervals(dt_pred$spatial_pred_freq, num_bins, style = "fisher")
  breaks[[j]] <- classint_fisher$brks
  shape_Belgium$class_fisher <- cut(shape_Belgium$spatial_pred_freq, 
                                    breaks = classint_fisher$brks, 
                                    right = FALSE, include.lowest = TRUE, 
                                    dig.lab = 2) 
  
  df_geo$geo <- as.factor(cut(df_geo$spatial_pred_freq, 
                              breaks = classint_fisher$brks, right = FALSE, 
                              include.lowest = TRUE, dig.lab = 2))
  
  AIC_comp[j] <- gam(freq ~ s(ageph) + geo + power + cover + fleet + split 
                + fuel + sexph + agecar, offset = log(expo), method = "REML", data = df_geo,
                family = poisson(link = "log"))$aic
  
  BIC_comp[j] <- BIC(gam(freq ~ s(ageph) + geo + power + cover + fleet + split 
                         + fuel + sexph + agecar, offset = log(expo), method = "REML", data = df_geo,
                         family = poisson(link = "log")))
}

names(AIC_comp) <- 1:20
names(BIC_comp) <- 1:20

AIC_comp
BIC_comp

AIC_comp == min(AIC_comp, na.rm = T)  # 15
BIC_comp == min(BIC_comp, na.rm = T)  # 11

breaks

# PART 3: using the results of the for-loop to adjust dataset 

num_bins <- 11 # Based on the lowest BIC value 
classint_fisher <- classIntervals(dt_pred$spatial_pred_freq, num_bins, style = "fisher")

classint_fisher$brks            # -0.465118789 -0.341533023 -0.239718291 -0.178467058 -0.121691203 -0.064612749
                                # -0.005696999  0.053323814  0.116731018  0.189453085  0.270987408  0.364478448
min(dt_pred$spatial_pred_freq)  # -0.4651188
max(dt_pred$spatial_pred_freq)  # 0.3644784

crp <- colorRampPalette(c("#99CCFF", "#003366"))  
plot(classint_fisher, crp(num_bins), xlab = expression(hat(f)(long,lat)), main = "Fisher binning for spatial effect on GAM prediction")

shape_Belgium$class_fisher <- cut(shape_Belgium$spatial_pred_freq, 
                                    breaks = classint_fisher$brks, 
                                    right = FALSE, include.lowest = TRUE, 
                                    dig.lab = 2) 

crp2 <- colorRampPalette(brewer.pal(7, "Reds"))(11)  # RED
crp3 <- colorRampPalette(brewer.pal(7, "Blues"))(11) # BLUE

ggplot(shape_Belgium) + theme_bw() + labs(fill = "Fisher") +  # RED
  geom_sf(aes(fill = class_fisher), colour = NA) +
  ggtitle("Frequency data - binned spatial") +
  scale_fill_manual(values = crp2, na.value = "white") +
  theme_bw()

ggplot(shape_Belgium) + theme_bw() + labs(fill = "Fisher") +  # BLUE
  geom_sf(aes(fill = class_fisher), colour = NA) +
  ggtitle("Frequency data - binned spatial") +
  scale_fill_manual(values = crp3, na.value = "white") +
  theme_bw()

# Adjust initial dataset 
df_geo$postal <- as.numeric(df_geo$postal)
df_geo <- Data %>% dplyr::select(freq,ageph,expo,power, cover, fleet, split, fuel, sexph, agecar,postal)
df_geo <- left_join(df_geo,dt_pred, by = c("postal" = "pc"))
df_geo$geo <- as.factor(cut(df_geo$spatial_pred_freq, 
                              breaks = classint_fisher$brks, right = FALSE, 
                              include.lowest = TRUE, dig.lab = 2))

Data$geo <- df_geo$geo
Data$geo <- factor(Data$geo, ordered = T, levels = levels(Data$geo), labels = levels(Data$geo))


### ---___---___---___---___---___---___---___---___---___---___---___---___---___---
### Binning ageph (Frequency)
### ---___---___---___---___---___---___---___---___---___---___---___---___---___---
freq_gam_opt_geo <- gam(freq ~ s(ageph) + geo + power + cover + fleet + split 
                           + fuel + sexph + agecar, offset = log(expo), 
                           method = "REML", data = data_train,
                           family = poisson(link = "log")) 
  # Use the optimal GAM found for spatial binning, with 'geo' adjusted 

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

gam_ageph <- getGAMdata_single(freq_gam_opt_geo, "s(ageph)", data_train$ageph, "ageph")
ctrl.freq <- evtree.control(alpha = 100, maxdepth = 5, 
                            minbucket = 0.05*nrow(data_train), 
                            minsplit = 0.10*nrow(data_train))
  # Alpha controls the complexity: higher alpha is higher penalty for complexity and thus simpler model
evtree_freq_ageph <- evtree(s ~ ageph, data = gam_ageph, weights = tot, 
                               control = ctrl.freq )
plot(evtree_freq_ageph)

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

freq_splits_ageph <- splits_evtree(evtree_freq_ageph, gam_ageph$ageph, 
                                    data_train$ageph)

freq_splits_ageph # 17 26 29 32 35 38 51 55 59 63 73 95

breaks_ageph <- freq_splits_ageph
group_ageph <- cut(Data$ageph,
                   breaks = breaks_ageph, include.lowest = T,
                   right = FALSE)

Data$agephGR <- group_ageph

# Set labels correctly
str(Data)
Data$agephGR <- factor(Data$agephGR, levels = levels(Data$agephGR), labels = levels(Data$agephGR), ordered = T)
Data$agecar <- factor(Data$agecar, levels = levels(Data$agecar), labels = levels(Data$agecar), ordered = T)
Data$sexph <- factor(Data$sexph, levels = levels(Data$sexph), labels = levels(Data$sexph), ordered = F)
Data$fuel <- factor(Data$fuel, levels = levels(Data$fuel), labels = levels(Data$fuel), ordered = F)
Data$split <- factor(Data$split, levels = levels(Data$split), labels = levels(Data$split), ordered = T)
Data$use <- factor(Data$use, levels = levels(Data$use), labels = levels(Data$use), ordered = F)
Data$fleet <- factor(Data$fleet, levels = levels(Data$fleet), labels = levels(Data$fleet), ordered = F)
Data$sportc <- factor(Data$sportc, levels = levels(Data$sportc), labels = levels(Data$sportc), ordered = F)
Data$cover <- factor(Data$cover, levels = levels(Data$cover), labels = levels(Data$cover), ordered = F)
Data$power <- factor(Data$power, levels = levels(Data$power), labels = levels(Data$power), ordered = T)
Data$geo <- factor(Data$geo, levels = levels(Data$geo), labels = levels(Data$geo), ordered = T)
str(Data)


### ---___---___---___---___---___---___---___---___---___---___---___---___---___---
### Making final data frame (Frequency)
### ---___---___---___---___---___---___---___---___---___---___---___---___---___---
FINALDF <- Data[,c(-1,-2,-14,-15)] # rm 'postal', cont. 'ageph', 'long', 'lat'
names(FINALDF)
str(FINALDF)
summary(FINALDF)

model_train <- FINALDF[trainIndex,]
model_test <- FINALDF[-trainIndex,]  


