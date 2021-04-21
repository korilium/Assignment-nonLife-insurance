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

KUL <- "#116E8A"

# Making the data ready 
Data$chargtot <- NULL   # This analysis will only focus on frequency data
Data[,15] <- ordered(Data[,15], levels = c("<66","66-110",">110"))
colnames(Data) <- c("ageph","postal","expo","lnexpo","freq","freq_ann","agecar","sexph","fuel",
                    "split","use","fleet","sportc","cover","power")

# Getting a feel for the data
str(Data)
names(Data)
head(Data)
summary(Data)


# Plot relative frequency of variates
g_freq <- ggplot(Data, aes(freq))+
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = KUL, col = KUL, alpha = 0.7)+
  ggtitle("Relative Frequency - Number of claims")+
  labs(y = "Relative frequency", x = "freq")

g_expo <- ggplot(Data, aes(expo))+
  geom_histogram(aes(y = (..count..)/sum(..count..)), fill = KUL, col = KUL, alpha = 0.7, bins = 20)+
  ggtitle("Relative Frequency - Exposure")+
  labs(y = "Relative frequency", x = "expo")

g_ageph <- ggplot(Data, aes(ageph))+
  geom_histogram(aes(y = (..count..)/sum(..count..)), fill = KUL, col = KUL, binwidth = 2, alpha = 0.7)+
  ggtitle("Relative Frequency - Age policyholder")+
  labs(y = "Relative frequency", x = "ageph")

g_agecar <- ggplot(Data, aes(agecar))+
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = KUL, col = KUL, alpha = 0.7)+
  ggtitle("Relative Frequency - Age vehicle")+
  labs(y = "Relative frequency", x = "agecar")

g_sexph <- ggplot(Data, aes(sexph))+
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = KUL, col = KUL, alpha = 0.7)+
  ggtitle("Relative Frequency - Sex policyholder")+
  labs(y = "Relative frequency", x = "sexpg")

g_fuel <- ggplot(Data, aes(fuel))+
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = KUL, col = KUL, alpha = 0.7)+
  ggtitle("Relative Frequency - Fueltype")+
  labs(y = "Relative frequency", x = "fuel")

g_split <- ggplot(Data, aes(split))+
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = KUL, col = KUL, alpha = 0.7)+
  ggtitle("Relative Frequency - Payment split")+
  labs(y = "Relative frequency", x = "split")

g_use <- ggplot(Data, aes(use))+
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = KUL, col = KUL, alpha = 0.7)+
  ggtitle("Relative Frequency - Utilization of the vehicle")+
  labs(y = "Relative frequency", x = "use")

g_fleet <- ggplot(Data, aes(fleet))+
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = KUL, col = KUL, alpha = 0.7)+
  ggtitle("Relative Frequency - Part of fleet or not")+
  labs(y = "Relative frequency", x = "fleet")

g_sportc <- ggplot(Data, aes(sportc))+
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = KUL, col = KUL, alpha = 0.7)+
  ggtitle("Relative Frequency - Sportscar or not")+
  labs(y = "Relative frequency", x = "sportc")

g_cover <- ggplot(Data, aes(cover))+
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = KUL, col = KUL, alpha = 0.7)+
  ggtitle("Relative Frequency - Type of cover")+
  labs(y = "Relative frequency", x = "cover")

g_power <- ggplot(Data, aes(power))+
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = KUL, col = KUL, alpha = 0.7)+
  ggtitle("Relative Frequency - Power of car")+
  labs(y = "Relative frequency", x = "power")

grid.arrange(g_freq,g_expo,g_agecar,g_ageph,g_cover,g_fleet,g_fuel,g_power,g_sexph,g_split,g_sportc, ncol=4)

# Empirical frequency 
Data %>% summarize(emp_freq = sum(freq) / sum(expo))   # 0.1393355

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


# Frequency variable - descriptives 
freq_emp_mean <- sum(Data$freq) / sum(Data$expo)
freq_emp_var <- sum((Data$freq - freq_emp_mean*Data$expo)^2)/sum(Data$expo)


# Representing spatial data
shape_Belgium <-st_read(file.choose(), quiet = TRUE)
shape_Belgium<-st_transform(shape_Belgium, "+proj=longlat +datum=WGS84")

post_expo <- Data %>% group_by(postal) %>% summarize(num = n(), total_expo = sum(expo))
shape_Belgium <- left_join(shape_Belgium, post_expo, by = c("POSTCODE" = "postal"))
shape_Belgium$freq_area <- shape_Belgium$total_expo/shape_Belgium$Shape_Area
shape_Belgium$freq_class <- cut(shape_Belgium$freq_area,
                          breaks = quantile(shape_Belgium$freq_area, c(0,0.2,0.8,1), na.rm = TRUE),
                          right = FALSE, include.lowest = TRUE,
                          labels = c("low", "average", "high"))

g_shapefile <- ggplot(shape_Belgium) + 
  geom_sf(aes(fill = freq_class), colour = KUL, size = 0.1) +
  ggtitle("Claim frequency data") +
  labs(fill = "Relative\nexposure") +
  scale_fill_brewer(palette = "Reds", na.value = "white")

g_shapefile


# Checking the individual impact of each covariate on dependent variable 
# NOTE: when independent variable is a factor with >2 levels, R makes k-1 dummies for linear model 
lm(freq~agecar, data = Data)[1] 
lm(freq~ageph, data = Data)[1]    # negative (older less)
lm(freq~cover, data = Data)[1]    # positive 
lm(freq~fleet, data = Data)[1]    # negative (fleet less)
lm(freq~fuel, data = Data)[1]     # negative (petrol less)
lm(freq~power, data = Data)[1]    # positive (higher more)
lm(freq~sexph, data = Data)[1]    # negative (male less)
lm(freq~split, data = Data)[1]    # 
lm(freq~sportc, data = Data)[1]   # positive (sportscar more)
lm(freq~use, data = Data)[1]      # positive (professional more)

























# Plotting histograms or density plots for all variates 
ggplot(Data, aes(x=freq))+
  geom_bar(fill = KUL)+
  ggtitle("Claim Frequency")+
  labs(y = "Count", x = "Claim frequency during period of exposure")

ggplot(Data, aes(x=ageph))+
  geom_density(color = KUL)+
  ggtitle("")+
  labs(y = "Count", x = "Claim frequency during period of exposure")










# Quick linear model to see the sign of influence between variables on frequency
lm(freq~., data = Data)[1]
  # Obviously very small values, because a lot of 0-values in frequency


ggplot(Data, aes(x=freq),
       xlab = "Claim frequency in exposure",
       ylab = "Count")+
  geom_bar()+
  ggtitle("Histogram Claim Frequency")
ggplot(Data, aes(x=sexph))+
  geom_bar()

hist(Data$sexph)
hist(Data$ageph)
hist(Data$agecar)














































































