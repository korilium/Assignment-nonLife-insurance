

#students: Mathijs Gerits, Dieter Maes, Ignace Decocq
#student numbers: r848304, r0853946, r0811036



##### libraries #####
library(gridExtra)
library(ggplot2)
library(dplyr)
library(ClustOfVar)
library(PCAmixdata)
library(visdat)
library(httpgd)
library(skimr)
library(DataExplorer)
library(sf)
library(interactions)
hgd()
hgd_browse()

##### loading in data ##### 
data <- read.csv2(file = "toledo files/Assignment.csv", sep = ",")
dataM <- read.csv2(file = "Frequency_Analysis/Data.csv", sep = ",")


belgium_shape_sf <- st_read('Severity_Analysis//shape file Belgie postcodes//npc96_region_Project1.shp', quiet = TRUE)
belgium_shape_sf <- st_transform(belgium_shape_sf, "+proj=longlat +datum=WGS84")
class(belgium_shape_sf)

data$geo <- dataM$geo
data$group_ageph <- dataM$agephGR
names(data) <- c("ageph", "postal", "expo", "lnexpo", "freq", 
                "annfreq", "claimAm", "agecar", "sexph", "fuel",
                 "split", "use", "fleet", "sportc", "cover", "power",
                 "geo", "group_ageph"
)


#### setting to the right type ###
Data <- as.data.frame(data)
train <- Data %>%mutate(across(c(claimAm,  ageph), as.numeric)) %>%
 mutate(across(c(agecar, sexph, power, split, fuel, use, fleet, sportc, cover, group_ageph, geo), as.factor))

#### take elements with claimam and create logclaimam variable #### 
train_nozero <- subset(train, Data$claimAm != 0)
train_nozero$logclaimAm <- log(train_nozero$claimAm)

str(train_nozero)
dim(train_nozero)



######### exploratory data analysis ##############
#plot density claimAm 
ggplot(train_nozero, aes(x = claimAm)) +
geom_density() +
scale_x_continuous(trans = 'log2') +
geom_histogram(aes(y = ..density..), bins = 20,fill = "#116e8a", color = "white", alpha = 0.7) +
geom_rug()+
labs(x ="claim amount")

mean(train_nozero$claimAm)
sd(train_nozero$claimAm)
kurtosis(train_nozero$claimAm)
skewness(train_nozero$claimAm)

plot <- list()

for (i in colnames(train_nozero)) {
  plot[[i]] <- ggplot(data = train_nozero, mapping = aes_string(x = i, y = "logclaimAm")) +
  geom_jitter(col = '#116e8a', alpha = 0.2) +
  geom_boxplot(, fill = "#05039e", color = "black", alpha = 0.3) +
    labs(x = i) +
  guides(color = FALSE) +
  theme_minimal()
}

grid.arrange(plot[[4]],plot[[5]], plot[[6]], plot[[7]],
            plot[[8]],plot[[9]], plot[[10]], plot[[11]],
             plot[[12]],plot[[14]], ncol = 3)


#map of balgium 
post_expo <- train_nozero %>% group_by(postal) %>% summarize(tot_claimAm = sum(claimAm))
post_expo %>% slice(1:5)

belgium_shape_sf <- left_join(belgium_shape_sf, post_expo, by = c("POSTCODE" = "postal"))

belgium_shape_sf$claimAm <- belgium_shape_sf$tot_claimAm/belgium_shape_sf$Shape_Area

belgium_shape_sf$claimAm_class <- cut(belgium_shape_sf$claimAm,
                                   breaks = quantile(belgium_shape_sf$claimAm,
                                   c(0.2,0.5,0.8,0.9,0.95,0.99, 1), na.rm = TRUE),
                                   right = FALSE, include.lowest = TRUE,
                                   labels = c("0.2","0.5","0.8", "0.9", "0.95", "0.99"))

ggplot(belgium_shape_sf) +
geom_sf(aes(fill = claimAm_class), colour = "black", size = 0.1) +
ggtitle("claim amount per county") +
labs(fill = "relative claim amount") +
scale_fill_brewer(palette = "Blues", na.value = "white") +
theme_bw()

#interaction effects 

train_nozero1 <- train_nozero%>% select(-logclaimAm)
Data_group <- splitmix(train_nozero)
Data_quanti <- train_nozero[Data_group$col.quant]
Data_quali <- train_nozero[Data_group$col.qual]

tree <- hclustvar(X.quanti = Data_quanti, X.quali = Data_quali)
plot(tree)

