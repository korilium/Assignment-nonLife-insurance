
library(gridExtra)
library(ggplot2)
library(dplyr)
library(ClustOfVar)
library(PCAmixdata)
library(visdat)
library(httpgd)
library(skimr)
library(DataExplorer)
hgd()
hgd_browse()

data <- read.csv2(file = "Severity_Analysis/train.csv", sep = ",")

# factorize 
Data <- as.data.frame(data)
train <- Data %>% select(-X) %>%
 mutate(across(c(claimAm, lat, long, logclaimAm), as.numeric)) %>%
 mutate(across(c(agecar, sexph, power, split, fuel, use, fleet, sportc, cover, ageph, group_ageph), as.factor))

str(train)
dim(train)

######### exploratory data analysis ##############
#plot density claimAm 
ggplot(train, aes(x = claimAm)) +
geom_density() +
scale_x_continuous(trans = 'log2') +
geom_histogram(aes(y = ..density..), fill = "#116e8a", color = "white", alpha = 0.7) +
geom_rug()

plot <- list()

for (i in colnames(train)) {
  plot[[i]] <- ggplot(data = train, mapping = aes_string(x = i, y = "logclaimAm")) +
  geom_jitter(aes(color = 'red'), alpha = 0.2) +
  geom_boxplot(, fill = "#116e8a", color = "black", alpha = 0.3) +
    labs(x = i) +
  guides(color = FALSE) +
  theme_minimal()
}

grid.arrange(plot[[17]], plot[[9]], plot[[4]],
            plot[[5]], plot[[6]], plot[[7]],
            plot[[8]], plot[[10]], plot[[11]], plot[[12]], ncol = 3)


#map of balgium 
post_expo <- train %>% group_by(postal) %>% summarize(tot_freq = sum(claimAm))
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

#basic plots for variables

train %>% plot_bar()
train %>% plot_histogram()


Data_group <- splitmix(train)
Data_quanti <- train[Data_group$col.quant]
Data_quali <- train[Data_group$col.qual]

tree <- hclustvar(X.quanti = Data_quanti, X.quali = Data_quali)
plot(tree)

# mean, variance skewnnes and kurtosis
sapply(Data_quanti, mean)
sapply(Data_quanti, var)
sapply(Data_quanti, skewness)
sapply(Data_quanti, kurtosis)
