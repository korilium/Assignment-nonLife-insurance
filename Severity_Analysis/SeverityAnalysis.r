#Students: Mathijs Gerits, Dries Maes and Ignace Decocq
#student numbers: r0811036
library(ggplot2)
library(dplyr)
library(visdat)
library(httpgd)
library(skimr)
library(DataExplorer)
hgd()
hgd_browse()
KULbg <- "#116e8a"




data  <- read.csv("assignment.csv", ",", header = T)

# exploratory data analysis 

data %>% vis_dat(warn_large_data = F)
str(data)
# set to the right class 
ggplot(data, aes(x=chargtot))+
geom_density()+
scale_x_continuous(trans='log2')


data %>% plot_bar()
data %>% plot_histogram
