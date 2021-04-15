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


data  <- read.csv("assignment.csv", ",", header = T)

# exploratory data analysis 

data %>% vis_dat(warn_large_data = F)
data %>% skim()
data %>% plot_bar()
data %>% plot_histogram