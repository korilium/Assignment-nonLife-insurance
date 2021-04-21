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
hgd()
hgd_browse()
KULbg <- "#116e8a"



#setting op same datavariable with claim amount instead of freq 
toledo <- read.csv("toledo files//Assignment.csv", ",", header = T)
Mdata <- read.csv("data.csv", ",", header = T)
Mdata$claimAm <- toledo$chargtot
data <- Mdata %>% select(-freq, - freq_ann)

#set to same factor as rdataset
data <- as.data.frame(data)
Data <- data %>%
 mutate(across(c(X,ageph, expo, lnexpo, postal), as.numeric)) %>%
 mutate(across(c(agecar, sexph,power, split,  fuel, use, fleet, sportc, cover), as.factor))

Data <- as_tibble(Data)

# exploratory data analysis 
ggplot(Data, aes(x = claimAm)) +
geom_density() +
scale_x_continuous(trans = 'log2')
Data %>% plot_bar()
Data %>% plot_histogram()

Data <- as.data.frame(Data)
Data_group<- splitmix(Data)
Data_quanti <- Data[Data_group$col.quant]
Data_quali <- Data[Data_group$col.qual]

tree <- hclustvar(X.quanti = Data_quanti, X.quali = Data_quali)

dim(Data)

Data %>% group_by(sexph)%>% summarize(emp_m_claimAm = sum(claimAm)/sum(expo))
Data %>% group_by(sexph) %>%summarise(emp_var_claimAm = sum((claimAm - sum(claimAm)/sum(expo)*claimAm)^2)/sum(expo))

#order the power and split
Data$power <- ordered(data$power, levels = c("<66", "66-110", ">110"))
Data$split <- ordered(data$split, levels = c("Once", "Twice", "Thrice", "Monthly"))
