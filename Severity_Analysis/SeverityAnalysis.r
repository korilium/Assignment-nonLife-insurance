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

Data_nozero <- Data %>% select(-X, -expo, -lnexpo)

Data_nozero <- subset(Data_nozero, Data$claimAm != 0)
dim(Data_nozero)

#split data in test and train data with stratified sampling 
# the dependend variable is factorized 
set.seed(666)
trainIndex <- createDataPartition(Data_nozero$claimAm, p=0.8, list = FALSE, times=1, group = 10)

train <- Data_nozero[trainIndex,]
test <- Data_nozero[-trainIndex, ]


# exploratory data analysis 
ggplot(train, aes(x = claimAm)) +
geom_density() +
scale_x_continuous(trans = 'log2')
train %>% plot_bar()
train %>% plot_histogram()


Data_group<- splitmix(Data_nozero)
Data_quanti <- Data_nozero[Data_group$col.quant]
Data_quali <- Data_nozero[Data_group$col.qual]

tree <- hclustvar(X.quanti = Data_quanti, X.quali = Data_quali)
plot(tree)

# mean, variance skewnnes and kurtosis
sapply(Data_quanti, mean)
sapply(Data_quanti, var)
sapply(Data_quanti, skewness)
sapply(Data_quanti, kurtosis)

Data %>% group_by(sexph)%>% summarize(emp_m_claimAm = sum(claimAm)/sum(expo))
Data %>% group_by(sexph) %>%summarise(emp_var_claimAm = sum((claimAm - sum(claimAm)/sum(expo)*claimAm)^2)/sum(expo))




#order the power and split
#Data$power <- ordered(data$power, levels = c("<66", "66-110", ">110"))
#Data$split <- ordered(data$split, levels = c("Once", "Twice", "Thrice", "Monthly"))



# GLM model to fit the data using gamma distribution


lm <- lm(claimAm ~ ., data= Data_nozero)
lm %>% broom::tidy()


glm <- glm(claimAm ~ ., offset = log(expo), family = gaussian(), data = Data_nozero)

glm %>% broom::tidy()
glm %>% broom::augment(type.predict = "response")
summary(glm)
plot(glm)
Data[11749, ]
glm <- glm(claimAm ~ postal + power + cover + sportc +  fleet + use , offset = log(expo), family = Gamma(link ="inverse"), data = Data_nozero)
plot(glm)

gam(claimAm ~ postal, family = gaussian(), data =Data)


