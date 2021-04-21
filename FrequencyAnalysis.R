### ---___---___---___---___---___---___---___---___---___---___---___---___---___---
### Exploratory data analysis (Frequency)
### ---___---___---___---___---___---___---___---___---___---___---___---___---___---

# Making the data ready 
Data$chargtot <- NULL   # This analysis will only focus on frequency data
Data[,15] <- ordered(Data[,15], levels = c("<66","66-110",">110"))
colnames(Data) <- c("ageph","postal","expo","lnexpo","freq","freq_ann","agecar","sexph","fuel",
                    "split","use","fleet","sportc","cover","power")


# Getting a feel for the data
str(Data)
head(Data)
summary(Data)






















































































