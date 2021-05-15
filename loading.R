library(ggplot2)
library(dplyr)
library(DataExplorer)
library(tidyverse)
library(infer)

#data

prem <-  read.csv2(file = "pure_premium.csv", sep = ",")
premium <- prem %>% mutate(pred_GLM_full = as.numeric(pred_GLM_full),
                           pred_sev_glm = as.numeric(pred_sev_glm))
premium$Pure_premium <- premium$pred_GLM_full*premium$pred_sev_glm
str(premium)
sum_pure_premium <- sum(premium$Pure_premium)

# bootstrap 
med_pp <- premium %>%
  # Specify the variable of interest
  specify(response = Pure_premium) %>%  
  # Generate bootstrap samples
  generate(reps = 2500, type = "bootstrap") %>%
  # Calculate the median of each bootstrap sample
  calculate(stat = "sum")
 
# View its structure
str(med_pp)
med_pp %>% summarize(l = quantile(stat, 0.005),
                     u = quantile(stat, 0.995))
# phi 
C <- med_pp %>% summarise( u = quantile(stat, 0.995))

phi <- (C - sum_pure_premium)/sum_pure_premium


# risk loading and risk premium 

premium$risk_loading <- premium$Pure_premium*0.003698552
premium$risk_premium <- premium$Pure_premium + premium$risk_loading

