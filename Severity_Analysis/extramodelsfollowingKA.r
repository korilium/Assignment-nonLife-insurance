

#####linear model  ####
lm <- lm(claimAm ~ agecar + sexph + fuel + split + 
          use + fleet + sportc + cover + power, data = train)
summary(lm)


### test glm and gam with Age 
a <- seq(min(train_nozero$ageph), max(train_nozero$ageph))

#glm age not binned 
glm_age <- glm(claimAm ~ ageph,
               data = train_nozero, family = Gamma(link = "log"))

pred_glm_age <- predict(glm_age, newdata = data.frame(ageph = a),
                          type = "terms", se.fit = TRUE)
b_glm_age <- pred_glm_age$fit
l_glm_age <- pred_glm_age$fit - qnorm(0.975) * pred_glm_age$se.fit
u_glm_age <- pred_glm_age$fit + qnorm(0.975) * pred_glm_age$se.fit
df <- data.frame(a, b_glm_age, l_glm_age, u_glm_age)

ggplot(df) +
geom_line(aes(x = a, y = b_glm_age)) +
geom_line(aes(x = a, y = l_glm_age), linetype = 3) +
geom_line(aes(x = a, y = u_glm_age), linetype = 3)

#glm age binned
# note the cut is done on A  as it needs the same factorization as age 
glm_grouped_age <- glm(claimAm ~ group_ageph,
                       data = train_nozero, family = Gamma(link = "log"))

pred_glm_grouped_age <- predict(glm_grouped_age,
                    newdata = data.frame(group_ageph = 
                    cut(a, breaks = breaks, include.lowest = T,
                    right = FALSE)), type = "terms", se.fit = TRUE)

b_glm_grouped_age <- pred_glm_grouped_age$fit
l_glm_grouped_age <- pred_glm_grouped_age$fit - qnorm(0.975) * pred_glm_grouped_age$se.fit
u_glm_grouped_age <- pred_glm_grouped_age$fit + qnorm(0.975) * pred_glm_grouped_age$se.fit
df <- data.frame(a, b_glm_grouped_age, l_glm_grouped_age, u_glm_grouped_age)

ggplot(df) +
geom_line(aes(x = a, y = b_glm_grouped_age)) +
geom_line(aes(x = a, y = l_glm_grouped_age), linetype = 3) +
geom_line(aes(x = a, y = u_glm_grouped_age), linetype = 3)

#gam age 
gam_age <- gam(claimAm ~ s(ageph), data = train_nozero, family = Gamma(link = "log"))
plot(gam_age, scheme = 1)