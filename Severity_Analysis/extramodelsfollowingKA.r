

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
#####################################################
#spatial effects without binning 
gam_spatial <- gam(claimAm ~s(LONG, LAT, bs = "tp"), family = Gamma(link="log"), data = train_nozero)
plot(gam_spatial, scheme = 2)

#xtracting long and lat 
post_dt <- st_centroid(belgium_shape_sf)
post_dt$LONG<- do.call(rbind, post_dt$geometry)[,1]
post_dt$LAT <- do.call(rbind, post_dt$geometry)[,2]

#create prediction dataframe 
pred  <- predict(gam_spatial, newdata = post_dt, type= "terms", 
                 terms = 's(LONG,LAT)')
pred_dt <- data.frame(pc = post_dt$POSTCODE, 
                      LONG= post_dt$LONG, 
                      LAT = post_dt$LAT, pred)
names(pred_dt)[4] <- "fit_spatial"
belgium_shape_sf <- left_join(belgium_shape_sf, 
                              pred_dt, by = c("POSTCODE" = "pc"))


#plot result 
tm_shape(belgium_shape_sf) + 
  tm_borders(col = 'white', lwd = .1 )+
  tm_fill("fit_spatial", style = "cont",
  palette = "RdBu", legend.reverse = TRUE, 
  midpoint = TRUE) + 
  tm_layout(legend.title.size = 1.0 , 
            legend.text.size = 1.0 )


#### binning 
belgium_shape_sf <- st_read("Severity_Analysis//shape file Belgie postcodes//npc96_region_Project1.shp", quiet = TRUE)
belgium_shape_sf <- st_transform(belgium_shape_sf, "+proj=longlat +datum=WGS84")
class(belgium_shape_sf)
inspost <- read_excel("toledo files//inspost.xls")
# name change
Mdata$claimAm <- toledo$chargtot
# ann freq delete
data <- Mdata %>% select(-freq)
# extracting long and lat
post_dt <- st_centroid(belgium_shape_sf)
post_dt <- st_make_valid(post_dt)
post_dt$LONG <- do.call(rbind, post_dt$geometry)[, 1]
post_dt$LAT <- do.call(rbind, post_dt$geometry)[, 2]

# adding long an lat from special dataframe post_dt (can take 5 min to load )


for (i in 1:length(data$postal)) {
  data$LONG[i] <- inspost$LONG[data$postal[i] == inspost$CODPOSS]
  data$LAT[i] <- inspost$LAT[data$postal[i] == inspost$CODPOSS]
}

# factor as rdataset
data <- as.data.frame(data)
Data <- data %>%
  mutate(across(c(X, ageph, expo, lnexpo, postal), as.numeric)) %>%
  mutate(across(c(agecar, sexph, power, split, fuel, use, fleet, sportc, cover), as.factor)) %>%
  select(-X, -expo, -lnexpo)


# remove elements with no claimamount
data_nozero <- subset(Data, Data$claimAm != 0)
dim(data_nozero)


################# binning age and spatial  ####################################

# finding optimal GAM for binning spatial variable by hand 
gam1 <- gam(claimAm ~ cover + fuel + power + s(ageph)+
agecar + sexph + split + use + fleet + sportc 
+ s(LONG,LAT) ,  family = Gamma(link = "log"), 
data = data_nozero, method = 'REML'
)
summary(gam1)

#remove use 
gam2 <- gam(claimAm ~ cover + fuel + power + s(ageph)+
agecar + sexph + split + fleet + sportc 
+ s(LONG,LAT) ,  family = Gamma(link = "log"), 
data = data_nozero, method = 'REML'
)
summary(gam2)
#remove fleet 

gam3 <- gam(claimAm ~ cover + fuel + power + s(ageph)+
agecar + sexph + split + sportc 
+ s(LONG,LAT) ,  family = Gamma(link = "log"), 
data = data_nozero, method = 'REML'
)
summary(gam3)

#remove power 
gam4 <- gam(claimAm ~ cover + fuel + s(ageph)+
agecar + sexph + split + sportc 
+ s(LONG,LAT) ,  family = Gamma(link = "log"), 
data = data_nozero, method = 'REML'
)
summary(gam4)

#remove sexph 
gam5 <- gam(claimAm ~ cover + fuel + s(ageph)+
agecar  + split + sportc 
+ s(LONG,LAT) ,  family = Gamma(link = "log"), 
data = data_nozero, method = 'REML'
)
summary(gam5)

#return to GAM2 add ageph by power 
gam6 <- gam(claimAm ~ cover + fuel + power + s(ageph, by=power)+
agecar + sexph + split + fleet + sportc 
+ s(LONG,LAT) ,  family = Gamma(link = "log"), 
data = data_nozero, method = 'REML'
)
summary(gam6)
#remove sportc 
gam7 <- gam(claimAm ~ cover + fuel + power + s(ageph, by=power)+
agecar + sexph + split + fleet 
+ s(LONG,LAT) ,  family = Gamma(link = "log"), 
data = data_nozero, method = 'REML'
)
summary(gam7)

#change ageph by split 
gam8 <- gam(claimAm ~ cover + fuel + power + s(ageph, by=split)+
agecar + sexph + split + fleet 
+ s(LONG,LAT) ,  family = Gamma(link = "log"), 
data = data_nozero, method = 'REML'
)
summary(gam8)

## comparing models 

logLik(gam1) # -50251.48 (!)
logLik(gam2) # -50252.16 (!)
logLik(gam3) # -50514.78
logLik(gam4) # -50254.22 (!)
logLik(gam5) # -50256.22
logLik(gam6) # -50528.10
logLik(gam7) # -50535.65
logLik(gam8)

AIC(gam1) #302638.3
AIC(gam2) #302636.5 (!)
AIC(gam3) #302640.4
AIC(gam4) #302663.4
AIC(gam5) #302675.9 
#AIC is only increasing start from gam2 
AIC(gam6) #302555.3
AIC(gam7) #302554.4
AIC(gam8) #



post_dt["cover"] <- as.factor(data_nozero$cover[1])
post_dt["fuel"] <- as.factor(data_nozero$fuel[1])
post_dt["power"] <- as.factor(data_nozero$power[1])

# new predict based on binning
pred <- predict(gam_spatial,
  newdata = post_dt, type = "terms",
  terms = "s(LONG,LAT)"
)


pred_dt <- data.frame(
  pc = post_dt$POSTCODE,
  LONG = post_dt$LONG,
  LAT = post_dt$LAT, pred
)
names(pred_dt)[4] <- "fit_spatial"

belgium_shape_sf <- left_join(belgium_shape_sf, pred_dt, by = c("POSTCODE" = "pc"))

# creating dataframe with binned spatial effect
data_geo <- data_nozero
data_geo <- left_join(data_geo, pred_dt, by = c("postal" = "pc"))

# find optimal number of bins
AIC_comp <- c()
BIC_comp <- c()
breaks <- list()
crp <- colorRampPalette(c("#99CCFF", "#003366"))

for (j in 2:15) {
  num_bins <- j
  classint_fisher <- classIntervals(pred_dt$fit_spatial,
    num_bins,
    style = "fisher"
  )

  breaks[[j]] <- classint_fisher$brks


  belgium_shape_sf$classint_fisher <- cut(pred_dt$fit_spatial,
    breaks = classint_fisher$brks, right = FALSE,
    include.lowest = TRUE, dig.lab = 2
  )



  data_geo$geo <- as.factor(cut(data_geo$fit_spatial,
    breaks = classint_fisher$brks, right = FALSE,
    include.lowest = TRUE, dig.lab = 2
  ))

  AIC_comp[j] <- gam(claimAm ~ s(ageph) + geo + power + cover + fleet + split +
    fuel + sexph + agecar + use + sportc,
  method = "REML", data = data_geo,
  family = Gamma(link = "log")
  )$aic
  BIC_comp[j] <- BIC(gam(claimAm ~ s(ageph) + geo + power + cover + fleet + split +
    fuel + sexph + agecar + use + sportc,
  method = "REML", data = data_geo,
  family = Gamma(link = "log")
  ))
}

plot(1:15, AIC_comp, type = "l")
plot(1:15, BIC_comp, type = "l")

# create bins for spatial data using optimal bins = 12 following BIC
num_bins <- 12
classint_fisher <- classIntervals(pred_dt$fit_spatial,
  num_bins,
  style = "fisher"
)

breaks <- classint_fisher$brks


belgium_shape_sf$classint_fisher <- cut(belgium_shape_sf$fit_spatial,
  breaks = classint_fisher$brks, right = FALSE,
  include.lowest = TRUE, dig.lab = 2
)



data_geo$geo <- as.factor(cut(data_geo$fit_spatial,
  breaks = classint_fisher$brks, right = FALSE,
  include.lowest = TRUE, dig.lab = 2
))


crp <- colorRampPalette(c("#99CCFF", "#003366"))
plot(classint_fisher, crp(num_bins),
  xlab = expression(hat(f)(long, lat)),
  main = "Fisher"
)

belgium_shape_sf$class_fisher <- cut(belgium_shape_sf$fit_spatial,
  breaks = classint_fisher$brks, right = FALSE, include.lowest = TRUE, dig.lab = 2
)


crp <- colorRampPalette(c("#99CCFF", "#003366"))

colourCount <- length(unique(belgium_shape_sf$class_fisher))
getPalette <- colorRampPalette(brewer.pal(9, "Blues"))(length(unique(belgium_shape_sf$class_fisher)))

ggplot(belgium_shape_sf) +
  theme_bw() +
  labs(fill = "Fisher") +
  geom_sf(aes(fill = class_fisher), colour = NA) +
  ggtitle("MTPLclaimseveritydata") +
  scale_fill_manual(values = getPalette, na.value = "white") +
  theme_bw()

# test
freq_glm_geo <- glm(claimAm ~ cover + fuel + geo, data = data_geo, family = Gamma(link = "log"))

# binning age

claimAm_gam_geo <- gam(claimAm ~ cover + fuel + s(ageph) + geo, data = data_geo, family = Gamma(link = "log"))
# getting the dataset for age
getGAMdata_single <- function(model, term, var, varname) {
  pred <- predict(model, type = "terms", terms = term)
  dt_pred <- tibble("x" = var, pred)
  dt_pred <- arrange(dt_pred, x)
  names(dt_pred) <- c("x", "s")
  dt_unique <- unique(dt_pred)
  dt_exp <- dt_pred %>%
    group_by(x) %>%
    summarize(tot = n())
  dt_exp <- dt_exp[c("x", "tot")]
  GAM_data <- left_join(dt_unique, dt_exp)
  names(GAM_data) <- c(varname, "s", "tot")
  GAM_data <- GAM_data[which(GAM_data$tot != 0), ]
  return(GAM_data)
}

gam_ageph <- getGAMdata_single(claimAm_gam_geo, "s(ageph)", data_geo$ageph, "ageph")

ctrl.freq <- evtree.control(alpha = 77, maxdepth = 5)

evtree_claimAm_ageph <- evtree(s ~ ageph,
  data = gam_ageph,
  weights = tot,
  control = ctrl.freq
)

plot(evtree_claimAm_ageph)


# extract the points from the tree model
splits_evtree <- function(evtreemodel, GAMvar, DTvar) {
  preds <- predict(evtreemodel, type = "node")
  nodes <- data.frame("x" = GAMvar, "nodes" = preds)
  nodes$change <- c(0, pmin(1, diff(nodes$nodes)))
  splits_evtree <- unique(c(
    min(DTvar),
    nodes$x[which(nodes$change == 1)],
    max(DTvar)
  ))
  return(splits_evtree)
}

claimAm_splits_ageph <- splits_evtree(
  evtree_claimAm_ageph,
  gam_ageph$ageph,
  data_nozero$ageph
)
claimAm_splits_ageph

breaks <- claimAm_splits_ageph
group_ageph <- cut(data_nozero$ageph,
  breaks = breaks, include.lowest = T,
  right = FALSE
)

data_nozero$group_ageph <- group_ageph

data_geo$group_ageph <- group_ageph
# test
freq_glm_geo <- glm(claimAm ~ cover + fuel + geo + group_ageph, data = data_geo, family = Gamma(link = "log"))

