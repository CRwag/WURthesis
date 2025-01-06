library(tidyverse)
library(tidycensus)
library(terra)
library(tigris)
library(sf)
library(ggplot2)
library(cem)
library(mgcv)
library(car)
library(pscl)
library(glmulti)



master.raster <- read_rds("~/Wageningen/University/Thesis/Data/1. Modified Data/master_raster_df_v2.rds")

#creating binary solar variable
master.raster <- master.raster %>%
  mutate(Solar_binary = ifelse(is.na(Solar_MW) | Solar_MW == 0, 0, 1))

names(master.raster)

#prepping master.raster_reduced for CEM
master.raster_reduced <- master.raster %>%
  select(cell, x, y, Solar_binary, Land_usage_cat, State, aspect_cardinal, slope,
         HVline_dist, substation_dist, nsrdb3_ghi, landval_ln)

#freeing up RAM
remove(master.raster)
gc()

#further prep/checks for CEM
master.raster_reduced %>%
  summarise(
    slope_min = min(slope, na.rm = TRUE),
    slope_max = max(slope, na.rm = TRUE),
    slope_mean = mean(slope, na.rm = TRUE),
    
    HVline_dist_min = min(HVline_dist, na.rm = TRUE),
    HVline_dist_max = max(HVline_dist, na.rm = TRUE),
    HVline_dist_mean = mean(HVline_dist, na.rm = TRUE),
    
    substation_dist_min = min(substation_dist, na.rm = TRUE),
    substation_dist_max = max(substation_dist, na.rm = TRUE),
    substation_dist_mean = mean(substation_dist, na.rm = TRUE)
  )

master.raster_reduced <- master.raster_reduced %>% 
  na.omit()

str(master.raster_reduced)
master.raster_reduced$Land_usage_cat <- factor(master.raster_reduced$Land_usage_cat)
master.raster_reduced$aspect_cardinal <- factor(master.raster_reduced$aspect_cardinal)

#CEM model
cem_model <- cem(
  treatment = "Solar_binary",
  data = master.raster_reduced,
  drop = c("cell", "x", "y"),
  cutpoints = list(
    slope = c(0, 2, 4, 6, 8, 10),
    HVline_dist = c(0, 500, 1000, 1500, 2000, 2500, 3000, 3500, 4000, 4500, 5000, 7500, 10000, 15000),
    substation_dist = c(0, 500, 1000, 1500, 2000, 2500, 3000, 3500, 4000, 4500, 5000, 7500, 10000, 15000),
    landval_ln = c(0, 7, 7.5, 8, 8.5, 9, 9.5, 10, 10.5, 11, 11.5, 12, 12.5, 13, 13.5, 14, 14.5, 15 ),
    nsrdb3_ghi = c(2.5, 3.5, 3.7, 3.9, 4.1, 4.3, 4.5, 4.7, 4.9, 5.1, 5.3, 5.5, 5.7, 6)
  )
)

cem_model # I think the matching ended up being too strict here

#CEM model V2
cem_model_v2 <- cem(
  treatment = "Solar_binary",
  data = master.raster_reduced,
  drop = c("cell", "x", "y"),
  cutpoints = list(
    slope = c(0, 2, 4, 6, 8, Inf),
    HVline_dist = c(0, 500, 1000, 2500, 5000, 10000, 20000, Inf),
    substation_dist = c(0, 500, 1000, 2500, 5000, 10000, 20000, Inf),
    landval_ln = c(0, 8, 9, 10, 11, 12, 13, 14, Inf),
    nsrdb3_ghi = c(2.5, 3.5, 3.9, 4.3, 4.7, 5.1, 5.5, 5.9, Inf)
  )
)

cem_model_v2

#Checking which treatment observations did not get a match
matched <-  as.data.frame(cem_model_v2[["matched"]])

matchcheck <- master.raster_reduced %>%
  bind_cols(matched)

names(matchcheck)

matchcheck %>%
  filter(`cem_model_v2[["matched"]]` == FALSE, Solar_binary == 1) %>%
  summary()

#Third CEM, excluding state variable
cem_model_v3 <- cem(
  treatment = "Solar_binary",
  data = master.raster_reduced,
  drop = c("cell", "x", "y", "State"),
  cutpoints = list(
    slope = c(0, 2, 4, 6, 8, Inf),
    HVline_dist = c(0, 500, 1000, 2500, 5000, 10000, 20000, Inf),
    substation_dist = c(0, 500, 1000, 2500, 5000, 10000, 20000, Inf),
    landval_ln = c(0, 8, 9, 10, 11, 12, 13, 14, Inf),
    nsrdb3_ghi = c(2.5, 3.5, 3.9, 4.3, 4.7, 5.1, 5.5, 5.9, Inf)
  )
)


#xtables

xtabs(~ Solar_binary + Land_usage_cat, data = master.raster_reduced)
xtabs(~ Solar_binary + aspect_cardinal, data = master.raster_reduced)


cem_model_v3

write_rds(cem_model_v3,"~/Wageningen/University/Thesis/Data/1. Modified Data/cem_model_v3.rds")

#Turning into data frame in prep for regression
matched_v3 <-  as.data.frame(cem_model_v3[["matched"]])

master.raster_reduced_wcem <- master.raster_reduced %>%
  bind_cols(matched_v3)

Matched_df <- master.raster_reduced_wcem %>%
  filter(`cem_model_v3[[\"matched\"]]` == TRUE)

write_rds(Matched_df, "~/Wageningen/University/Thesis/Data/1. Modified Data/Matched_df.rds")
write_rds(logit_reg_model, "~/Wageningen/University/Thesis/Data/1. Modified Data/logit_reg_model.rds")
write_rds(logit_reg_model_v2, "~/Wageningen/University/Thesis/Data/1. Modified Data/logit_reg_model_v2.rds")
#-----------------------------------
#Regression

head(Matched_df)
summary(Matched_df)

#First model
logit_reg_model <- glm(
  Solar_binary ~ 
    Land_usage_cat + State + aspect_cardinal + slope + 
    HVline_dist + substation_dist + nsrdb3_ghi + landval_ln,
  data = Matched_df,
  family = binomial(link = "logit")
)

summary(logit_reg_model)

#checking multicolliniarity
vif(logit_reg_model)
cor(Matched_df[, c("nsrdb3_ghi", "slope", "HVline_dist", "substation_dist", "landval_ln")])

##I think Hvline_dist can probably be removed.
##It is moderately correlated with substation dist (~.61), and is not statistically significant in the reg model.

pR2(logit_reg_model)

#Second model - left out "State", as most of the effect from "State" is likely due to regulation/political differences
logit_reg_model_v2 <- glm(
  Solar_binary ~ 
    Land_usage_cat + aspect_cardinal + slope + 
    HVline_dist + substation_dist + nsrdb3_ghi + landval_ln,
  data = Matched_df,
  family = binomial(link = "logit")
)

summary(logit_reg_model_v2)
pR2(logit_reg_model_v2)


#Sampling
set.seed(5555)

ones <- Matched_df %>%
  filter(Solar_binary == 1)

zeros <- Matched_df %>%
  filter(Solar_binary == 0) %>%
  group_by(Land_usage_cat) %>%
  sample_frac(0.02) %>%
  ungroup()

sampled_df <- bind_rows(ones, zeros)


summary(Matched_df)
summary(sampled_df)


#Using glmulti to find the best model given these variables
best_model_sample_df <- glmulti(Solar_binary ~ 
                        Land_usage_cat + aspect_cardinal + slope + 
                        substation_dist + nsrdb3_ghi + landval_ln, 
                      data = sampled_df, 
                      family = binomial(link = "logit"),
                      method = "g",
                      crit = "aic",
                      level = 2, 
                      confsetsize = 10,
                      report = T,
                      conseq = 2)

write_rds(best_model,"~/Wageningen/University/Thesis/Data/1. Modified Data/2. Models/glmulti_supertiny.rds")
write_rds(best_model_sample_df,"~/Wageningen/University/Thesis/Data/1. Modified Data/2. Models/glmulti_sample_df.rds")
summary(best_model_sample_df)
best_model_sample_df@formulas

#Testing best models on full data set
# Model 1
t1 <- glm(
  formula = Solar_binary ~ 1 + aspect_cardinal + substation_dist + nsrdb3_ghi + 
    landval_ln + nsrdb3_ghi:substation_dist + landval_ln:slope + 
    landval_ln:substation_dist + Land_usage_cat:nsrdb3_ghi + 
    Land_usage_cat:landval_ln + aspect_cardinal:landval_ln,
  data = Matched_df,
  family = binomial(link = "logit")
)

# Model 2
t2 <- glm(
  formula = Solar_binary ~ 1 + aspect_cardinal + slope + substation_dist + 
    nsrdb3_ghi + landval_ln + nsrdb3_ghi:substation_dist + landval_ln:substation_dist + 
    Land_usage_cat:nsrdb3_ghi + Land_usage_cat:landval_ln + aspect_cardinal:landval_ln,
  data = Matched_df,
  family = binomial(link = "logit")
)

# Model 3
t3 <- glm(
  formula = Solar_binary ~ 1 + aspect_cardinal + substation_dist + nsrdb3_ghi + 
    landval_ln + nsrdb3_ghi:slope + nsrdb3_ghi:substation_dist + 
    landval_ln:substation_dist + Land_usage_cat:nsrdb3_ghi + 
    Land_usage_cat:landval_ln + aspect_cardinal:landval_ln,
  data = Matched_df,
  family = binomial(link = "logit")
)

# Model 4
t4 <- glm(
  formula = Solar_binary ~ 1 + aspect_cardinal + substation_dist + nsrdb3_ghi + 
    landval_ln + nsrdb3_ghi:substation_dist + landval_ln:substation_dist + 
    Land_usage_cat:nsrdb3_ghi + Land_usage_cat:landval_ln + aspect_cardinal:landval_ln,
  data = Matched_df,
  family = binomial(link = "logit")
)

# Model 5
t5 <- glm(
  formula = Solar_binary ~ 1 + aspect_cardinal + substation_dist + nsrdb3_ghi + 
    landval_ln + nsrdb3_ghi:slope + nsrdb3_ghi:substation_dist + 
    landval_ln:slope + landval_ln:substation_dist + Land_usage_cat:nsrdb3_ghi + 
    Land_usage_cat:landval_ln + aspect_cardinal:landval_ln,
  data = Matched_df,
  family = binomial(link = "logit")
)

aic_values <- c(
  t0 = AIC(logit_reg_model_v2),
  t1 = AIC(t1),
  t2 = AIC(t2),
  t3 = AIC(t3),
  t4 = AIC(t4),
  t5 = AIC(t5)
)
print(aic_values)


#Testing best models from the sample_df glmulti

# Fit the models to the dataset
t1.1 <- glm(
  formula = Solar_binary ~ 1 + Land_usage_cat + aspect_cardinal + slope +
    nsrdb3_ghi + aspect_cardinal:Land_usage_cat + substation_dist:slope +
    aspect_cardinal:nsrdb3_ghi,
  data = Matched_df,
  family = binomial(link = "logit")
)
summary(t1.1)
pR2(t1.1)
write_rds(t1.1, "~/Wageningen/University/Thesis/Data/1. Modified Data/2. Models/GLmulti Matched_df tests/t1.1")


t2.1 <- glm(
  formula = Solar_binary ~ 1 + Land_usage_cat + aspect_cardinal + slope +
    substation_dist + nsrdb3_ghi + landval_ln + aspect_cardinal:Land_usage_cat +
    substation_dist:slope + nsrdb3_ghi:slope + nsrdb3_ghi:substation_dist +
    landval_ln:slope + landval_ln:substation_dist + Land_usage_cat:slope +
    Land_usage_cat:substation_dist + Land_usage_cat:nsrdb3_ghi +
    Land_usage_cat:landval_ln + aspect_cardinal:nsrdb3_ghi,
  data = Matched_df,
  family = binomial(link = "logit")
)
summary(t2.1)
pR2(t2.1)
write_rds(t2.1, "~/Wageningen/University/Thesis/Data/1. Modified Data/2. Models/GLmulti Matched_df tests/t2.1")

t3.1 <- glm(
  formula = Solar_binary ~ 1 + Land_usage_cat + aspect_cardinal + slope +
    substation_dist + nsrdb3_ghi + landval_ln + aspect_cardinal:Land_usage_cat +
    substation_dist:slope + nsrdb3_ghi:slope + nsrdb3_ghi:substation_dist +
    landval_ln:slope + landval_ln:substation_dist + Land_usage_cat:slope +
    Land_usage_cat:substation_dist + Land_usage_cat:nsrdb3_ghi +
    Land_usage_cat:landval_ln + aspect_cardinal:slope + aspect_cardinal:nsrdb3_ghi,
  data = Matched_df,
  family = binomial(link = "logit")
)
summary(t3.1)
pR2(t3.1)
write_rds(t3.1, "~/Wageningen/University/Thesis/Data/1. Modified Data/2. Models/GLmulti Matched_df tests/t3.1")


t4.1 <- glm(
  formula = Solar_binary ~ 1 + Land_usage_cat + aspect_cardinal + slope +
    substation_dist + nsrdb3_ghi + landval_ln + aspect_cardinal:Land_usage_cat +
    nsrdb3_ghi:slope + nsrdb3_ghi:substation_dist + landval_ln:slope +
    landval_ln:substation_dist + Land_usage_cat:slope + Land_usage_cat:substation_dist +
    Land_usage_cat:nsrdb3_ghi + Land_usage_cat:landval_ln + aspect_cardinal:nsrdb3_ghi,
  data = Matched_df,
  family = binomial(link = "logit")
)
summary(t4.1)
pR2(t4.1)
write_rds(t4.1, "~/Wageningen/University/Thesis/Data/1. Modified Data/2. Models/GLmulti Matched_df tests/t4.1")


t5.1 <- glm(
  formula = Solar_binary ~ 1 + Land_usage_cat + aspect_cardinal + slope +
    substation_dist + nsrdb3_ghi + landval_ln + aspect_cardinal:Land_usage_cat +
    substation_dist:slope + nsrdb3_ghi:slope + nsrdb3_ghi:substation_dist +
    landval_ln:slope + landval_ln:substation_dist + landval_ln:nsrdb3_ghi +
    Land_usage_cat:slope + Land_usage_cat:substation_dist + Land_usage_cat:nsrdb3_ghi +
    Land_usage_cat:landval_ln + aspect_cardinal:slope + aspect_cardinal:nsrdb3_ghi,
  data = Matched_df,
  family = binomial(link = "logit")
)
summary(t5.1)
pR2(t5.1)
write_rds(t5.1, "~/Wageningen/University/Thesis/Data/1. Modified Data/2. Models/GLmulti Matched_df tests/t5.1")
remove(t5.1)
gc()


t6.1 <- glm(
  formula = Solar_binary ~ 1 + Land_usage_cat + aspect_cardinal + slope +
    substation_dist + nsrdb3_ghi + landval_ln + aspect_cardinal:Land_usage_cat +
    substation_dist:slope + nsrdb3_ghi:slope + landval_ln:slope +
    landval_ln:substation_dist + landval_ln:nsrdb3_ghi + Land_usage_cat:slope +
    Land_usage_cat:substation_dist + Land_usage_cat:nsrdb3_ghi +
    Land_usage_cat:landval_ln + aspect_cardinal:slope + aspect_cardinal:nsrdb3_ghi,
  data = Matched_df,
  family = binomial(link = "logit")
)
summary(t6.1)
pR2(t6.1)
write_rds(t6.1, "~/Wageningen/University/Thesis/Data/1. Modified Data/2. Models/GLmulti Matched_df tests/t6.1")
remove(t6.1)
gc()


t7.1 <- glm(
  formula = Solar_binary ~ 1 + Land_usage_cat + aspect_cardinal + slope +
    substation_dist + nsrdb3_ghi + landval_ln + aspect_cardinal:Land_usage_cat +
    substation_dist:slope + nsrdb3_ghi:slope + nsrdb3_ghi:substation_dist +
    landval_ln:slope + landval_ln:substation_dist + Land_usage_cat:slope +
    Land_usage_cat:substation_dist + Land_usage_cat:nsrdb3_ghi +
    Land_usage_cat:landval_ln + aspect_cardinal:substation_dist +
    aspect_cardinal:nsrdb3_ghi,
  data = Matched_df,
  family = binomial(link = "logit")
)
summary(t7.1)
pR2(t7.1)
write_rds(t7.1, "~/Wageningen/University/Thesis/Data/1. Modified Data/2. Models/GLmulti Matched_df tests/t7.1")
remove(t7.1)
gc()

t8.1 <- glm(
  formula = Solar_binary ~ 1 + Land_usage_cat + aspect_cardinal + slope +
    substation_dist + nsrdb3_ghi + landval_ln + aspect_cardinal:Land_usage_cat +
    nsrdb3_ghi:slope + landval_ln:substation_dist + Land_usage_cat:slope +
    Land_usage_cat:substation_dist + Land_usage_cat:nsrdb3_ghi +
    Land_usage_cat:landval_ln + aspect_cardinal:slope + aspect_cardinal:landval_ln,
  data = Matched_df,
  family = binomial(link = "logit")
)
summary(t8.1)
pR2(t8.1)
write_rds(t8.1, "~/Wageningen/University/Thesis/Data/1. Modified Data/2. Models/GLmulti Matched_df tests/t8.1")
remove(t8.1)
gc()


t9.1 <- glm(
  formula = Solar_binary ~ 1 + Land_usage_cat + slope + substation_dist +
    nsrdb3_ghi + landval_ln + nsrdb3_ghi:slope + nsrdb3_ghi:substation_dist +
    landval_ln:slope + landval_ln:nsrdb3_ghi + Land_usage_cat:slope +
    Land_usage_cat:substation_dist + Land_usage_cat:nsrdb3_ghi +
    Land_usage_cat:landval_ln + aspect_cardinal:nsrdb3_ghi +
    aspect_cardinal:landval_ln,
  data = Matched_df,
  family = binomial(link = "logit")
)
summary(t9.1)
pR2(t9.1)
write_rds(t9.1, "~/Wageningen/University/Thesis/Data/1. Modified Data/2. Models/GLmulti Matched_df tests/t9.1")
remove(t9.1)
gc()


t10.1 <- glm(
  formula = Solar_binary ~ 1 + Land_usage_cat + slope + substation_dist +
    nsrdb3_ghi + landval_ln + nsrdb3_ghi:slope + nsrdb3_ghi:substation_dist +
    landval_ln:slope + Land_usage_cat:slope + Land_usage_cat:substation_dist +
    Land_usage_cat:nsrdb3_ghi + Land_usage_cat:landval_ln + aspect_cardinal:slope +
    aspect_cardinal:nsrdb3_ghi,
  data = Matched_df,
  family = binomial(link = "logit")
)
summary(t10.1)
pR2(t10.1)
write_rds(t10.1, "~/Wageningen/University/Thesis/Data/1. Modified Data/2. Models/GLmulti Matched_df tests/t10.1")
remove(t10.1)
gc()

#Regression based on what I think makes sense to interact
logit_reg_model_v3 <- glm(
  Solar_binary ~ 
    Land_usage_cat + aspect_cardinal + slope + 
    HVline_dist + substation_dist + nsrdb3_ghi + 
    landval_ln + aspect_cardinal:slope + Land_usage_cat:slope + 
    nsrdb3_ghi:slope + nsrdb3_ghi:aspect_cardinal,
  data = Matched_df,
  family = binomial(link = "logit")
)
summary(logit_reg_model_v3)
pR2(logit_reg_model_v3)
write_rds(logit_reg_model_v3, "~/Wageningen/University/Thesis/Data/1. Modified Data/2. Models/logit_reg_model_v3.rds")
