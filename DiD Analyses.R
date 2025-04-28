library(tidyverse)


###DiD Analyses

did_solar <- lm(
  `Solar_Percentage` ~ group + shock + treated_post,
  data = panel_data_V2
)

summary(did_solar)


did_solarpv <- lm(
  `Solar Photovoltaic` ~ group + shock + treated_post,
  data = panel_data_V2
)

summary(did_solarpv)


did_solar_YCOM <- lm(
  Solar_Percentage ~ group + shock + treated_post + YCOM_weighted_avg,
  data = panel_data_V2
)

summary(did_solar_YCOM)


did_solarpv_YCOM <- lm(
  `Solar Photovoltaic` ~ group + shock + treated_post + YCOM_weighted_avg,
  data = panel_data_V2
)

summary(did_solarpv_YCOM)


#Using cell level panel data
cell_year_panel <- readRDS("~/Wageningen/University/Thesis/Data/1. Modified Data/3. Panel Data/cell_year_panel.rds")

did_solar_cell <- lm(
  `Solar Photovoltaic` ~ group + shock + treated_post,
  data = cell_year_panel,
  weights = weight
)

summary(did_solar_cell)




#Simple DiD using a weighted percentage of capacity that comes from utility scale solar PV at group level. Data created in "2-14-25 Comparing group trends"
did_group_solarpercent <- lm(
  solar_share ~ group + shock + treated_post,
  data = group_year_panel
)

summary(did_group_solarpercent)

#Same as above, with the weighted avg of dem_lean as control
did_group_solarpercent_v1.1 <- lm(
  solar_share ~ group + shock + treated_post + dem_lean_weighted_avg,
  data = group_year_panel
)

summary(did_group_solarpercent_v1.1)

#lagged shock
did_group_solarpercent_lagged <- lm(
  solar_share ~ group + shock_lag + treated_post_lag,
  data = group_year_panel
)

summary(did_group_solarpercent_lagged)

#lagged shock with dem_lean
did_group_solarpercent_lagged_v1.1 <- lm(
  solar_share ~ group + shock_lag + treated_post_lag + dem_lean_weighted_avg,
  data = group_year_panel
)

summary(did_group_solarpercent_lagged_v1.1)

#lagged shock w. dem_lean and YCOM
did_group_solarpercent_lagged_v1.2 <- lm(
  solar_share ~ group + shock_lag + treated_post_lag + dem_lean_weighted_avg + YCOM_weighted_avg,
  data = group_year_panel
)

summary(did_group_solarpercent_lagged_v1.2)


##Strata Level Mixed Models
#Installing and loading Panel Generalized Linear Models package
install.packages("pglm")
library(pglm)

#panel logit model of solar_binary *Doesn't work*
#RE_logit_weighted <- pglm(
#  solar_binary ~ group + shock_lag + treated_post_lag,
#  data = strata_group_year_panel,
#  family = binomial("logit"),
#  model = "random",
#  effect = "individual",
#  index = c("strata", "group", "year"))




##MaineCEMv5 Panel Data DiD with lagged shock
did_group_solarpercent_laggedv2 <- lm(
  solar_share ~ group + shock_lag + treated_post_lag,
  data = group_year_panel_v2
)

summary(did_groupv2_solarpercent_lagged)

#lagged shock with dem_lean
did_group_solarpercent_lagged_v2.1 <- lm(
  solar_share ~ group + shock_lag + treated_post_lag + dem_lean_weighted_avg,
  data = group_year_panel_v2
)

summary(did_group_solarpercent_lagged_v2.1)

#lagged shock w. dem_lean and YCOM
did_group_solarpercent_lagged_v2.2 <- lm(
  solar_share ~ group + shock_lag + treated_post_lag + dem_lean_weighted_avg + YCOM_weighted_avg,
  data = group_year_panel_v2
)

summary(did_group_solarpercent_lagged_v2.2)


##MaineCEMv6 Data

#MaineCEMv6 Panel Data DiD with lagged shock
did_group_solarpercent <- lm(
  solar_share ~ group + shock + treated_post,
  data = group_year_panel_v3
)

summary(did_group_solarpercent)


#MaineCEMv6 Panel Data DiD with lagged shock
did_group_solarpercent_laggedv3 <- lm(
  solar_share ~ group + shock_lag + treated_post_lag,
  data = group_year_panel_v3
)

summary(did_group_solarpercent_laggedv3)

#lagged shock with dem_lean
did_group_solarpercent_lagged_v3.1 <- lm(
  solar_share ~ group + shock_lag + treated_post_lag + dem_lean_weighted_avg,
  data = group_year_panel_v3
)

summary(did_group_solarpercent_lagged_v3.1)

#lagged shock w. dem_lean and YCOM
did_group_solarpercent_lagged_v3.2 <- lm(
  solar_share ~ group + shock_lag + treated_post_lag + dem_lean_weighted_avg + YCOM_weighted_avg,
  data = group_year_panel_v3
)

summary(did_group_solarpercent_lagged_v3.2)


#lagged shock w. dem_lean, YCOM, and LCOE_diff
did_group_solarpercent_lagged_v3.3 <- lm(
  solar_share ~ group + shock_lag + treated_post_lag + dem_lean_weighted_avg + YCOM_weighted_avg + LCOE_difference_weighted,
  data = group_year_panel_v3
)

summary(did_group_solarpercent_lagged_v3.3)

#lagged shock w. dem_lean, YCOM, and Solar_LCOE
did_group_solarpercent_lagged_v3.4 <- lm(
  solar_share ~ group + shock_lag + treated_post_lag + dem_lean_weighted_avg + YCOM_weighted_avg + Solar_LCOE_weighted,
  data = group_year_panel_v3
)

summary(did_group_solarpercent_lagged_v3.4)

#creating a truncated data set
group_year_panel_v3_trun <- group_year_panel_v3 %>%
  filter(year > 2016)

ggplot(group_year_panel_v3_trun, aes(x = year, y = solar_share, color = factor(group))) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Solar Share Over Time by Group",
    x = "Year",
    y = "Solar Share",
    color = "Group"
  ) +
  theme_minimal()

# truncated data lagged shock w. dem_lean, YCOM, and LCOE_diff
did_group_solarpercent_lagged_v4.3 <- lm(
  solar_share ~ group + shock_lag + treated_post_lag,
  data = group_year_panel_v3_trun
)

summary(did_group_solarpercent_lagged_v4.3)


##cell-year regressions
#simple w. lagged
DiD_cell_year <- lm(
  Solar_Photovoltaic ~ group + shock_lag + treated_post_lag,
  data = cell_year_panel_v3,
  weights = weight
)

summary(DiD_cell_year)

#lagged w. covariates
DiD_cell_year_v2 <- lm(
  Solar_Photovoltaic ~ group + shock_lag + treated_post_lag + dem_lean + YCOM_value + LCOE_difference,
  data = cell_year_panel_v3,
  weights = weight
)

summary(DiD_cell_year_v2)




















#simple two way fixed effect with fixest package
library(fixest)
DiD_cell_year_FE <- feols(
  Solar_Photovoltaic ~ group + shock_lag + treated_post_lag | cell + year, 
  data = cell_year_panel_v3, 
  weights = ~ weight
)

summary(DiD_cell_year_FE)


#TWFE with covariates
DiD_cell_year_FE_v2 <- feols(
  Solar_Photovoltaic ~ group + shock_lag + treated_post_lag + dem_lean + YCOM_value + LCOE_difference | cell + year, 
  data = cell_year_panel_v3, 
  weights = ~ weight
)

summary(DiD_cell_year_FE_v2)




