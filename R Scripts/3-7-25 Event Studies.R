###Event study prep (method described by Callaway and Sant'Anna 2021. Callaway and Sant'Anna are also the creators of the did package)
#install.packages("did")
library(did)
library(tidyverse)

#creating variable that describes when the treatment takes place (for "gname =" argument below)
cell_year_panel_v3 <- cell_year_panel_v3 %>%
  mutate(first.treat = if_else(group == 1, 2019, 0))

#removing superfluous columns
cell_year_panel_v3 <- cell_year_panel_v3 %>%
  select(cell, State, weight, year, `Solar Photovoltaic`, dem_lean, YCOM_value, first.treat, Solar_LCOE, LCOE_difference)

#Initial group-time ATT: 
#No covariates
#Standard errors clustered only at the individual level
#Varying  base period
event_study_v1 <- att_gt(
  yname = "Solar Photovoltaic",
  tname = "year",
  idname = "cell",
  gname = "first.treat",
  data = cell_year_panel_v3,
  weightsname = "weight",
  print_details = T
)

summary(event_study_v1)
write_rds(event_study_v1, "~/Wageningen/University/Thesis/Data/1. Modified Data/2. Models/Event Studies/event_study_v1.rds")


#group-time ATT plot
ggdid(event_study_v1)

#Aggregated group-time ATT (should be the same because there is only one treatment group)
esv1_agg <- aggte(event_study_v1, type = "dynamic")

#aggregated ATT plot (again, should be the same, x axis should be +/- 0 where 0 is the shock period)
ggdid(esv1_agg)


#Second group-time ATT
#No Covariates
#Standard errors clustered at the individual AND group level
#Varying  base period
event_study_v2 <- att_gt(
  yname = "Solar Photovoltaic",
  tname = "year",
  idname = "cell",
  gname = "first.treat",
  data = cell_year_panel_v3,
  weightsname = "weight",
  clustervars = c("cell", "first.treat"),
  print_details = T
)

summary(event_study_v2)

#group-time ATT plot
ggdid(event_study_v2)

#Clustering on the individual and group level didn't produce std. error vals, maybe because there are only two groups?


#Third group-time ATT
#No Covariates
#Standard errors clustered at the individual and State level
#Varying  base period
event_study_v3 <- att_gt(
  yname = "Solar Photovoltaic",
  tname = "year",
  idname = "cell",
  gname = "first.treat",
  data = cell_year_panel_v3,
  weightsname = "weight",
  clustervars = c("cell", "State"),
  print_details = T
)

summary(event_study_v3)
write_rds(event_study_v3, "~/Wageningen/University/Thesis/Data/1. Modified Data/2. Models/Event Studies/event_study_v3.rds")


#group-time ATT plot
ggdid(event_study_v3)


#Fourth group-time ATT
#Covariates: dem_lean, YCOM_value, LCOE_difference
#Standard errors clustered at the individual and State level
#Varying  base period
event_study_v4 <- att_gt(
  yname = "Solar Photovoltaic",
  tname = "year",
  idname = "cell",
  gname = "first.treat",
  data = cell_year_panel_v3,
  weightsname = "weight",
  clustervars = c("cell", "State"),
  xformla = ~ dem_lean + YCOM_value + LCOE_difference,
  print_details = T
)

summary(event_study_v4)

#group-time ATT plot
ggdid(event_study_v4)
#did not work, provided all NAs for estimations


#Fifth group-time ATT - normal regression method
#Covariates: dem_lean, YCOM_value, LCOE_difference
#Standard errors clustered at the individual and State level
#Varying  base period
event_study_v5 <- att_gt(
    yname = "Solar Photovoltaic",
    tname = "year",
    idname = "cell",
    gname = "first.treat",
    data = cell_year_panel_v3,
    weightsname = "weight",
    clustervars = c("cell", "State"),
    xformla = ~ dem_lean + YCOM_value + LCOE_difference,
    est_method = "reg",
    print_details = T
)

summary(event_study_v5)
ggdid(event_study_v5)
write_rds(event_study_v5, "~/Wageningen/University/Thesis/Data/1. Modified Data/2. Models/Event Studies/event_study_v5.rds")


#Sixth group-time ATT
#No Covariates
#Standard errors clustered at the individual and State level
#Universal base period
event_study_v6 <- att_gt(
  yname = "Solar_Photovoltaic",
  tname = "year",
  idname = "cell",
  gname = "first.treat",
  data = cell_year_panel_v3,
  weightsname = "weight",
  clustervars = c("cell", "State"),
  base_period = "universal",
  print_details = T
)

summary(event_study_v6)
write_rds(event_study_v6, "~/Wageningen/University/Thesis/Data/1. Modified Data/2. Models/Event Studies/event_study_v6.rds")


#group-time ATT plot
ggdid(event_study_v6) + geom_vline(xintercept = 9, linetype = "dashed", color = "black")





##Event study using fixest package
library(fixest)

#Creating event-time indicator
cell_year_panel_v3 <- cell_year_panel_v3 %>%
  mutate(event_time = if_else(first.treat > 0, year - first.treat, NA_real_))
str(cell_year_panel_v3)

#TWFE event study with no covariates
twfe_event_study <- feols(
  Solar_Photovoltaic ~ i(year, group, ref = 2019) | cell + year,
  data = cell_year_panel_v3,
  weights = ~ weight
)
summary(twfe_event_study)
coefplot(twfe_event_study)
#this gave me the coefficients for the wrong group

#swapping around the levels of group 
cell_year_panel_v3 <- cell_year_panel_v3 %>%
   mutate(
     group_factor = factor(group, levels = c(1, 0))
     )

#Second TWFE
twfe_event_study2 <- feols(
  Solar_Photovoltaic ~ i(year, group_factor, ref = 2019) | cell + year,
  data = cell_year_panel_v3,
  weights = ~ weight
)
summary(twfe_event_study2)
etable(twfe_event_study2)
coefplot(twfe_event_study2)

#Third TWFE - clustering SE on cell and State
twfe_event_study3 <- feols(
  Solar_Photovoltaic ~ i(year, group_factor, ref = 2019) | cell + year,
  data = cell_year_panel_v3,
  weights = ~ weight,
  vcov = ~ cell + State
)
summary(twfe_event_study3)
etable(twfe_event_study3)
coefplot(twfe_event_study3)

#fourth TWFE
#clustering on cell and state
#with covariates
twfe_event_study4 <- feols(
  Solar_Photovoltaic ~ i(year, group_factor, ref = 2019) + dem_lean + YCOM_value + LCOE_difference | cell + year,
  data = cell_year_panel_v3,
  weights = ~ weight,
  vcov = ~ cell + State
)

summary(twfe_event_study4)
etable(twfe_event_study4)
coefplot(
  twfe_event_study4, 
  drop = c("dem_lean", "YCOM_value", "LCOE_difference")
)

write_rds(twfe_event_study, "~/Wageningen/University/Thesis/Data/1. Modified Data/2. Models/Event Studies/twfe_event_study.rds")
write_rds(twfe_event_study2, "~/Wageningen/University/Thesis/Data/1. Modified Data/2. Models/Event Studies/twfe_event_study2.rds")
write_rds(twfe_event_study3, "~/Wageningen/University/Thesis/Data/1. Modified Data/2. Models/Event Studies/twfe_event_study3.rds")
write_rds(twfe_event_study4, "~/Wageningen/University/Thesis/Data/1. Modified Data/2. Models/Event Studies/twfe_event_study4.rds")

#5th TWFE 
#clustering SE on State ONLY
#No covariates
twfe_event_study5 <- feols(
  Solar_Photovoltaic ~ i(year, group_factor, ref = 2019) | cell + year,
  data = cell_year_panel_v3,
  weights = ~ weight,
  vcov = ~ State
)
summary(twfe_event_study5)
etable(twfe_event_study5)
coefplot(twfe_event_study5)

coefplot(twfe_event_study5,
         dict = c("year::2010:group_factor::1" = "2010",
                  "year::2011:group_factor::1" = "2011",
                  "year::2012:group_factor::1" = "2012",
                  "year::2013:group_factor::1" = "2013",
                  "year::2014:group_factor::1" = "2014",
                  "year::2015:group_factor::1" = "2015",
                  "year::2016:group_factor::1" = "2016",
                  "year::2017:group_factor::1" = "2017",
                  "year::2018:group_factor::1" = "2018",
                  "year::2020:group_factor::1" = "2020",
                  "year::2021:group_factor::1" = "2021",
                  "year::2022:group_factor::1" = "2022",
                  "year::2023:group_factor::1" = "2023"),
         ref = c("2019" = 10),
         ref.line = TRUE,
         ref.line.par = list(lty = "44", col = "black"),  # "44" for longer dashes
         xlab = "Year",
         main = "",
         group = NULL
)




#6th TWFE
#clustering on State ONLY
#with covariates
twfe_event_study6 <- feols(
  Solar_Photovoltaic ~ i(year, group_factor, ref = 2019) + dem_lean + YCOM_value + LCOE_difference | cell + year,
  data = cell_year_panel_v3,
  weights = ~ weight,
  vcov = ~ State
)

summary(twfe_event_study6)
etable(twfe_event_study6)
coefplot(
  twfe_event_study6, 
  drop = c("dem_lean", "YCOM_value", "LCOE_difference")
)

coefplot(twfe_event_study6,
         drop = c("dem_lean", "YCOM_value", "LCOE_difference"),
         dict = c("year::2010:group_factor::1" = "2010",
                  "year::2011:group_factor::1" = "2011",
                  "year::2012:group_factor::1" = "2012",
                  "year::2013:group_factor::1" = "2013",
                  "year::2014:group_factor::1" = "2014",
                  "year::2015:group_factor::1" = "2015",
                  "year::2016:group_factor::1" = "2016",
                  "year::2017:group_factor::1" = "2017",
                  "year::2018:group_factor::1" = "2018",
                  "year::2020:group_factor::1" = "2020",
                  "year::2021:group_factor::1" = "2021",
                  "year::2022:group_factor::1" = "2022",
                  "year::2023:group_factor::1" = "2023"),
         ref = c("2019" = 10),
         ref.line = TRUE,
         ref.line.par = list(lty = "44", col = "black"),  # "44" for longer dashes
         xlab = "Year",
         main = "",
         group = NULL
)

write_rds(twfe_event_study5, "~/Wageningen/University/Thesis/Data/1. Modified Data/2. Models/Event Studies/twfe_event_study5.rds")
write_rds(twfe_event_study6, "~/Wageningen/University/Thesis/Data/1. Modified Data/2. Models/Event Studies/twfe_event_study6.rds")
write_rds(cell_year_panel_v3, "~/Wageningen/University/Thesis/Data/1. Modified Data/3. Panel Data/cell_year_panel_v3.rds")



### TWFE with cell_year_panel_v4 ----

#creating group factor so the right group is taken as the treatment group
cell_year_panel_v4 <- cell_year_panel_v4 %>%
  mutate(
    group_factor = factor(group, levels = c(1, 0))
  )

#renaming dependent variable to reference easier
cell_year_panel_v4 <- cell_year_panel_v4 %>%
  rename(Solar_Photovoltaic = `Solar Photovoltaic`)

write_rds(cell_year_panel_v4, "~/Wageningen/University/Thesis/Data/1. Modified Data/3. Panel Data/cell_year_panel_v4.rds")


#7th TWFE 
#clustering SE on State ONLY
#No covariates
twfe_event_study7 <- feols(
  Solar_Photovoltaic ~ i(year, group_factor, ref = 2019) | cell + year,
  data = cell_year_panel_v4,
  weights = ~ weight,
  vcov = ~ State
)

summary(twfe_event_study7)
etable(twfe_event_study7)

coefplot(twfe_event_study7,
         dict = c("year::2010:group_factor::1" = "2010",
                  "year::2011:group_factor::1" = "2011",
                  "year::2012:group_factor::1" = "2012",
                  "year::2013:group_factor::1" = "2013",
                  "year::2014:group_factor::1" = "2014",
                  "year::2015:group_factor::1" = "2015",
                  "year::2016:group_factor::1" = "2016",
                  "year::2017:group_factor::1" = "2017",
                  "year::2018:group_factor::1" = "2018",
                  "year::2020:group_factor::1" = "2020",
                  "year::2021:group_factor::1" = "2021",
                  "year::2022:group_factor::1" = "2022",
                  "year::2023:group_factor::1" = "2023"),
         ref = c("2019" = 10),
         ref.line = TRUE,
         ref.line.par = list(lty = "44", col = "black"),
         xlab = "Year",
         ylab = "Policy Impact Est. Coefficient & 95% Conf. Int.",
         main = "",
         group = NULL
)




#8th TWFE
#clustering on State ONLY
#with covariates
twfe_event_study8 <- feols(
  Solar_Photovoltaic ~ i(year, group_factor, ref = 2019) + dem_lean + YCOM_value + LCOE_difference | cell + year,
  data = cell_year_panel_v4,
  weights = ~ weight,
  vcov = ~ State
)

summary(twfe_event_study8)
etable(twfe_event_study8)


coefplot(twfe_event_study8,
         drop = c("dem_lean", "YCOM_value", "LCOE_difference"),
         dict = c("year::2010:group_factor::1" = "2010",
                  "year::2011:group_factor::1" = "2011",
                  "year::2012:group_factor::1" = "2012",
                  "year::2013:group_factor::1" = "2013",
                  "year::2014:group_factor::1" = "2014",
                  "year::2015:group_factor::1" = "2015",
                  "year::2016:group_factor::1" = "2016",
                  "year::2017:group_factor::1" = "2017",
                  "year::2018:group_factor::1" = "2018",
                  "year::2020:group_factor::1" = "2020",
                  "year::2021:group_factor::1" = "2021",
                  "year::2022:group_factor::1" = "2022",
                  "year::2023:group_factor::1" = "2023"),
         ref = c("2019" = 10),
         ref.line = TRUE,
         ref.line.par = list(lty = "44", col = "black"),
         xlab = "Year",
         ylab = "Policy Impact Est. Coefficient & 95% Conf. Int.",
         main = "",
         group = NULL
)


