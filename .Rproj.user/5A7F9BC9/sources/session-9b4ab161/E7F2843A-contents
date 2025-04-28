library(terra)
library(sf)
library(tidyverse)
library(readxl)



###Building LCOE data for Utility Scale Solar and Onshore Wind - From HIFLD

#pulling in HIFLD data
regions <- vect("~/Wageningen/University/Thesis/Data/Infrastructure/HIFLD Control Areas/Control__Areas.geojson")
regions
plot(regions)

#Pulling in landval raster
landval <- rast("~/Wageningen/University/Thesis/Data/Land Value Data Nolte 2020/places_fmv_pnas_dryad/1 estimates/places_fmv_all.tif")
landval
plot(landval)

#reprojecting regions
regions <- project(regions, crs(landval))
regions
plot(regions)

plot(landval)
plot(regions, add = T)

#rasterizing regions
regions_rast <- rasterize(regions, landval, field = "NAME")

#pulling in reclassification table I made to reclassify to Berkeley regions
Control_Areas <- read_csv("~/Wageningen/University/Thesis/Data/Infrastructure/HIFLD Control Areas/Control__Areas.csv")

#cleaning
Control_Areas <- Control_Areas %>%
  select(-State, -...4, -...5, -`Region Options`)

Control_Areas <- Control_Areas %>%
  rename("NAME" = `Control Area Name`)

#turning into data frame
regions_df <- as.data.frame(regions_rast, xy = T)

#creating Berkeley Lab Region column
regions_df <- regions_df %>%
  left_join(Control_Areas, by = "NAME")

#Turning back into spatrast to check map
regions_vect <- vect(regions_df, geom = c("x", "y"), crs = crs(landval))
regions_rast_new <- rasterize(regions_vect, landval, field = "Berkeley Lab Region")
regions_rast_new

write_rds(regions_df, "~/Wageningen/University/Thesis/Data/1. Modified Data/regions_df.rds")

#joining to cell_year_panel_v3
cell_year_panel_v3 <- cell_year_panel_v3 %>%
  left_join(regions_df %>% 
              select(x, y, `Berkeley Lab Region`), by = c("x", "y"))

#looking at NA region observations
na_by_state <- cell_year_panel_v3 %>%
  group_by(State) %>%
  summarise(n_missing = sum(is.na(`Berkeley Lab Region`)), .groups = "drop") %>%
  arrange(desc(n_missing))

na_by_state

#Turning region in a factor
cell_year_panel_v3 <- cell_year_panel_v3 %>%
  mutate(`Berkeley Lab Region` = factor(`Berkeley Lab Region`))

###I have no idea why this is messed up, I am just going to assign region by state, because the estimates will be reasonable enough that way
#pulling in state-region key
state_region <- read_excel("~/Wageningen/University/Thesis/Data/1. Modified Data/Solar_Wind_LCOE_estimates.xlsx", sheet = "State-Region")

#Joining regions to cell_year_panel_v3
cell_year_panel_v3 <- cell_year_panel_v3 %>%
  left_join(state_region, by = "State")

#checking for NAs: 0 NA
cell_year_panel_v3 %>%
  filter(is.na(Region)) %>%
  nrow()

#Pulling in Onshore-Wind and Utility Scale Solar LCOE estimates
Solar_Wind_LCOE_estimates <- read_excel("~/Wageningen/University/Thesis/Data/1. Modified Data/Solar_Wind_LCOE_estimates.xlsx", sheet = "Import")

#Joining to cell_year_panel_v3
cell_year_panel_v3 <- cell_year_panel_v3 %>%
  left_join(Solar_Wind_LCOE_estimates, by = c("year" = "Year", "Region"))

#Creating difference in cost variables
cell_year_panel_v3 <- cell_year_panel_v3 %>%
  mutate(
    LCOE_difference = Solar_LCOE - Wind_LCOE,
    LCOE_percent_diff = ((Solar_LCOE - Wind_LCOE) / Wind_LCOE) * 100
  )

#adding to group_year_panel_v3
LCOE_agg <- cell_year_panel_v3 %>%
  group_by(group, year) %>%
  summarise(
    LCOE_difference_weighted = sum(LCOE_difference * weight) / sum(weight),
    LCOE_percent_diff_weighted = sum(LCOE_percent_diff * weight) / sum(weight),
    .groups = "drop"
  )

LCOE_agg <- LCOE_agg %>%
  mutate(group = as.numeric(as.character(group)))

group_year_panel_v3 <- group_year_panel_v3 %>%
  left_join(LCOE_agg, by = c("group", "year"))

write_rds(cell_year_panel_v3, "~/Wageningen/University/Thesis/Data/1. Modified Data/3. Panel Data/cell_year_panel_v3.rds")


#adding solar_LCOE to group

cell_year_panel_v3 <- cell_year_panel_v3 %>%
  mutate(group = as.numeric(as.character(group)))

solar_lcoe_agg <- cell_year_panel_v3 %>%
  group_by(group, year) %>%
  summarise(
    Solar_LCOE_weighted = sum(Solar_LCOE * weight) / sum(weight),
    .groups = "drop"
  )

group_year_panel_v3 <- group_year_panel_v3 %>%
  left_join(solar_lcoe_agg, by = c("group", "year"))

write_rds(group_year_panel_v3, "~/Wageningen/University/Thesis/Data/1. Modified Data/3. Panel Data/group_year_panel_v3.rds")

