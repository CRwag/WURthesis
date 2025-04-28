library(tidyverse)
library(ggplot2)

### Creating cell_year_panel_v4 (cell-year panel dataset using MaineCEMv7.3) 4/15/25

#read in CEM object
MaineCEMv7.3 <- readRDS("~/Wageningen/University/Thesis/Data/1. Modified Data/2. Models/MaineCEM/MaineCEMv7_3.rds")

# Read in cell_year_capacity
cell_year_capacity <- readRDS("~/Wageningen/University/Thesis/Data/1. Modified Data/cell_year_capacity.rds")

#pull match values into a df
MaineCEMv7.3_df <- data.frame(
  strata = MaineCEMv7.3$strata,
  group  = MaineCEMv7.3$groups,
  matched = MaineCEMv7.3$matched,
  weight = MaineCEMv7.3$w
)

#joining with master_raster_v2_reduced_filtered_v3
master_raster_V2_reduced_filtered_v3 <- bind_cols(master_raster_V2_reduced_filtered_v3, MaineCEMv7.3_df)
head(master_raster_V2_reduced_filtered_v3)

#removing unmatched cells
master_raster_V2_reduced_filtered_v3 <- master_raster_V2_reduced_filtered_v3 %>%
  filter(matched == TRUE)

#creating cell_year_panelv3
cell_year_panel_v4 <- expand_grid(master_raster_V2_reduced_filtered_v3, year = 2010:2023)

# Read in cell_year_capacity
cell_year_capacity <- readRDS("~/Wageningen/University/Thesis/Data/1. Modified Data/cell_year_capacity.rds")

head(cell_year_capacity)

# Pull `Solar Photovoltaic` in from cell_year_capacity
cell_year_panel_v4 <- cell_year_panel_v4 %>%
  left_join(
    cell_year_capacity %>% select(x, y, year, `Solar Photovoltaic`),
    by = c("x", "y", "year")
  ) %>%
  mutate(`Solar Photovoltaic` = replace_na(`Solar Photovoltaic`, 0))

# Creating weighted Solar PV capacity
cell_year_panel_v4 <- cell_year_panel_v4 %>%
  mutate(weighted_solar = `Solar Photovoltaic` * weight)

## Adding LCOE variable--------------
#assign regions by state using key
state_region <- read_excel("~/Wageningen/University/Thesis/Data/1. Modified Data/Solar_Wind_LCOE_estimates.xlsx", sheet = "State-Region")

cell_year_panel_v4 <- cell_year_panel_v4 %>%
  left_join(state_region, by = "State")

# Pulling in Onshore-Wind and Utility Scale Solar LCOE estimates
Solar_Wind_LCOE_estimates <- read_excel("~/Wageningen/University/Thesis/Data/1. Modified Data/Solar_Wind_LCOE_estimates.xlsx", sheet = "Import")

# Joining to cell_year_panel_v4
cell_year_panel_v4 <- cell_year_panel_v4 %>%
  left_join(Solar_Wind_LCOE_estimates, by = c("year" = "Year", "Region"))

# Creating LCOE difference variables
cell_year_panel_v4 <- cell_year_panel_v4 %>%
  mutate(
    LCOE_difference = Solar_LCOE - Wind_LCOE,
    LCOE_percent_diff = ((Solar_LCOE - Wind_LCOE) / Wind_LCOE) * 100
  )

## Adding dem_lean -----

#reading in dem_leans data [created in 2-12-25 Creating dem_leans_complete]
dem_leans_complete <- readRDS("~/Wageningen/University/Thesis/Data/1. Modified Data/dem_leans_complete.rds")

#joining
cell_year_panel_v4 <- cell_year_panel_v4 %>%
  left_join(dem_leans_complete, by = c("State" = "state_po", "year" = "year"))

## Adding YCOM values------

#Read in the YCOM data
YCOM <- read_excel("~/Wageningen/University/Thesis/Data/Yale Climate Opinions/YCOM7_publicdata.xlsx", 
                   sheet = "YCOM_2010_2023")

# filter for our variable (fundrenewables)
YCOM <- YCOM %>%
  filter(varname == "fundrenewables", geotype == "state")

# Convert to long format
YCOM_long <- YCOM %>%
  pivot_longer(
    cols = starts_with("x"), 
    names_to = "year", 
    names_prefix = "x",
    values_to = "YCOM_value"
  ) %>%
  mutate(year = as.integer(year))

# Read in statenames key
statenames <- read_excel("~/Wageningen/University/Thesis/Data/50 states and abbreviations.xlsx")

#Join statenames
YCOM_long <- YCOM_long %>%
  left_join(statenames, by = c("geoname" = "State")) %>%
  select(year, YCOM_value, Abbreviation) %>%
  rename(State = Abbreviation)

# Join YCOM to cell_year_panel_v4
cell_year_panel_v4 <- cell_year_panel_v4 %>%
  left_join(YCOM_long, by = c("year", "State"))

#renaming full state name column
cell_year_panel_v4 <- cell_year_panel_v4 %>%
  rename(State_full = state)

write_rds(cell_year_panel_v4, "~/Wageningen/University/Thesis/Data/1. Modified Data/3. Panel Data/cell_year_panel_v4.rds")

