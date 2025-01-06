library(tidyverse)
library(tidycensus)
library(terra)
library(tigris)
library(sf)
library(ggplot2)

master.raster <- read_rds("~/Wageningen/University/Thesis/Data/1. Modified Data/master_raster_df_v2.rds")
names(master.raster)
unique(master.raster$State)

#ADDING STATE VALUES TO ALL CELLS
states_vect <- vect("~/Wageningen/University/Thesis/Data/1. Modified Data/states_vect/states_vect.shp")
states_vect

landval <- rast("~/Wageningen/University/Thesis/Data/1. Modified Data/landval.tiff")
states_rast <- rasterize(states_vect, landval, field = "STUSPS")

states_rast
plot(states_rast)
names(states_vect)
unique(states_vect$NAME)

states_df <- as.data.frame(states_rast, cell = T)
head(states_df)
head(master.raster)

master.raster <- master.raster %>%
  left_join(states_df, by = "cell")

names(master.raster)

master.raster <- master.raster %>%
  select(-State) %>%
  rename("State" = "STUSPS")

names(master.raster)
unique(master.raster$State)

remove(states_df, states_rast, states_vect)

write_rds(master.raster, "~/Wageningen/University/Thesis/Data/1. Modified Data/master_raster_df_v2.rds")

#--------------------------------------------------------------

#ADDING LAND USAGE DATA TO ALL CELLS

land_usage <- rast("~/Wageningen/University/Thesis/Data/USDA NASS Cropland Data/2023 National CDL 30m/2023_30m_cdls.tif")
plot(land_usage)
land_usage
unique(land_usage$Class_Names)

land_usage_re <- resample(land_usage, landval, method = "near")
land_usage_re
plot(land_usage_re)
land_usage_re_df <- as.data.frame(land_usage_re, cells = T)
unique(land_usage_re_df$Class_Names)

land_usage_re_df <- land_usage_re_df %>%
  filter(Class_Names != "Background")

unique(land_usage_re_df)

#categorizing land uses
cat_key <- read.csv("~/Wageningen/University/Thesis/Data/1. Modified Data/Land Use Category Key.csv", header = F)

cat_key <- cat_key %>%
  rename("Class_Names" = "V1")

land_usage_re_df$Class_Names <- str_trim(land_usage_re_df$Class_Names, side = "both")

land_usage_re_df <- land_usage_re_df %>%
  left_join(cat_key, by = "Class_Names")

land_usage_re_df %>%
  filter(is.na(V2))

land_usage_re_df <- land_usage_re_df %>%
  rename("Land_usage" = "Class_Names") %>%
  rename("Land_usage_cat" = "V2")

write_rds(land_usage_re_df, "~/Wageningen/University/Thesis/Data/1. Modified Data/CroplandCROS2023_resampled_df.rds")

master.raster <- master.raster %>%
  left_join(land_usage_re_df, by = "cell")

write_rds(master.raster, "~/Wageningen/University/Thesis/Data/1. Modified Data/master_raster_df_v2.rds")



