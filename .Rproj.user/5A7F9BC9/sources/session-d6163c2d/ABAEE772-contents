library(RANN)
library(sf)
library(terra)
library(tidyverse)



HVlines <- vect("~/Wageningen/University/Thesis/Data/1. Modified Data/HVpowerlines_clean/HVpowerlines_clean.shp")
landval <- rast("~/Wageningen/University/Thesis/Data/1. Modified Data/landval.tiff")

blank_raster <- rast(landval)

# Rasterize HVlines onto blank_raster
HVlines_raster <- rasterize(HVlines, blank_raster, touches = TRUE)
plot(HVlines_raster, col = "black")

plot(HVlines_raster, col = "black", xlim = c(1780435, 2000589), ylim = c(2155623, 2234074), main = "Zoomed in on LI")

HVlines_raster

#Pull coordinates of cells which contain HV powerlines
HVlines_df <- as.data.frame(HVlines_raster, xy=T)
HVlines_df <- HVlines_df %>%
  select(x,y)

HVlines_cell_coords <- as.matrix(HVlines_df)

#Coordinates of all cells in the cont. USA (created from landval)
centroid_coords <- read_rds("~/Wageningen/University/Thesis/Data/1. Modified Data/raster_centroid_coords.rds")

#Distance calculation from each cell
nearest_HVline_dist <- nn2(data = HVlines_cell_coords, query = centroid_coords, k = 1)

#Turning into data frame
centroid_coords_df <- as.data.frame(centroid_coords)
nearest_HVline_dist_df <- as.data.frame(nearest_HVline_dist)

nearest_HVline_dist_df <- nearest_HVline_dist_df %>%
  select(nn.dists)

nearest_HVline_dist_df <- bind_cols(centroid_coords_df, nearest_HVline_dist_df)

#plotting for reasonableness check
nearest_HVline_vect <- vect(nearest_HVline_dist_df, geom = c("x", "y"), crs = crs(landval))
nearest_HVline_raster <- rasterize(nearest_HVline_vect, landval, field = "nn.dists")
plot(nearest_HVline_raster)


#Adding to master_raster_df_v2

master.raster_df <- read_rds("~/Wageningen/University/Thesis/Data/1. Modified Data/master_raster_df_v2.rds")
names(master.raster_df)

master.raster_df %>%
  filter(!is.na(landval_ln)) %>%
  head()

master.raster_df <- master.raster_df %>%
  select(-nn.idx) %>%
  rename(substation_dist = nn.dists)

master.raster_df <- master.raster_df %>%
  left_join(nearest_HVline_dist_df, by = c("x", "y"))

master.raster_df <- master.raster_df %>%
  rename(HVline_dist = nn.dists)

write_rds(master.raster_df, "~/Wageningen/University/Thesis/Data/1. Modified Data/master_raster_df_v2.rds")
