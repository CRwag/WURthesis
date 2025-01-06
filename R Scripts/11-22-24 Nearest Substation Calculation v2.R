library(tidyverse)
library(tidycensus)
library(terra)
library(tigris)
library(sf)
library(ggplot2)
library(RANN)

#CLEANING AND VECTORIZIING SUBSTATIONS
substations <- read.csv("~/Wageningen/University/Thesis/Data/US Substations 2020/Substations.csv")

contiguous_states <- c(
  "AL", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "ID", "IL", 
  "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", 
  "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", 
  "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", 
  "WA", "WV", "WI", "WY", "DC"
)

substations <- substations %>%
  filter(TYPE == "SUBSTATION") %>%
  filter(STATUS == "IN SERVICE") %>%
  filter(COUNTRY == "USA") %>%
  filter(STATE %in% contiguous_states )


substations_vect <- vect(substations, geom = c("LONGITUDE", "LATITUDE"), crs = "EPSG:4326")
plot(substations_vect)

substations_vect <- project(substations_vect, "EPSG:5070")

substations_vect

writeVector(substations_vect, "~/Wageningen/University/Thesis/Data/1. Modified Data/substations_clean.shp")

#pulling out coordinates
substations_coords <- geom(substations_vect)[, c("x", "y")]

centroid_coords <- read_rds("~/Wageningen/University/Thesis/Data/1. Modified Data/raster_centroid_coords.rds")

#test
cent_test <- centroid_coords[1:100, ]
nntest <- nn2(data = substations_coords, query = cent_test, k = 1)
head(nntest)
substations_coords[43662, ]
print(cent_test)

#test 2
cent_test <- centroid_coords[1:50000, ]
system.time({
  nntest <- nn2(data = substations_coords, query = cent_test, k = 1)
})

remove(cent_test)
remove(nntest)

#full calculation
nearest_substation_dist <- nn2(data = substations_coords, query = centroid_coords, k = 1)


#CREATING RASTER AND PLOT
centroid_coords_df <- as.data.frame(centroid_coords)
nearest_substation_dist_df <- as.data.frame(nearest_substation_dist)

nearest_substation_dist_df <- bind_cols(centroid_coords_df, nearest_substation_dist_df)

landval <- rast("~/Wageningen/University/Thesis/Data/1. Modified Data/landval.tiff")
landval

write_rds(nearest_substation_dist_df, "~/Wageningen/University/Thesis/Data/1. Modified Data/nearest_substation_dist.rds")

#Creating vect and rast to plot for reasonableness check
nearest_substation_dist_vect <- vect(nearest_substation_dist_df, geom = c("x","y"), crs= "EPSG:5070")
nearest_substation_dist_vect
head(nearest_substation_dist_vect)

nearest_substation_dist_rast <- rasterize(nearest_substation_dist_vect, landval, field="nn.dists")
nearest_substation_dist_rast
plot(nearest_substation_dist_rast)

#ADDING TO master_raster_df_v2
master.raster_df <- read_rds("~/Wageningen/University/Thesis/Data/1. Modified Data/master_raster_df_v2.rds")

master.raster_df %>%
  filter(!is.na(landval_ln)) %>%
  head()

nearest_substation_dist <- read_rds("~/Wageningen/University/Thesis/Data/1. Modified Data/nearest_substation_dist.rds")

head(nearest_substation_dist)

master.raster_df <- master.raster_df %>%
  left_join(nearest_substation_dist, by = c("x","y"))

write_rds(master.raster_df, "~/Wageningen/University/Thesis/Data/1. Modified Data/master_raster_df_v2.rds")
