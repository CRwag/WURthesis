library(tidyverse)
library(tidycensus)
library(terra)
library(tigris)
library(sf)
library(ggplot2)
library(RANN)
library(FNN)


#PULLING IN LANDVAL RASTER FOR BASELINE RASTER DIMENSIONS
landval <- rast("~/Wageningen/University/Thesis/Data/1. Modified Data/landval.tiff")
landval

landval_pts <- as.points(landval)
head(landval_pts)


#PULLING IN AND FILTERING SUBSTATION DATA
substations <- read.csv("~/Wageningen/University/Thesis/Data/US Substations 2020/Substations.csv")
names(substations)

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


#CSV to vector
substations_vect <- vect(substations, geom = c("X", "Y"), crs = "EPSG:5070")


#RANN ATTEMPT
#Pulling coordinates
landval_coords <- geom(landval_pts)[, c("x", "y")]
substations_coords <- geom(substations_vect)[, c("x", "y")]
write_rds(landval_coords, "~/Wageningen/University/Thesis/Data/1. Modified Data/raster_centroid_coords.rds")

#removing all unneeded elements to save on RAM
remove(landval)
remove(landval_pts)
remove(substations)
remove(substations_vect)
gc()

#test
landval_coords_subset <- rast_centers[1:100, ]
subs_subs <- substations_coords[1:10, ]
nn <- nn2(data = substations_coords, query = landval_coords_subset, k = 1)


landval_coords_vect <- vect()

landval_pts <- vect(landval_coords_subset, type = "points", crs = "EPSG:5070")
substation_pts <- vect(substation_coords, type = "points", crs = "EPSG:5070")

plot(landval_pts)
HVlines
