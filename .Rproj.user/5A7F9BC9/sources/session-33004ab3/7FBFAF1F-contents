library(tidyverse)
library(tidycensus)
library(terra)
library(tigris)
library(sf)
library(ggplot2)

#PULLING IN AND CHECKING AVG GHI DATA FROM 1998-2016 FOR THE USA
avg_GHI <- rast("~/Wageningen/University/Thesis/Data/NREL_NSRB_v3_GHI/nsrdbv3_ghi/Annual GHI/nsrdb3_ghi.tif")
plot(avg_GHI)
avg_GHI

#PULLING IN AND CHECKING CORE-BASED STATISTICAL AREAS (CBSAs), WHICH INCLUDES METRO. STAT. AREAS
CBSA <- core_based_statistical_areas(cb = TRUE, year = 2023)
plot(CBSA)
CBSA
CBSA_vect <- vect(CBSA)
CBSA_vect

#PULLING IN AND CHECKING POWER PLANT DATA
all_power <- vect("~/Wageningen/University/Thesis/Data/EIA/Power Plants geospatial/Power_Plants.geojson")
plot(all_power)
all_power

#PULLING IN AND CHECKING LAND VALUE DATA
landval <- rast("~/Wageningen/University/Thesis/Data/Land Value Data Nolte 2020/places_fmv_pnas_dryad/1 estimates/places_fmv_all.tif")
plot(landval)
landval

#PULLING IN STATE BORDER DATA AND CHECKING
states <- states(cb = TRUE, year = 2023)
states <- filter(states, !STUSPS %in% c("AK", "HI", "AS", "GU", "MP", "PR", "VI"))
plot(states)
states_vect <- vect(states)
plot(states_vect, border = "black", col = NA)
states_vect

#REPROJECT ALL CRS TO: NAD83 / Conus Albers (EPSG:5070) 
all_power <- project(all_power, "EPSG:5070")
avg_GHI <- project(avg_GHI, "EPSG:5070")
states_vect <- project(states_vect,"EPSG:5070")
CBSA_vect <- project(CBSA_vect, "EPSG:5070")

#SETTING EXTENT TO CONTIGUOUS USA (MATCHING ORIGINAL landval EXTENT)
extent_contUSA <- ext(states_vect)
all_power <- crop(all_power, extent_contUSA)
avg_GHI <- crop(avg_GHI, extent_contUSA) #I'm having trouble here getting this to crop properly
landval <- crop(landval, extent_contUSA)
CBSA_vect <- crop(CBSA_vect, extent_contUSA)

#Addressing old-style csr warning message "old-style crs object detected; please recreate object with a recent sf::st_crs()"
crs(avg_GHI) <- st_crs(5070)$wkt
crs(states_vect) <- st_crs(5070)$wkt

#MASKING avg_GHI TO USA ONLY
avg_GHI <- mask(avg_GHI, states_vect)

#RESAMPLING avg_GHI TO landval
res(landval)
res(avg_GHI)
avg_GHI <- resample(avg_GHI, landval, method = "bilinear")

#NICE
plot(landval)
plot(states_vect, border = 'black', add=T)
plot(all_power, cex=0.1, add=T) #there is one point in Canada?
plot(CBSA_vect, add=T)

#SAVING MODIFIED DATA
writeVector(all_power, "all_power_vect")
writeRaster(avg_GHI, "avg_GHI.tiff")
writeVector(CBSA_vect, "CBSA_vect")
writeRaster(landval, "landval.tiff")
writeVector(states_vect, "states_vect")

#----------------------------------------------------------------------

#ATTEMPTING TO JOIN DATA IN ONE DATAFRAME
rastercombo <- c(landval, avg_GHI_masked)
rastercombo_df <- as.data.frame(rastercombo, xy= T, na.rm = T)
rastercombo_vect <- vect(rastercombo_df, geom = c("x", "y"), crs = crs(landval))


