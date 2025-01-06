library(tidyverse)
library(tidycensus)
library(sf)
library(terra)
library(tigris)


setwd("~/Wageningen/University/Thesis")

PVprojects <- vect("~/Wageningen/University/Thesis/Data/USGC Large Scale Solar PV Data/uspvdbSHP/uspvdb_v2_0_20240801.shp")

plot(PVprojects)

PVprojects

Landval <- rast("~/Wageningen/University/Thesis/Data/Land Value Data Nolte 2020/places_fmv_pnas_dryad/1 estimates/places_fmv_all.tif")

plot(Landval)

Landval

#Reproject Solar Project Data to the CRS of the Land Value data
PVprojects_rp <- project(PVprojects, "EPSG:5070")
PVprojects_rp


#Downloading State Borders and Prepping
stborders <- states(cb=TRUE)
stborders_vt <- vect(stborders)
stborders_vt
stborders_vt <- project(stborders_vt,"EPSG:5070")
lvext <- ext(Landval)
PVprojects_rp
stborders_vt
stborders_vtcrp <- crop(stborders_vt,lvext)
plot(stborders_vtcrp)

#Overlayed Maps
plot(Landval, main="Land Value & PV Projects Overlay")
plot(PVprojects_rp, col="yellow", border="yellow", add=TRUE)
plot(stborders_vtcrp, add=TRUE)





