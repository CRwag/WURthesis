install.packages("readxl")

library(tidyverse)
library(tidycensus)
library(terra)
library(tigris)
library(sf)
library(ggplot2)
library(readxl)
library(geodata)

Gendata <- read_xlsx("~/Wageningen/University/Thesis/Data/EIA/EIA-860M/september_generator2024.xlsx")

Gendata <- Gendata[-1, ] 
colnames(Gendata) <- Gendata[1, ]
Gendata <- Gendata[-1, ] 

cols_to_convert <- c(
  "Entity ID", "Plant ID", "Nameplate Capacity (MW)", "Net Summer Capacity (MW)", 
  "Net Winter Capacity (MW)", "Operating Month", "Operating Year", 
  "Planned Retirement Month", "Planned Retirement Year", 
  "Nameplate Energy Capacity (MWh)", "DC Net Capacity (MW)", 
  "Planned Derate Year", "Planned Derate Month", 
  "Planned Derate of Summer Capacity (MW)", "Planned Uprate Year", 
  "Planned Uprate Month", "Planned Uprate of Summer Capacity (MW)", 
  "Latitude", "Longitude"
)

Gendata[cols_to_convert] <- lapply(Gendata[cols_to_convert], as.numeric)

Gendata_vect <- vect(Gendata, geom = c("Longitude", "Latitude"), crs = "EPSG:4326")
plot(Gendata_vect)

master.raster_df <- read_rds("~/Wageningen/University/Thesis/WURthesisGithub/Modified Data/master_raster_df.rds")

names(master.raster_df)
head(master.raster_df)

sum(!is.na(master.raster_df$nsrdb3_ghi))
sum(!is.na(master.raster_df$places_fmv_all))
sum(!is.na(master.raster_df$Plant_Name))

sum(is.na(Gendata$`Plant Name`))

Gendata <- Gendata %>%
  filter(!is.na(`Plant Name`))

master.raster_df %>%
  filter(!is.na(Plant_Code)) %>%
  head()

Gendata <- Gendata %>%
  select(-`Google Map`, -`Bing Map`)

n_distinct(Gendata$`Plant ID`)

master.raster_df %>%
  filter(!is.na(Plant_Code)) %>%
  summarise(unique_count = n_distinct(Plant_Code))


#INCLUDING ELEVATION/SLOPE DATA
landval <- rast("~/Wageningen/University/Thesis/WURthesisGithub/Modified Data/landval.tiff")
landval

Elevation <- elevation_30s(country = "USA", path = "~/Wageningen/University/Thesis/Data")
plot(Elevation)
Elevation
slope <- terrain(Elevation, v = "slope", neighbors = 8, unit = "degrees")

plot(slope)
landval
slope_rp <- project(slope, "EPSG:5070")
slope_rp
plot(slope_rp)

slope_rp <- resample(slope_rp, landval, method = "bilinear")
extent <- ext(landval)
slope_rp  <- crop(slope_rp, extent)
plot(slope_rp)
slope_rp

writeRaster(slope_rp, "~/Wageningen/University/Thesis/WURthesisGithub/Modified Data/Slope_rast.tif")
slope_df <- as.data.frame(slope_rp, xy=T, cells=T)

names(master.raster_df)
master.raster_df <- master.raster_df %>%
  left_join(slope_df %>%
              select(cell, slope), by = "cell")


##master.raster CLEANUP

#RENAMING VARIABLES
names(master.raster_df)
master.raster_df <- master.raster_df %>%
  rename(landval_ln = places_fmv_all) %>%
  rename(plant_longitude = Longitude) %>%
  rename(plant_latitude = Latitude)

#REORDERING
master.raster_df <- master.raster_df %>%
  select(cell, x, y, landval_ln, nsrdb3_ghi, slope, everything())

write_rds(master.raster_df, "~/Wageningen/University/Thesis/WURthesisGithub/Modified Data/master_raster_df.rds")


