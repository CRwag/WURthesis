library(tidyverse)
library(tidycensus)
library(terra)
library(tigris)
library(sf)
library(ggplot2)


#PULLING IN AND CLEANING UP POWERLINE DATA
powerlines_vect <- vect("~/Wageningen/University/Thesis/Data/Infrastructure/Electric__Power_Transmission_Lines.geojson")

plot(powerlines_vect)
powerlines_vect

states <- states(cb = TRUE, year = 2023)
states <- filter(states, !STUSPS %in% c("AK", "HI", "AS", "GU", "MP", "PR", "VI"))
plot(states)
states_vect <- vect(states)
extent_contUSA <- ext(states_vect)

powerlines_vect <- crop(powerlines_vect, extent_contUSA)
powerlines_vect <- project(powerlines_vect, "EPSG:5070")

plot(powerlines_vect)
powerlines_vect
extent_contUSA

#PULLING IN AND CHECKING POWER PLANT DATA
all_power <- vect("~/Wageningen/University/Thesis/Data/EIA/Power Plants geospatial/Power_Plants.geojson")
all_power <- crop(all_power, extent_contUSA)
all_power <- project(all_power, "EPSG:5070")

plot(all_power)
all_power

names(powerlines_vect)

HV_powerlines_vect <- powerlines_vect[powerlines_vect$VOLTAGE >= 69,] #I chose 69kv because that's generally when lines are considered to be "high voltage", and assumed they would not build HV lines to connect the power plant to the existing HV transmission lines
#distances_PL_to_PP <- distance(all_power, HV_powerlines_vect) #takes like 20 min to run and produces a 7.6GB matrix
nrow(distances_PL_to_PP)
ncol(distances_PL_to_PP)
mindists <- apply(distances_PL_to_PP, 1, min)
mindists_df <- as.data.frame(mindists)
mindists_df <- as.data.frame(t(mindists_df))

#COMPARING WITH POWER PLANT SITES
all_power_df  <- as.data.frame(all_power)

all_power_df <- cbind(all_power_df, mindists_df)

avgstdists <- all_power_df %>%
  group_by(State) %>%
  summarise(mean(V1))

barplot(avgstdists$`mean(V1)`, 
        names.arg = avgstdists$State, 
        las = 2,          # Rotate axis labels for better readability
        main = "Mean Distance from Power Plant to HV Powerline by State", 
        ylab = "Mean Distance (meters)", 
        col = "skyblue")

states_vect <- project(states_vect,"EPSG:5070")
plot(powerlines_vect, col = "green")
plot(states_vect, add = T)
plot(all_power, cex = 0.1, add = T)

all_power_df <- all_power_df %>%
  rename("dist_to_HVline" = V1)

write_rds(all_power_df, "~/Wageningen/University/Thesis/WURthesisGithub/Modified Data/all_power_with_HVline_dist.rds")


#JOIN DISTANCE VALUES WITH master.raster.df
master.raster_df <- readRDS("~/Wageningen/University/Thesis/WURthesisGithub/Modified Data/master_raster_df.rds")

names(master.raster_df)
master.raster_df <- master.raster_df %>%
  left_join(all_power_df %>%
              select(OBJECTID, dist_to_HVline), by = "OBJECTID")






