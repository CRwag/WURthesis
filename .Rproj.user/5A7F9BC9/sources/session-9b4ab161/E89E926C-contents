library(tidyverse)
library(tidycensus)
library(terra)
library(tigris)
library(sf)
library(ggplot2)

all_powerplants_vect <- vect("~/Wageningen/University/Thesis/WURthesisGithub/Modified Data/all_power_vect/all_power_vect.shp")
avg_GHI_rast <- rast("~/Wageningen/University/Thesis/WURthesisGithub/Modified Data/avg_GHI.tiff")
CBSA_vect <- vect("~/Wageningen/University/Thesis/WURthesisGithub/Modified Data/CBSA_vect/CBSA_vect.shp")
landval_rast <- rast("~/Wageningen/University/Thesis/WURthesisGithub/Modified Data/landval.tiff")
states_vect <- vect("~/Wageningen/University/Thesis/WURthesisGithub/Modified Data/states_vect/states_vect.shp")


#RASTERIZING states_vect$NAME
states_rast <- rasterize(states_vect, landval_rast, field = "NAME", touches = TRUE)
plot(states_rast)


#RASTERIZING CBSA_vect$NAMELSAD
CBSA_rast <- rasterize(CBSA_vect, landval_rast, field = "NAMELSAD", touches = TRUE)
plot(CBSA_rast)


#ASSOCIATING POWER PLANTS WITH RASTER CELLS 
all_powerplants_by_cell <- extract(landval_rast, all_powerplants_vect, cells = TRUE, xy = TRUE)
all_powerplants_by_cell$OBJECTID <- all_powerplants_vect$OBJECTID
all_powerplants_by_cell$ID = NULL
all_powerplants_by_cell$places_fmv_all = NULL

all_powerplants_by_cell <- all_powerplants_by_cell %>%
  select(OBJECTID, everything())

all_powerplants_by_cell <- all_powerplants_by_cell %>%
  rename(rastercell_ID = cell)

all_powerplants_df <- as.data.frame(all_powerplants_vect)

all_powerplants_by_cell <- all_powerplants_by_cell %>%
  left_join(all_powerplants_df %>%
              select(OBJECTID:Latitude), by = "OBJECTID")

all_powerplants_by_cell <-all_powerplants_by_cell %>%
  select(rastercell_ID, x, y, everything())

#598 cells contain multiple power plants
all_powerplants_by_cell %>%
  summarize(sum(duplicated(rastercell_ID)))

all_pp_by_cell_dups <- all_powerplants_by_cell %>%
  group_by(rastercell_ID) %>% 
  filter(n() > 1) %>%
  ungroup()



#TURNING REMAINING SpatRasters INTO DATA FRAMES
landval_df <- as.data.frame(landval_rast, xy = T, cells = T, na.rm = F) # 33,675,482 non-NA observations I assume due to the shape of the US and maybe fewer than GHI due to areas like inland bodies of water?
avg_GHI_df <- as.data.frame(avg_GHI_rast, xy = T, cells = T, na.rm = F) # 34,218,139 non-NA observations
CBSA_df <- as.data.frame(CBSA_rast, xy = T, cells = T, na.rm = F) # 19,414,472 non-NA observations due to CBSAs not including the most rural areas of the country

avg_GHI_df %>%
  summarise(sum(!is.na(nsrdb3_ghi)))

landval_df %>%
  summarise(sum(!is.na(places_fmv_all)))

CBSA_df %>%
  summarise(sum(!is.na(NAMELSAD)))


#COMBINING INTO ONE DATA FRAME
master.raster_df <- landval_df %>%
  left_join(CBSA_df, by = "cell") %>%
  left_join(avg_GHI_df, by = "cell") %>%
  left_join(all_powerplants_by_cell, by = c("cell" = "rastercell_ID"))

names(master.raster_df)
master.raster_df$x.x = NULL
master.raster_df$y.x = NULL
master.raster_df$x.y = NULL
master.raster_df$y.y = NULL


CBSA_df %>%
  summarize(sum(duplicated("cell")))

avg_GHI_df %>%
  summarize(sum(duplicated("cell")))

landval_df %>%
  summarize(sum(duplicated("cell")))

#SAVING master.raster_df
saveRDS(master.raster_df, file = "~/Wageningen/University/Thesis/WURthesisGithub/Modified Data/master_raster_df.rds")


