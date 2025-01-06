library(tidyverse)
library(sf)
library(readxl)
library(geodata)

#PULLING IN master_raster_df
master.raster <- read_rds("~/Wageningen/University/Thesis/Data/1. Modified Data/master_raster_df.rds")

#CELLS THAT CONTAIN MULTIPLE POWER PLANTS
duplicates <- master.raster %>%
  group_by(cell) %>%
  filter(n() > 1) %>%
  ungroup()

n_dupcells <-duplicates %>%
  count(cell)

#COMBINED POWER PLANTS INTO 1 ROW PER CELL
combo_dups <- duplicates %>%
  group_by(cell) %>%
  summarise(
    all_plants_in_cell = paste(Plant_Name, collapse = "; "),
    all_primsources_in_cell = paste(PrimSource, collapse = "; "),
    pp_avg_dist_to_HVline = mean(dist_to_HVline),
    across(ends_with("_MW"), sum, na.rm = TRUE),
    .groups = "drop")

#CHECKING # OF CELLS THERE SHOULD BE BY LOOKING AT THE DIMENSIONS OF THE CROPPED landval.tif
landval <- rast("~/Wageningen/University/Thesis/Data/1. Modified Data/landval.tiff")
landval


#CUTTING DOWN master.raster TO JUST CELL LEVEL DATA
names(master.raster)

master.raster_v2 <- master.raster %>%
   select(cell:NAMELSAD, dist_to_HVline, Plant_Name, County, State, PrimSource, Install_MW:Other_MW)


#REMOVING DUPLICATED CELLS IN PREPARATION TO ADD THEM BACK WITH THE AGGREGATED DATA
master.raster_v2 <- master.raster_v2 %>%
  filter(!cell %in% combo_dups$cell)


#MATCHING VARIABLES TO master.raster_v2
combo_dups <- combo_dups %>%
  left_join(duplicates %>%
              select(cell, x, y, landval_ln, nsrdb3_ghi, slope, NAMELSAD, County, State), by = "cell")

combo_dups <- combo_dups %>%
  distinct(cell, .keep_all = T)


#COMBINING master.raster_v2 AND combo_dups
names(master.raster_v2)
names(combo_dups)

master.raster_v2 <- master.raster_v2 %>%
  rename(all_plants_in_cell = Plant_Name,
         all_primsources_in_cell = PrimSource,
         pp_avg_dist_to_HVline = dist_to_HVline)

remove(master.raster)

master.raster_v2 <- master.raster_v2 %>%
  bind_rows(combo_dups)

anyDuplicated(master.raster_v2$cell)

write_rds(master.raster_v2, "~/Wageningen/University/Thesis/Data/1. Modified Data/master_raster_df_v2.rds")
