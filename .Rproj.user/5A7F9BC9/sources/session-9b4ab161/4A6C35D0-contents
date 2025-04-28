library(tidyverse)
library(tidycensus)
library(terra)
library(tigris)
library(sf)
library(ggplot2)

#Pulling in master.raster_reduced_Mainmatch, created in script "1-15-25 Maine CEM"
master.raster_reduced_Mainematch <- read_rds("~/Wageningen/University/Thesis/Data/1. Modified Data/master_raster_V2_reduced_mainematch.rds")

#Pulling in full master_raster
Master_raster <- read_rds("~/Wageningen/University/Thesis/Data/1. Modified Data/master_raster_df_v2.rds")



#CREATING % PROD. CAPCITY FROM SOLAR 
#Pulling Install_MW from master_raster
master.raster_reduced_Mainematch <- master.raster_reduced_Mainematch %>%
  left_join(Master_raster %>% 
              select(cell, Install_MW), by = "cell")

#Pulling Solar_MW from master_raster
master.raster_reduced_Mainematch <- master.raster_reduced_Mainematch %>%
  left_join(Master_raster %>% 
              select(cell, Solar_MW), by = "cell")

# % of capacity from Solar in Maine
solar_percentage_maine <- master.raster_reduced_Mainematch %>%
  filter(Maine == 1) %>%
  summarise(percentage_solar = sum(Solar_MW, na.rm = TRUE) / sum(Install_MW, na.rm = TRUE) * 100)

# % of capacity from solar in counterfactual Maine
solar_percentage_counterfactual <- master.raster_reduced_Mainematch %>%
  filter(Maine == 0, `MaineCEM[["matched"]]` == T) %>%
  summarise(percentage_solar = sum(Solar_MW, na.rm = TRUE) / sum(Install_MW, na.rm = TRUE) * 100)

print(solar_percentage_maine)
print(solar_percentage_counterfactual)



#Creating map of Maine and counterfactual Maine
#Pulling in Landval raster to use as raster template
landval <- rast("~/Wageningen/University/Thesis/Data/1. Modified Data/landval.tiff")
landval

#Adding variables to full master raster file so I have the full raster data and can turn back into a Spatraster
Master_raster <- Master_raster %>%
  left_join(
    master.raster_reduced_Mainematch %>%
      select(cell, Maine, `MaineCEM[["matched"]]`, strata),
    by = "cell")

#Creating data frame for map
CEM_map_df <- Master_raster %>%
  mutate(
    match_status = case_when(
      `MaineCEM[["matched"]]` == TRUE & Maine == 1 ~ "Maine Matched",
      `MaineCEM[["matched"]]` == TRUE & Maine == 0 ~ "Non-Maine Matched",
      `MaineCEM[["matched"]]` == FALSE & Maine == 1 ~ "Maine Non-Matched",
      TRUE ~ NA_character_)) %>%
  select(x, y, match_status )

str(CEM_map_df)
CEM_map_df <- CEM_map_df %>%
  mutate(match_status = as.factor(match_status))

remove(Master_raster)
remove(master.raster_reduced_Mainematch)
gc()

#Turning CEM_map_df into a Spatraster
CEM_map_df$match_status <- as.numeric(factor(CEM_map_df$match_status, exclude = NULL))
str(CEM_map_df)
summary(CEM_map_df$match_status)

CEM_map_rast <- rast(CEM_map_df, type = "xyz", crs = crs(landval))
plot(CEM_map_rast)

#Masking
state_borders <- vect("~/Wageningen/University/Thesis/Data/1. Modified Data/states_vect/states_vect.shp")
state_borders
plot(state_borders)

us_mask <- rasterize(state_borders, CEM_map_rast, field=1)
us_mask
plot(us_mask)

CEM_map_rast[is.na(us_mask)] <- 5

plot(CEM_map_rast)

plot(CEM_map_rast, 
     col = c("#304529", "red", "#77ab59", "#dce3c7", "#536878"))

#Finalizing Map
plot(CEM_map_rast, 
     col = c("#304529", "red", "#77ab59", "#dce3c7", "#536878"),
     legend = FALSE,
     main = "Maine CEM")

legend("bottomleft",
       legend = c("Matched Maine cells", 
                  "Unmatched Maine cells", 
                  "Matched counterfactual cells", 
                  "Unmatched non-Maine cells"),
       fill = c("#304529", "red", "#77ab59", "#dce3c7"),
       border = "black",
       bty = "n",
       cex = 0.9)

plot(state_borders, add= T)

writeRaster(CEM_map_rast, "~/Wageningen/University/Thesis/Data/1. Modified Data/2. Models/MaineCEM/MaineCEM_rast.tif")

#Checking why the map shows Unmatched non-Maine cells within the borders of Maine
Maine_ext <- ext(1927272, 2276613, 2496501, 3022728)

plot(CEM_map_rast, 
     col = c("#304529", "red", "#77ab59", "#dce3c7", "#536878"),
     legend = FALSE,
     main = "Maine CEM: View of Maine",
     ext = Maine_ext)

legend("bottomleft",
       legend = c("Matched Maine cells", 
                  "Unmatched Maine cells", 
                  "Matched counterfactual cells", 
                  "Unmatched non-Maine cells"),
       fill = c("#304529", "red", "#77ab59", "#dce3c7"),
       border = "black",
       bty = "n",
       cex = 0.9)

plot(state_borders, add= T)
#Oh they're lakes, thank god.

#Checking the distribution of matches in each strata
#Number of cells per strata
cells_per_strata <- master_raster_V2_reduced_mainematch %>%
  filter(`MaineCEM[["matched"]]` == TRUE) %>%
  count(strata)

#Number of unique strata within Maine
unique_strata_maine <- master_raster_V2_reduced_mainematch %>%
  filter(Maine == 1) %>%
  summarize(unique_strata = n_distinct(strata))

#Number of unique strata outside of Maine
unique_strata_non_maine <- master_raster_V2_reduced_mainematch %>%
  filter(Maine == 0) %>%
  summarize(unique_strata = n_distinct(strata))

print(unique_strata_maine)
print(unique_strata_non_maine)

# Histogram of matched counts by strata
ggplot(cells_per_strata, aes(x = strata, y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Number of Matches per Strata",
       x = "Strata",
       y = "Count of Matches")

gc()

#Only strata that appear in Maine
maine_strata <- master_raster_V2_reduced_mainematch %>%
  filter(Maine == 1) %>%
  select(strata) %>%
  distinct()

#Counting how many cells have Maine strata in the non-Maine cells
matched_counts <- master_raster_V2_reduced_mainematch %>%
  filter(Maine == 0, strata %in% maine_strata$strata) %>%
  group_by(strata) %>%
  summarise(count = n())

summary(matched_counts$count)

#Counting how many Maine cells are in each Maine strata
maine_cells_per_strata <- master_raster_V2_reduced_mainematch %>%
  filter(Maine == 1) %>%
  group_by(strata) %>%
  summarise(count = n())

summary(maine_cells_per_strata$count)

#histogram of Maine cells per Maine strata
ggplot(maine_cells_per_strata, aes(x = count)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +
  labs(title = "Distribution of Maine Cells per Strata",
       x = "Number of Cells per Strata",
       y = "Frequency")


write_rds(maine_cells_per_strata,"~/Wageningen/University/Thesis/Data/1. Modified Data/2. Models/MaineCEM/maine_cells_per_strata.rds" )
write_rds(maine_strata,"~/Wageningen/University/Thesis/Data/1. Modified Data/2. Models/MaineCEM/maine_strata.rds" )

