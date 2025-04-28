library(tidyverse)
library(terra)
library(sf)
library(ggplot2)
library(cem)
library(readxl)

#Preface: I had all sorts of trouble here trying to match the generator locations (given in lat/long) with the cell they fell on in my raster. Finally found a solution using extract() and only can take the x,y not the cell ID.


###Creating panel data, using the strata as the unit observation


##Creating data frame from CEM match

#Pulling in CEM match
MaineCEMv4.1 <- readRDS("~/Wageningen/University/Thesis/Data/1. Modified Data/2. Models/MaineCEM/MaineCEMv4.1.rds")

#Creating data frame object
MaineCEMv4.1_df <- data.frame(
  strata = MaineCEMv4.1$strata,
  group = MaineCEMv4.1$groups,
  matched = MaineCEMv4.1$matched
)

head(MaineCEMv4.1_df)


#pulling in reduced master raster
master_raster_V2_reduced <- readRDS("~/Wageningen/University/Thesis/Data/1. Modified Data/master_raster_V2_reduced.rds")
head(master_raster_V2_reduced)

#joining
master_raster_V2_reduced <- bind_cols(master_raster_V2_reduced, MaineCEMv4.1_df)
head(master_raster_V2_reduced)

#reorganizing
master_raster_V2_reduced <- master_raster_V2_reduced %>%
  select(cell, strata, group, matched, everything())
head(master_raster_V2_reduced)


##Creating annual operating generator data sets

#Pulling in September EIA 860M data
Op_gens_df <- read_excel("~/Wageningen/University/Thesis/Data/EIA/EIA-860M/september_generator2024.xlsx", sheet = "Operating", range = "A3:AG26410")
Ret_gens_df <- read_excel("~/Wageningen/University/Thesis/Data/EIA/EIA-860M/september_generator2024.xlsx", sheet = "Retired", range = "A3:Z6711")
view(Op_gens_df)
view(Ret_gens_df)

#Selecting columns
Op_gens_df <- Op_gens_df %>%
  select(`Entity ID`, `Entity Name`, `Plant ID`, `Plant Name`, `Plant State`, `Generator ID`, `Nameplate Capacity (MW)`, Technology, `Operating Month`, `Operating Year`, Latitude, Longitude)

Ret_gens_df <- Ret_gens_df %>%
  select(`Entity ID`, `Entity Name`, `Plant ID`, `Plant Name`, `Plant State`, `Generator ID`, `Nameplate Capacity (MW)`, Technology, `Operating Month`, `Operating Year`,`Retirement Month`,`Retirement Year`, Latitude, Longitude)

Op_gens_df <- Op_gens_df %>%
  mutate(`Retirement Month` = NA, `Retirement Year` = NA)

#Combining data
gens_df <- bind_rows(Op_gens_df, Ret_gens_df)


#altering start and retirement years
gens_df <- gens_df %>%
  mutate(
    `Adjusted Operating Year` = case_when(
      `Operating Month` <= 6 ~ `Operating Year`,       # If generator started operating in the first half of the year, I'll count it towards that year
      `Operating Month` > 6  ~ `Operating Year` + 1    # If generator started operating in the second half of the year, I'll only start to count it from the next year onwards
    ),
    `Adjusted Retirement Year` = case_when(
      is.na(`Retirement Year`) ~ NA_integer_,          # If Retirement Year is NA, keep NA
      `Retirement Month` > 6 ~ `Retirement Year`,      # If a generator retires in the second half of the year, I'll consider it operational for that year
      `Retirement Month` <= 6  ~ `Retirement Year` - 1  # If a generator retires in the first half of the year, subtract 1 year
    )
  )

#Removing retirement and operating months/years
gens_df <- gens_df %>%
  select(-`Operating Month`, -`Retirement Month`, -`Operating Year`, -`Retirement Year`)


##Determining which cells/strata each generator lies in 

#pulling landval raster for dimensions and CRS
landval <- rast("~/Wageningen/University/Thesis/Data/Land Value Data Nolte 2020/places_fmv_pnas_dryad/1 estimates/places_fmv_all.tif")
landval

#realized some of the retired generators don't have locations
sum(is.na(gens_df$Latitude)) #22 retired generators are missing lat values
sum(is.na(gens_df$Longitude)) #25 are missing long value
sum(is.na(gens_df$Latitude) | is.na(gens_df$Longitude)) #28 are missing at least one of the values

gens_df %>% 
  filter(is.na(Latitude) | is.na(Longitude))

#Removing generators which do not have location data
gens_df <- gens_df %>% filter(!is.na(Latitude) & !is.na(Longitude))

#removing generators which are outside of the contiguous USA
summary(gens_df)
gens_df <- gens_df %>%
  mutate(`Plant State` = as.factor(`Plant State`))
levels(gens_df$`Plant State`)
gens_df <- gens_df %>%
  filter(`Plant State` != "AK" & `Plant State` != "HI") %>%
  mutate(`Plant State` = droplevels(`Plant State`))

#Turning generator data into a spatial object
gens_sf <- st_as_sf(gens_df, coords = c("Longitude", "Latitude"), crs = 4326)  # Assuming original CRS is WGS84, I can't find anything on EIA that says so, but I think it'll work
gens_sf <- st_transform(gens_sf, crs = 5070)
gens_vect <- vect(gens_sf)
plot(gens_vect)
gens_vect

####FROM HERE IS GARBAGE, MUST SKIP TO LINE 175
#finding which cell each generator lies in
help("cells")
gens_df$cell <- cells(landval, gens_vect)



head(gens_df)
max(gens_df$cell)
min(gens_df$cell)
names(gens_df)

#keeping just the cell in the data frame, not cell and ID
gens_df <- gens_df %>%
  mutate(cell = cell[, "cell"])

#moving cell column
gens_df <- gens_df %>%
  select(cell, everything())


##Aggregating generator data to the cell level (cell is unit of observation)
#turning technology into a factor
gens_df <- gens_df %>%
  mutate("Technology" = as.factor(`Technology`))

levels(gens_df$Technology)

#transforming technology/capacity to wide format
gens_df_wide <- gens_df %>%
  pivot_wider(
    names_from = Technology,
    values_from = `Nameplate Capacity (MW)`,
    values_fill = list(`Nameplate Capacity (MW)` = 0)
  )

#need every generator to have a retirement date, so will give NAs = 2025 and then remove later. Only retired generators have a retired date listed, so no current 2025s in data set.
gens_df_wide_2025 <- gens_df_wide %>%
  mutate(`Adjusted Retirement Year` = ifelse(is.na(`Adjusted Retirement Year`), 2025, `Adjusted Retirement Year`))

#Creating one row per year per generator
expanded_gens <- gens_df_wide_2025 %>%
  rowwise() %>%
  mutate(year = list(seq(`Adjusted Operating Year`, `Adjusted Retirement Year`))) %>%
  unnest(year) %>%
  ungroup()

#aggregating at the cell-year level
cell_year_capacity <- expanded_gens %>%
  select(-c(`Entity ID`, `Entity Name`, `Plant ID`, `Plant Name`, `Generator ID`, 
            `Latitude`, `Longitude`, `Adjusted Operating Year`, `Adjusted Retirement Year`)) %>% 
  group_by(cell, year) %>%
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)), .groups = "drop")

#checking number of unique cells match 
n_distinct(gens_df$cell)
n_distinct((cell_year_capacity$cell))

#Warning: This is where I learned of my cell matching problem------------------
#bringing in strata IDs
cell_year_capacity <- cell_year_capacity %>%
  left_join(master_raster_V2_reduced %>% select(cell, strata), by = "cell")

NAstrata <- cell_year_capacity %>%
  filter(is.na(strata)) %>%
  distinct(cell)

NAstrata <- NAstrata %>%
  left_join(gens_df %>%
              group_by(cell) %>%
              summarise(
                `Plant State` = first(`Plant State`),
                Latitude = first(Latitude),
                Longitude = first(Longitude)
              ), 
            by = "cell")

NAstrata_sf <- st_as_sf(NAstrata, coords = c("Longitude", "Latitude"), crs = 4326)
NAstrata_sf <- st_transform(NAstrata_sf, crs = 5070)
NAstrata_vect <- vect(NAstrata_sf)
plot(NAstrata_vect)

#OK, so the cell IDs I have for the generators are clearly incorrect

landval_df <- as.data.frame(landval, xy = TRUE, cells = TRUE)
missing_cells <- anti_join(landval_df, master_raster_V2_reduced, by = "cell")

master_raster_df_v2 <- readRDS("~/Wageningen/University/Thesis/Data/1. Modified Data/master_raster_df_v2.rds")

master_raster_df_v2 <- master_raster_df_v2 %>%
  select(cell, x, y,)


values(master_raster) <- master_raster_df_v2$State
plot(master_raster)


##TESTING CREATING DIFFERENT MAPS FROM DATA IN MASTER RASTER DATA FRAME --- ALL WORKING CORRECTLY-- NOT SURE WHY I'M HAVING PROBLEMS MATCHING GENERATOR DATA
# Convert State to a numeric factor
statemap_df <- master_raster_V2_reduced %>%
  select(x, y, State) %>%
  mutate(State = as.numeric(factor(State, exclude = NULL)))  # Convert to numeric

# Check structure
str(statemap_df)

# Create raster
statemap <- rast(statemap_df, type = "xyz", crs = crs(landval))
statemap

# Plot the raster
plot(statemap)

head(landval_df)
master_raster_V2_reduced %>%
  filter(x == -1979685, y == 3172725)

# Convert State to a numeric factor
landvalmap_df <- master_raster_V2_reduced %>%
  select(x, y, landval_ln)

# Check structure
str(landvalmap_df)

# Create raster
landvalmap <- rast(landvalmap_df, type = "xyz", crs = crs(landval))
landvalmap

# Plot the raster
plot(landvalmap)



gens_rast <- rasterize(gens_vect, landval, field = "Nameplate Capacity (MW)", fun = "sum")
gens_rast
plot(gens_rast)

## TRYING TO PULL CELLS AND X,Y VALS AGAIN

gens_vect <- project(gens_vect, crs(landvalmap))

# Get the cell number from landval for each generator
gens_df$cell2 <- cells(landvalmap, gens_vect)

gens_df <- gens_df %>%
  mutate(cell2 = cell2[, "cell"])

# Extract the x, y coordinates of each corresponding cell in the raster
xandy2 <- xyFromCell(landval, gens_df$cell2)

#trying again
xandy3 <- extract(landvalmap, gens_vect, cells = T, method = "simple", xy= T)

master_raster_V2_reduced %>%
  filter(cell == 39668391)

master_raster_V2_reduced %>%
  filter(x == 797115, y == 1190805)
#finally a reasonable result. The cell ID is not reasonable when matched with the master_rasterV2_df, but the x,y values match up 








#End of troubleshooting - back to creating panel dataset
gens_vect <- cbind(gens_vect, extract(landvalmap, gens_vect, cells = TRUE, method = "simple", xy = TRUE))
gens_vect
gens_df2 <- as.data.frame(gens_vect)


##Aggregating generator data to the cell level (cell is unit of observation)
#turning technology into a factor
gens_df2 <- gens_df2 %>%
  mutate("Technology" = as.factor(`Technology`))

levels(gens_df2$Technology)

#transforming technology/capacity to wide format
gens_df_wide2 <- gens_df2 %>%
  pivot_wider(
    names_from = Technology,
    values_from = `Nameplate Capacity (MW)`,
    values_fill = list(`Nameplate Capacity (MW)` = 0)
  )

#need every generator to have a retirement date, so will give NAs = 2025 and then remove later. Only retired generators have a retired date listed, so no current 2025s in data set.
gens_df_wide2_2025 <- gens_df_wide2 %>%
  mutate(`Adjusted Retirement Year` = ifelse(is.na(`Adjusted Retirement Year`), 2025, `Adjusted Retirement Year`))

#Creating one row per year per generator
expanded_gens2 <- gens_df_wide2_2025 %>%
  rowwise() %>%
  mutate(year = list(seq(`Adjusted Operating Year`, `Adjusted Retirement Year`))) %>%
  unnest(year) %>%
  ungroup()

#pulling strata values into generator data
expanded_gens2 <- expanded_gens2 %>%
  left_join(master_raster_V2_reduced %>% select(x, y, strata), by = c("x", "y"))


#Another problem: I only used the cells which had a value for landval, 1. because I needed to remove NAs and 2. becuase I assumed that the cells which did not have a landval value was not suitable to build on (e.g. water, or at least marshland, swampland, etc.)
#Now I am coming to realize that 118 of the 32035 power generators in the US did in fact exist upon this land which had no landval value:

missing_cells_strata_df <- expanded_gens2 %>%
  filter(is.na(strata)) %>%
  select(everything()) %>%
  distinct(cell, .keep_all = TRUE)

missing_cells_sf <- missing_cells_strata_df %>%
  select(x, y) %>%
  distinct()

missing_cells_vect <- vect(missing_cells_sf, geom = c("x", "y"), crs = crs(landvalmap))

#Adding matched column to generator data
expanded_gens2 <- expanded_gens2 %>%
  left_join(master_raster_V2_reduced %>% select(x, y, matched), by = c("x", "y"))

expanded_gens2 %>%
  filter(matched == TRUE) %>%
  summarise(unique_cells = n_distinct(cell))

write_rds(expanded_gens2, "~/Wageningen/University/Thesis/Data/1. Modified Data/expanded_gens2.rds")


#aggregating to the strata level
strata_year_capacity <- expanded_gens2 %>%
  select(-c(`Entity ID`, `Entity Name`, `Plant ID`, `Plant Name`, `Plant State`, 
            `Generator ID`, `ID`, `x`, `y`, `landval_ln`, `cell`, 
            `Adjusted Operating Year`, `Adjusted Retirement Year`)) %>%
  group_by(strata, year) %>%
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)), .groups = "drop")

write_rds(strata_year_capacity, "~/Wageningen/University/Thesis/Data/1. Modified Data/strata_year_capacity.rds")

#adding back in matched column
strata_year_capacity <- strata_year_capacity %>%
  left_join(
    expanded_gens2 %>%
      select(strata, matched) %>%
      distinct(strata, .keep_all = TRUE), 
    by = "strata"
  )




##Combining strata-year capacity data with the other variables
#Creating structure for new panel data set
years <- tibble(year = 2000:2024)

matched_strata <- master_raster_V2_reduced %>%
  filter(matched == TRUE) %>%
  distinct(strata)

panel_data <- matched_strata %>%
  cross_join(years)

#adding in group (0 = Not Maine aka control, 1 = Maine aka treatment)
panel_data <- panel_data %>%
  left_join(
    master_raster_V2_reduced %>%
      select(strata, group) %>%
      distinct(strata, .keep_all = TRUE), 
    by = "strata"
  )

#Pulling in generator data
panel_data <- panel_data %>%
  left_join(strata_year_capacity %>% select(-matched), by = c("strata", "year")) %>%
  mutate(across(where(is.numeric), ~ replace_na(.x, 0)))

#Creating a total capacity column
panel_data <- panel_data %>%
  mutate(Total_Capacity = rowSums(select(., `Conventional Hydroelectric`:`Offshore Wind Turbine`), na.rm = TRUE))

#Creating column for % of capcity from solar PV
panel_data <- panel_data %>%
  mutate(
    Solar_Percentage = ifelse(Total_Capacity > 0, 
                              (`Solar Photovoltaic` / Total_Capacity) * 100, 
                              0)
  )

#creating shock variable (1 = post shock, 0 = pre shock)
panel_data <- panel_data %>%
  mutate(shock = ifelse(year >= 2019, 1, 0))

#creating treated-post interaction variable
str(panel_data)

panel_data <- panel_data %>%
  mutate(
    group = as.numeric(as.character(group)))

panel_data <- panel_data %>%
  mutate(treated_post = group * shock)


write_rds(panel_data,"~/Wageningen/University/Thesis/Data/1. Modified Data/3. Panel Data/panel_data_v1.rds")


##Calculating percentages for each state
#Only matched strata
matched_data_no_Maine <- master_raster_V2_reduced %>%
  filter(matched == TRUE, Maine == 0)

#Counting number of cells per state within each strata
state_counts <- matched_data_no_Maine %>%
  group_by(strata, State) %>%
  summarise(state_count = n(), .groups = "drop")

#Counting total cells per strata
total_counts <- matched_data_no_Maine %>%
  group_by(strata) %>%
  summarise(total_count = n(), .groups = "drop")

#final state percentages
state_percentage <- state_counts %>%
  left_join(total_counts, by = "strata") %>%
  mutate(percentage = (state_count / total_count) * 100)


##Adding YCOM data

#Pulling in Yale Climate Opinion data
YCOM <- read_excel("~/Wageningen/University/Thesis/Data/Yale Climate Opinions/YCOM7_publicdata.xlsx", 
                   sheet = "YCOM_2010_2023")

#filtering to fundrenewables data
YCOM <- YCOM %>%
  filter(varname == "fundrenewables", geotype == "state")

#Switching to long format
YCOM_long <- YCOM %>%
  pivot_longer(cols = starts_with("x"), 
               names_to = "year", 
               names_prefix = "x",
               values_to = "YCOM_value") %>%
  mutate(year = as.integer(year)) # Convert year column to integer

#Pulling in abb. to full state names
statenames <- read_excel("~/Wageningen/University/Thesis/Data/50 states and abbreviations.xlsx")

#Adding full state names to YCOM data
YCOM_long <- YCOM_long %>%
  left_join(statenames, by = c("geoname" = "State"))

#taking only counterfactual strata-years from 2010-2023
panel_data_cfs <- panel_data %>%
  filter(group == 0, year >= 2010, year <= 2023)

#bring in state weights per strata
panel_data_weighted <- panel_data_cfs %>%
  left_join(state_percentage, by = "strata")

#bring in YCOM data
panel_data_weighted <- panel_data_weighted %>%
  left_join(YCOM_long, by = c("State" = "Abbreviation", "year" = "year"))

#multiplying weights
panel_data_weighted <- panel_data_weighted %>%
  mutate(weighted_value = (percentage / 100) * YCOM_value)

#summing weighted values
weighted_averages <- panel_data_weighted %>%
  group_by(strata, year) %>%
  summarise(YCOM_weighted_avg = sum(weighted_value, na.rm = TRUE), .groups = "drop")

#Pulling weighted YCOM data into panel data
panel_data <- panel_data %>%
  left_join(weighted_averages, by = c("strata", "year")) %>%
  mutate(YCOM_weighted_avg = ifelse(group == 1, NA, YCOM_weighted_avg))

#YCOM just for Maine
maine_YCOM <- YCOM_long %>%
  filter(geoname == "Maine", year >= 2010, year <= 2023) %>%
  select(year, YCOM_value)

#Pulling Maine values into group = 1 
panel_data <- panel_data %>%
  left_join(maine_YCOM, by = "year") %>%
  mutate(YCOM_value = ifelse(group == 1, YCOM_value, NA))

#sticking the values in the same column
panel_data <- panel_data %>%
  mutate(YCOM_weighted_avg = ifelse(group == 1, YCOM_value, YCOM_weighted_avg))

#trimming to 2010-2023
panel_data_V2 <- panel_data %>%
  select(-YCOM_value) %>%
  filter(year >= 2010, year <= 2023)

write_rds(panel_data_V2, "~/Wageningen/University/Thesis/Data/1. Modified Data/3. Panel Data/panel_data_v2.rds")


##pulling median household income data from census using tidycensus
years <- 2010:2023
income_var <- "B19013_001"
income_data_list <- list()

#data pull
for (yr in years) {
  income_data <- get_acs(
    geography = "state",
    variables = income_var,
    year = yr,
    survey = "acs5"
  ) %>%
    mutate(year = yr)  # Add the year column
  
  income_data_list[[as.character(yr)]] <- income_data
}

#into one dataset
state_income_data <- bind_rows(income_data_list)

#cleanup
state_income_data <- state_income_data %>%
  rename(
    state = NAME,
    median_income = estimate
  ) %>%
  select(state, year, median_income)



###creating a strata_year_panel from the cell_year_panel

strata_group_year_panel <- cell_year_panel %>%
  group_by(strata, group, year, Land_usage_cat) %>%
  summarise(
    total_weight = sum(weight),
    Total_Capacity = sum(Total_Capacity * weight),
    Solar_Photovoltaic = sum(`Solar Photovoltaic` * weight),
    dem_lean = sum(dem_lean * weight) / sum(weight),
    .groups = "drop"
  )

#creating shock and treated_post 
strata_group_year_panel <- strata_group_year_panel %>%
  mutate(
    shock = ifelse(year >= 2019, 1, 0),
    treated_post = shock * group
  )

#creating shock_lag and treated_post_lag 
strata_group_year_panel <- strata_group_year_panel %>%
  mutate(
    shock_lag = ifelse(year >= 2020, 1, 0),
    treated_post_lag = shock_lag * group
  )

#creating solar_share 
strata_group_year_panel <- strata_group_year_panel %>%
  mutate(
    solar_share = ifelse(Total_Capacity > 0, (Solar_Photovoltaic / Total_Capacity), 0)
  )

#creating solar share relative to group
strata_group_year_panel <- strata_group_year_panel %>%
  left_join(group_total_capacity, by = c("group", "year")) %>%
  mutate(
    grouprelative_solar_share = Solar_Photovoltaic / Total_Capacity_Group
  )

#creating solar_binary
strata_group_year_panel <- strata_group_year_panel %>%
  mutate(solar_binary = ifelse(Solar_Photovoltaic > 0, 1, 0))

write_rds(strata_group_year_panel, "~/Wageningen/University/Thesis/Data/1. Modified Data/3. Panel Data/strata_group_year_panel.rds")
