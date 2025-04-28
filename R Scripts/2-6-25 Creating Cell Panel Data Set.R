library(ggplot2)
library(tidyverse)
library(readxl)


##Creating a cell level panel data file

#creating structure
matched_cells <- master_raster_V2_reduced %>%
  select(cell, x, y, matched) %>%
  filter(matched == T)

cell_year_panel <- expand_grid(cell = matched_cells$cell, year = years)

#giving x  and y vals
cell_year_panel <- cell_year_panel %>%
  left_join(matched_cells %>%
              select(cell, x, y), by = "cell")

#reorder
cell_year_panel <- cell_year_panel %>%
  select(cell, x, y, year)

#pulling in other identifying variables
cell_year_panel <- cell_year_panel %>%
  left_join(master_raster_V2_reduced %>%
              select(cell, State, strata, group), by = "cell")

#creating shock and treated_post variable
cell_year_panel <- cell_year_panel %>%
  mutate(group = as.numeric(as.character(group)))

cell_year_panel <- cell_year_panel %>%
  mutate(
    shock = ifelse(year >= 2019 & year <= 2023, 1, 0),
    treated_post = group * shock
  )

#adding weights
master_raster_V2_reduced <- master_raster_V2_reduced %>%
  mutate(weight = MaineCEMv4.1$w)

head(master_raster_V2_reduced)

cell_year_panel <- cell_year_panel %>%
  left_join(master_raster_V2_reduced %>%
              select(cell, weight), by = "cell")


#pulling in expanded_gens2.rds [from "2-4-25 Creating Panel Data Set"]
expanded_gens2 <- read_rds(~/Wageningen/University/Thesis/Data/1. Modified Data/expanded_gens2.rds)

#aggregating to the cell-year level
cell_year_capacity <- expanded_gens2 %>%
  select(-c(`Entity ID`, `Entity Name`, `Plant ID`, `Plant Name`, `Plant State`, 
            `Generator ID`, `ID`,`Adjusted Operating Year`, `Adjusted Retirement Year`)) %>%
  group_by(cell, year) %>%
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)), .groups = "drop")

#creating total capacity and Solar % columns
cell_year_capacity <- cell_year_capacity %>%
  mutate(Total_Capacity = rowSums(select(., `Conventional Hydroelectric`:`Offshore Wind Turbine`), na.rm = TRUE))

cell_year_capacity <- cell_year_capacity %>%
  mutate(
    Solar_Percentage = ifelse(Total_Capacity > 0, 
                              (`Solar Photovoltaic` / Total_Capacity) * 100, 
                              0)
  )

#pulling in Total Capacity, Solar percentage, and Solar Photovoltaic. I'm replacing missing values with 0 because cell_year_capacity only contains the cells which had at least one generator, so if a cell is missing, it had no generator
cell_year_panel <- cell_year_panel %>%
  left_join(
    cell_year_capacity %>% select(x, y, year, Total_Capacity, Solar_Percentage, `Solar Photovoltaic`),
    by = c("x", "y", "year")
  ) %>%
  mutate(
    Total_Capacity = replace_na(Total_Capacity, 0),
    Solar_Percentage = replace_na(Solar_Percentage, 0),
    `Solar Photovoltaic` = replace_na(`Solar Photovoltaic`, 0)
  )

#Wait it doesn't make sense to have solar percentage at the cell level, removing it
cell_year_panel <- cell_year_panel %>%
  select(-Solar_Percentage)

#adding log transformed solar capacity value, with log1p which adds a constant (+1) to avoid undefined values for 0
cell_year_panel <- cell_year_panel %>%
  mutate(log_solar_pv = log1p(`Solar Photovoltaic`))

#Checking how many cells even have any solar PV
cell_year_panel %>%
  filter(group == 1, `Solar Photovoltaic` > 0) %>%
  nrow()

#creating trend line graphs
solar_trend <- cell_year_panel %>%
  group_by(year, group) %>%
  summarise(Total_Solar_PV = sum(`Solar Photovoltaic`), .groups = "drop")

ggplot(solar_trend, aes(x = year, y = Total_Solar_PV, color = as.factor(group))) +
  geom_line(size = 1.2) +
  geom_point(size = 2)

#Actually, it's probably more appropriate to do a weighted sum to properly take advantage of the CEM 
solar_trend_weighted <- cell_year_panel %>%
  group_by(year, group) %>%
  summarise(Total_Solar_PV = sum(`Solar Photovoltaic` * weight), .groups = "drop")

ggplot(solar_trend_weighted, aes(x = year, y = Total_Solar_PV, color = as.factor(group))) +
  geom_line(size = 1) +  
  geom_point(size = 1)

ggplot(solar_trend_weighted, aes(x = year, y = Total_Solar_PV, color = as.factor(group))) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Total Weighted Solar PV Capacity Over Time (by Group)",
    x = "Year",
    y = "Total Weighted Solar PV Capacity (MW)",
    color = "Group")

write_rds(cell_year_panel, "~/Wageningen/University/Thesis/Data/1. Modified Data/3. Panel Data/cell_year_panel.rds")

##Looking into which states contribute most to the increase in solar capacity of the counterfactual group
solar_trend_weighted_state <- cell_year_panel %>%
  group_by(year, State, group) %>%
  summarise(Total_Solar_PV = sum(`Solar Photovoltaic` * weight, na.rm = TRUE), .groups = "drop")

#Plotting 
ggplot(solar_trend_weighted_state %>% filter(group == 0), 
       aes(x = year, y = Total_Solar_PV, color = State)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Total Weighted Solar PV Capacity Over Time by State (Counterfactual Group)",
    x = "Year",
    y = "Total Weighted Solar PV Capacity (MW)",
    color = "State")

#Better plot with labels on plot
ggplot(solar_trend_weighted_state %>% filter(group == 0), 
       aes(x = year, y = Total_Solar_PV, color = State, group = State)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_text(data = solar_trend_weighted_state %>% 
              filter(group == 0, year == max(year)), 
            aes(label = State),
            vjust = -0.5, size = 4) +
  labs(
    title = "Total Weighted Solar PV Capacity Over Time by State (Counterfactual Group)",
    x = "Year",
    y = "Total Weighted Solar PV Capacity (MW)",
    color = "State") +
  theme(legend.position = "none")

#Sand Chart
ggplot(solar_trend_weighted_state %>% filter(group == 0), 
       aes(x = year, y = Total_Solar_PV, fill = State)) +
  geom_area(alpha = 0.8) +
  labs(
    title = "State Contributions to Solar PV Growth in Counterfactual Group",
    x = "Year",
    y = "Total Weighted Solar PV Capacity (MW)",
    fill = "State"
  ) +  theme(legend.position = "right")

#trying to create better sand chart
state_labels <- solar_trend_weighted_state %>%
  filter(group == 0) %>%
  group_by(State) %>%
  filter(year == max(year)) %>%
  ungroup()

#Sand Chart with labels
ggplot(solar_trend_weighted_state %>% filter(group == 0), 
       aes(x = year, y = Total_Solar_PV, fill = State)) +
  geom_area(alpha = 0.8) +
  geom_text(data = state_labels, 
            aes(x = year -1, y = Total_Solar_PV, label = State),
            position = position_stack(vjust = 0.5),
            size = 3.5, fontface = "bold", color = "white") +
  labs(
    title = "State Contributions to Solar PV Growth in Counterfactual Group",
    x = "Year",
    y = "Total Weighted Solar PV Capacity (MW)",
    fill = "State"
  )

#again
ggplot(solar_trend_weighted_state %>% filter(group == 0), 
       aes(x = year, y = Total_Solar_PV, fill = State)) +
  geom_area(alpha = 0.8) +
  scale_fill_viridis_d(option = "plasma") +
  geom_text(data = state_labels, 
            aes(x = year -1, y = Total_Solar_PV, label = State),
            position = position_stack(vjust = 0.5),
            size = 3.5, fontface = "bold", color = "white") +
  labs(
    title = "State Contributions to Solar PV Growth in Counterfactual Group",
    x = "Year",
    y = "Total Weighted Solar PV Capacity (MW)",
    fill = "State"
  )

#ok, maybe just best to plot the states with the most capacity
top_states_2023 <- solar_trend_weighted_state %>%
  filter(group == 0, year == 2023) %>%
  arrange(desc(Total_Solar_PV)) %>%
  slice_head(n = 10) %>%
  pull(State)

top_states_data <- solar_trend_weighted_state %>%
  filter(group == 0, State %in% top_states_2023)

state_labels <- top_states_data %>%
  filter(year == max(year)) %>%
  group_by(State) %>%
  summarise(year = max(year),
            Total_Solar_PV = sum(Total_Solar_PV), .groups = "drop") 

ggplot(top_states_data, aes(x = year, y = Total_Solar_PV, fill = State)) +
  geom_area(alpha = 0.8, color = "black", size = 0.3) +  # Add borders for clarity
  scale_fill_viridis_d(option = "plasma") +  # Better color contrast
    labs(
    title = "Top 10 States for Solar PV Growth in Counterfactual Group",
    x = "Year",
    y = "Total Weighted Solar PV Capacity (MW)",
    fill = "State"
  )

#checking to make sure the panel data is right because I thought that some southern states being included was weird
head(solar_trend_weighted_state)
summary(cell_year_panel$State)
master_raster_V2_reduced_maineCEM4_1 %>%
  filter(matched == T) %>%
  summary(State)
  #apparently some southern states did have cells that were matched though

#adding political lean variable (from "2-12-25 Creating dem_leans_complete")
dem_leans_complete <- readRDS("~/Wageningen/University/Thesis/Data/1. Modified Data/dem_leans_complete.rds")

cell_year_panel <- cell_year_panel %>%
  left_join(dem_leans_complete %>% 
              select(state_po, year, dem_lean), by = c("State" = "state_po", "year" = "year"))

write_rds(cell_year_panel, "~/Wageningen/University/Thesis/Data/1. Modified Data/3. Panel Data/cell_year_panel.rds")

#adding Land_usage_cat
cell_year_panel <- cell_year_panel %>%
  left_join(master_raster_V2_reduced_maineCEM4_1 %>%
              select(cell, Land_usage_cat), by = "cell")

#Visualizing forest land solar growth (weighted)
solar_forest_trend <- cell_year_panel %>%
  filter(Land_usage_cat == "Forest") %>%
  group_by(year, group) %>%
  summarise(Total_Solar_PV = sum(`Solar Photovoltaic` * weight, na.rm = TRUE), .groups = "drop")

ggplot(solar_forest_trend, aes(x = year, y = Total_Solar_PV, color = as.factor(group))) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Solar PV Capacity Over Time for Forest Land Type",
    x = "Year",
    y = "Total Weighted Solar PV Capacity (MW)",
    color = "Group"
  ) +
  scale_color_manual(values = c("blue", "red"), labels = c("Counterfactual (Group 0)", "Maine (Group 1)")) +
  theme_minimal()





##Pulling YCOM fundrenewables opinion data in
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
  mutate(year = as.integer(year))

#Pulling in abb. to full state names
statenames <- read_excel("~/Wageningen/University/Thesis/Data/50 states and abbreviations.xlsx")

#Adding full state names to YCOM data
YCOM_long <- YCOM_long %>%
  left_join(statenames, by = c("geoname" = "State"))

#renaming and removing columns
YCOM_long <- YCOM_long %>%
  select(year, YCOM_value, Abbreviation) %>%
  rename(State = Abbreviation)

#adding to cell_year_panel
cell_year_panel <- cell_year_panel %>%
  left_join(YCOM_long, by = c("year", "State"))

##creating a variable that is a measure of solar production capacity relative to the total production capacity of the group

#calculating total capacity of each group-year
group_total_capacity <- cell_year_panel %>%
  group_by(group, year) %>%
  summarise(Total_Capacity_Group = sum(Total_Capacity, na.rm = TRUE), .groups = "drop")

#calculating solar share relative to group
cell_year_panel <- cell_year_panel %>%
  left_join(group_total_capacity, by = c("group", "year")) %>%
  mutate(
    relative_solar_share = `Solar Photovoltaic` / Total_Capacity_Group
  )


#creating shock_lag and treated_post_lag 
cell_year_panel <- cell_year_panel %>%
  mutate(
    shock_lag = ifelse(year >= 2020, 1, 0),
    treated_post_lag = shock_lag * group
  )

write_rds(cell_year_panel, "~/Wageningen/University/Thesis/Data/1. Modified Data/3. Panel Data/cell_year_panel.rds")

nonzero_data <- cell_year_panel %>% filter(relative_solar_share > 0)


ggplot(nonzero_data, aes(x = relative_solar_share)) +
  geom_histogram(bins = 1000, fill = "blue", alpha = 0.7) +
  labs(title = "Distribution of Non-Zero Relative Solar Share", 
       x = "Relative Solar Share", 
       y = "Count") +
  theme_minimal()

ggplot(nonzero_data, aes(x = log(relative_solar_share))) +
  geom_histogram(bins = 50, fill = "blue", alpha = 0.7) +
  labs(title = "Log-Transformed Distribution of Non-Zero Relative Solar Share", 
       x = "Log(Relative Solar Share)", 
       y = "Count") +
  theme_minimal()

#creating solar_binary
cell_year_panel <- cell_year_panel %>%
  mutate(solar_binary = ifelse(`Solar Photovoltaic` > 0, 1, 0))
