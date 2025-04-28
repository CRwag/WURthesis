library(tidyverse)
library(readxl)
library(modelsummary)
library(cobalt)
library(stargazer)
library(xtable)
library(terra)




###Creating capacity graphs for Maine----
#Reading in gen data
Operatingandretired <- read_excel("~/Wageningen/University/Thesis/Data/EIA/EIA-860M/Operatingandretired.xlsx")

#Keeping necessary columns
Operatingandretired <- Operatingandretired %>%
  select(`Entity ID`, `Entity Name`, `Plant ID`, `Plant Name`, `Plant State`, `Generator ID`, `Nameplate Capacity (MW)`, Technology, `Operating Month`, `Operating Year`, `Retirement Month`, `Retirement Year`, Status)


# Filter the data for Maine generators with technology "Onshore Wind Turbine".
wind_maine <- Operatingandretired %>%
  filter(`Plant State` == "ME",
         Technology == "Onshore Wind Turbine")

# Create a sequence of years for the analysis.
years <- 2000:2023

# For each year, calculate the total installed capacity of generators 
# that are operating in that year.
capacity_by_year <- data.frame(Year = years) %>%
  rowwise() %>%
  mutate(Capacity = sum(
    wind_maine$`Nameplate Capacity (MW)`[
      wind_maine$`Operating Year` <= Year &
        (is.na(wind_maine$`Retirement Year`) | wind_maine$`Retirement Year` > Year)
    ],
    na.rm = TRUE
  )) %>%
  ungroup()

# Plot the cumulative capacity over time.
ggplot(capacity_by_year, aes(x = Year, y = Capacity)) +
  geom_line() +
  geom_point() +
  labs(title = "Cumulative Onshore Wind Capacity in Maine (2000-2023)",
       x = "Year",
       y = "Nameplate Capacity (MW)") +
  theme_minimal()



##Percentage Wind Graphs-----
# Define renewable energy technologies.
renewable_techs <- c("Conventional Hydroelectric", "Geothermal", 
                     "Offshore Wind Turbine", "Onshore Wind Turbine", 
                     "Municipal Solid Waste", "Other Waste Biomass", 
                     "Solar Photovoltaic", "Solar Thermal with Energy Storage", 
                     "Solar Thermal without Energy Storage", "Wood/Wood Waste Biomass")


# Subset the data to include only Maine.
maine_data <- Operatingandretired %>%
  filter(`Plant State` == "ME")

# For each year, calculate cumulative capacity for onshore wind, renewable, and total for Maine.
capacity_summary <- data.frame(Year = years) %>%
  rowwise() %>%
  mutate(
    # Onshore wind capacity (filtering directly using the string)
    wind_capacity = sum(maine_data$`Nameplate Capacity (MW)`[
      maine_data$`Operating Year` <= Year &
        (is.na(maine_data$`Retirement Year`) | maine_data$`Retirement Year` > Year) &
        maine_data$Technology == "Onshore Wind Turbine"
    ], na.rm = TRUE),
    # Renewable capacity (all renewable technologies as defined)
    renewable_capacity = sum(maine_data$`Nameplate Capacity (MW)`[
      maine_data$`Operating Year` <= Year &
        (is.na(maine_data$`Retirement Year`) | maine_data$`Retirement Year` > Year) &
        maine_data$Technology %in% renewable_techs
    ], na.rm = TRUE),
    # Total capacity (all generators)
    total_capacity = sum(maine_data$`Nameplate Capacity (MW)`[
      maine_data$`Operating Year` <= Year &
        (is.na(maine_data$`Retirement Year`) | maine_data$`Retirement Year` > Year)
    ], na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    wind_perc_renewable = (wind_capacity / renewable_capacity) * 100,
    wind_perc_total = (wind_capacity / total_capacity) * 100
  )

# Graph 1: Onshore wind capacity as a percentage of renewable capacity.
plot1 <- ggplot(capacity_summary, aes(x = Year, y = wind_perc_renewable)) +
  geom_line() +
  geom_point() +
  labs(title = "Onshore Wind Capacity as % of Renewable Capacity (Maine, 2000-2023)",
       x = "Year",
       y = "Onshore Wind Capacity (% of Renewables)") +
  theme_minimal()

# Graph 2: Onshore wind capacity as a percentage of total capacity.
plot2 <- ggplot(capacity_summary, aes(x = Year, y = wind_perc_total)) +
  geom_line() +
  geom_point() +
  labs(title = "Onshore Wind Capacity as % of Total Capacity (Maine, 2000-2023)",
       x = "Year",
       y = "Onshore Wind Capacity (% of Total)") +
  theme_minimal()

# Display the plots
print(plot1)
print(plot2)

#-----------------
### 1. Sandplot: Stacked Area Plot of Renewable Energy Capacity by Technology

# For each year, calculate cumulative capacity by renewable technology.
renewables_yearly <- bind_rows(lapply(years, function(y) {
  maine_data %>%
    filter(`Operating Year` <= y,
           (is.na(`Retirement Year`) | `Retirement Year` > y),
           Technology %in% renewable_techs) %>%
    group_by(Technology) %>%
    summarise(capacity = sum(`Nameplate Capacity (MW)`, na.rm = TRUE)) %>%
    mutate(Year = y)
}))

# Ensure every renewable technology is represented for every year (fill missing with 0).
renewables_yearly <- renewables_yearly %>%
  ungroup() %>%
  complete(Year, Technology, fill = list(capacity = 0))

# Create the stacked area plot.
sandplot <- ggplot(renewables_yearly, aes(x = Year, y = capacity, fill = Technology)) +
  geom_area() +
  labs(title = "Cumulative Renewable Energy Capacity by Technology (Maine, 2000-2023)",
       x = "Year",
       y = "Cumulative Nameplate Capacity (MW)") +
  theme_minimal()

sandplot <- ggplot(renewables_yearly, aes(x = Year, y = capacity, fill = Technology)) +
  geom_area() +
  scale_fill_manual(values = c(
    "Conventional Hydroelectric" = "#023047",
    "Geothermal" = "#d95f02",
    "Offshore Wind Turbine" = "#7570b3",
    "Onshore Wind Turbine" = "#2a9d8f",
    "Municipal Solid Waste" = "#606c38",
    "Other Waste Biomass" = "#e6ab02",
    "Solar Photovoltaic" = "#ffb703",
    "Solar Thermal with Energy Storage" = "#666666",
    "Solar Thermal without Energy Storage" = "#a6cee3",
    "Wood/Wood Waste Biomass" = "#283618"
  )) +
  labs(title = "Cumulative Renewable Energy Capacity by Technology (Maine, 2000-2023)",
       x = "Year",
       y = "Cumulative Nameplate Capacity (MW)") +
  theme_minimal()



### 2. Line Graph: Percentage of Nameplate Capacity from Renewables vs. Non-Renewables

# For each year, calculate cumulative capacity for renewables and non-renewables.
capacity_total <- data.frame(Year = years) %>%
  rowwise() %>%
  mutate(
    renewable_capacity = sum(maine_data$`Nameplate Capacity (MW)`[
      maine_data$`Operating Year` <= Year &
        (is.na(maine_data$`Retirement Year`) | maine_data$`Retirement Year` > Year) &
        maine_data$Technology %in% renewable_techs
    ], na.rm = TRUE),
    nonrenewable_capacity = sum(maine_data$`Nameplate Capacity (MW)`[
      maine_data$`Operating Year` <= Year &
        (is.na(maine_data$`Retirement Year`) | maine_data$`Retirement Year` > Year) &
        !(maine_data$Technology %in% renewable_techs)
    ], na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(total_capacity = renewable_capacity + nonrenewable_capacity,
         perc_renewable = (renewable_capacity / total_capacity) * 100,
         perc_nonrenewable = (nonrenewable_capacity / total_capacity) * 100)

# Reshape the data for plotting
capacity_pct <- capacity_total %>%
  select(Year, perc_renewable, perc_nonrenewable) %>%
  pivot_longer(cols = c(perc_renewable, perc_nonrenewable),
               names_to = "Source", values_to = "Percentage") %>%
  mutate(Source = recode(Source,
                         "perc_renewable" = "Renewables",
                         "perc_nonrenewable" = "Non-Renewables"))

# Create the line graph.
lineplot <- ggplot(capacity_pct, aes(x = Year, y = Percentage, color = Source)) +
  geom_line() +
  geom_point() +
  labs(title = "Percentage of Nameplate Capacity: Renewables vs. Non-Renewables (Maine, 2000-2023)",
       x = "Year",
       y = "Percentage (%)",
       color = "Source") +
  theme_minimal()

# Display the plots.
print(sandplot)
print(lineplot)


###3. Technology Shares Line Graph-----------
# For each year, calculate cumulative capacity by technology and compute its share of total capacity.
tech_yearly <- bind_rows(lapply(years, function(y) {
  # Active generators for the given year
  active_data <- maine_data %>%
    filter(`Operating Year` <= y,
           is.na(`Retirement Year`) | `Retirement Year` > y)
  
  # Total cumulative capacity for the year
  total_capacity <- sum(active_data$`Nameplate Capacity (MW)`, na.rm = TRUE)
  
  # Cumulative capacity per technology
  tech_cap <- active_data %>%
    group_by(Technology) %>%
    summarise(capacity = sum(`Nameplate Capacity (MW)`, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(Year = y,
           total_capacity = total_capacity,
           share = ifelse(total_capacity > 0, (capacity / total_capacity) * 100, 0))
  
  tech_cap
}))

# Ensure every technology is represented for each year (fill missing combinations with 0).
tech_yearly <- tech_yearly %>%
  complete(Year, Technology, fill = list(capacity = 0, share = 0, total_capacity = 0))

# Plot the share of nameplate capacity (%) by technology over the years.
share_plot_all <- ggplot(tech_yearly, aes(x = Year, y = share, color = Technology)) +
  geom_line() +
  geom_point() +
  labs(title = "Share of Nameplate Capacity by Technology in Maine (2000-2023)",
       x = "Year",
       y = "Share of Nameplate Capacity (%)") +
  theme_minimal()

tech_yearly <- bind_rows(lapply(years, function(y) {
  
  # Subset to active generators in year y
  active_data <- maine_data %>%
    filter(`Operating Year` <= y,
           is.na(`Retirement Year`) | `Retirement Year` > y)
  
  # Calculate total capacity for that year
  total_capacity <- sum(active_data$`Nameplate Capacity (MW)`, na.rm = TRUE)
  
  # Create a "TechGroup" that lumps non-renewables together
  active_data <- active_data %>%
    mutate(TechGroup = if_else(Technology %in% renewable_techs, 
                               Technology, 
                               "Non-renewable"))
  
  # Sum capacity by TechGroup
  tech_cap <- active_data %>%
    group_by(TechGroup) %>%
    summarise(capacity = sum(`Nameplate Capacity (MW)`, na.rm = TRUE), .groups = "drop") %>%
    mutate(
      Year = y,
      total_capacity = total_capacity,
      share = ifelse(total_capacity > 0, (capacity / total_capacity) * 100, 0)
    )
  
  tech_cap
}))

# We want each TechGroup to appear every year, even if it's zero.
all_tech_groups <- c(renewable_techs, "Non-renewable")

tech_yearly <- tech_yearly %>%
  complete(Year, TechGroup = all_tech_groups, 
           fill = list(capacity = 0, total_capacity = 0, share = 0))

# Plot each TechGroup's share of total capacity over time, coloring by TechGroup.
ggplot(tech_yearly, aes(x = Year, y = share, color = TechGroup)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Share of Nameplate Capacity by Technology Group in Maine (2000-2023)",
    x = "Year",
    y = "Share of Total Nameplate Capacity (%)",
    color = "Technology Group"
  ) +
  theme_minimal()





### EIA 860M Power Generators Map-----------------
september_generator2024 <- read_excel("~/Wageningen/University/Thesis/Data/EIA/EIA-860M/september_generator2024.xlsx", range = "A3:AG26410")
names(september_generator2024)

gens_vect <- vect(september_generator2024, geom = c("Longitude", "Latitude"), crs = "EPSG:4326")
gens_vect

gens_vect <- project(gens_vect, "EPSG:5070")
gens_vect <- crop(gens_vect, ext(landval))

plot(gens_vect)

solar_points <- gens_vect[gens_vect$Technology == "Solar Photovoltaic", ]
plot(solar_points, col = "yellow", main = "Solar Photovoltaic Generators")

statelines <- vect("~/Wageningen/University/Thesis/Data/1. Modified Data/states_vect/states_vect.shp")
plot(statelines)
plot(solar_points, 
     col = "yellow",
     main = "Solar Photovoltaic Generators", 
     add = T)


#Final PV Map
plot(statelines, 
     col = "#dce3c7",
     main = "Solar Photovoltaic Generators",)
plot(
  solar_points,
  cex = sqrt(solar_points$`Nameplate Capacity (MW)`)/10,
  pch = 16,
  col = "yellow",
  add = T
)



###Land Use Map----------
master_raster_V2_reduced <- readRDS("~/Wageningen/University/Thesis/Data/1. Modified Data/master_raster_V2_reduced.rds")
names(master_raster_V2_reduced)
levels(master_raster_V2_reduced$Land_usage_cat)

Landuse_cat <- master_raster_V2_reduced %>%
  select(cell, x, y, Land_usage_cat)

master_raster_df <- readRDS("~/Wageningen/University/Thesis/Data/1. Modified Data/master_raster_df.rds")

master_raster_df <- master_raster_df %>%
  select(cell, x, y)

master_raster_df <- master_raster_df %>%
  left_join(Landuse_cat, by = "cell")

master_raster_df <- master_raster_df %>%
  select(x.x, y.x, Land_usage_cat) %>%
  rename(x = x.x) %>%
  rename(y = y.x)

master_raster_df$Land_usage_cat <- as.integer(master_raster_df$Land_usage_cat)

landcat_rast <- rast(master_raster_df, type = "xyz", crs = crs(landval))

colors <- c("#C2B280",  # Barren
            "#404040",  # Developed/High Intensity
            "#B0B0B0",  # Developed/Low Intensity
            "#808080",  # Developed/Med Intensity
            "#F5F5DC",  # Developed/Open Space
            "#EEDC82",  # Farmland
            "#228B22",  # Forest
            "#9ACD32",  # Grassland/Pasture
            "#1E90FF",  # Open Water
            "#F0F8FF",  # Perennial Ice/Snow
            "#8FBC8F",  # Shrubland
            "#20B2AA")  # Wetlands

coltab(landcat_rast) <- data.frame(value = 1:12, color = colors)

plot(landcat_rast, col = colors, legend = TRUE)

landcats <- c("Barren", "Dev/High", "Dev/Low", "Dev/Med", "Dev/Open",
              "Farmland", "Forest", "Grass/Pasture", "Inland Water",
              "Ice/Snow", "Shrubland", "Wetlands")

plot(landcat_rast, col = colors, legend = FALSE, main = "Land Usage")
legend(
  "bottomleft",                
  legend = landcats,
  fill = colors,
  title = "Land Usage Types",
  inset = c(0,0.1),
  ncol = 3,
  cex = 0.8,
  bty = "n"
)
plot(statelines, add = T)




  


### Event Study Models Output Table ------------

library(fixest)

etable(
  twfe_event_study7, twfe_event_study8,
  tex = TRUE,
  title = "Event Study Results",
  dict = c(
    "year::2010:group_factor::1" = "2010",
    "year::2011:group_factor::1" = "2011",
    "year::2012:group_factor::1" = "2012",
    "year::2013:group_factor::1" = "2013",
    "year::2014:group_factor::1" = "2014",
    "year::2015:group_factor::1" = "2015",
    "year::2016:group_factor::1" = "2016",
    "year::2017:group_factor::1" = "2017",
    "year::2018:group_factor::1" = "2018",
    "year::2020:group_factor::1" = "2020",
    "year::2021:group_factor::1" = "2021",
    "year::2022:group_factor::1" = "2022",
    "year::2023:group_factor::1" = "2023"
  )
)






### Love Plot OLD (for v6)-----
balance <- bal.tab(MaineCEMv6, data = master_raster_V2_reduced_filtered_v2, un = TRUE)

bal <- bal.tab(MaineCEMv6, 
               data = master_raster_V2_reduced_filtered_v2, 
               un = TRUE,
               continuous = "std",
               binary = "std")

love.plot(balance, 
          stats = "mean.diffs", 
          abs = TRUE, 
          threshold = 0.1, 
          var.order = "unadjusted")

love.plot(balance, 
          stats = "mean.diffs", 
          abs = TRUE, 
          threshold = 0.2, 
          var.order = "unadjusted"
          )

love.plot(bal, 
          stats = "mean.diffs", 
          abs = TRUE, 
          threshold = 0.2, 
          var.order = "unadjusted", 
          stars = TRUE)




### Summary Statistics Table for states which did not have RPS policy changes 2015-2020 ----------

sumtbldata_filtered <- master_raster_V2_reduced_filtered_v2 %>%
  select(Solar_binary, Land_usage_cat, State, aspect_cardinal, slope, HVline_dist, substation_dist, nsrdb3_ghi, landval_ln)

## Table for continuous variables in pre-matching dataset
stargazer(sumtbldata_filtered,
          type = "latex",
          title = "Summary Statistics",
          summary.stat = c("n", "mean", "sd", "min", "max"),
          digits = 2
          )

# Create frequency table for the categorical variable 'aspect_cardinal'
aspect_freq <- as.data.frame(table(master_raster_V2_reduced_filtered_v2$aspect_cardinal))
colnames(aspect_freq) <- c("Aspect", "Frequency")

aspect_freq <- aspect_freq %>%
  mutate(Percentage = 100 * Frequency / sum(Frequency))

xtable_aspect <- xtable(aspect_freq,
                        caption = "Frequency Table for Aspect Cardinal",
                        label = "tab:aspect_freq")
print(xtable_aspect,
      type = "latex",
      include.rownames = FALSE,
      digits = c(0, 0, 0, 2))


# Create frequency table for the categorical variable 'Land_usage_cat'
land_usage_freq <- as.data.frame(table(master_raster_V2_reduced_filtered_v2$Land_usage_cat))
colnames(land_usage_freq) <- c("Land Usage Category", "Frequency")

land_usage_freq <- land_usage_freq %>%
  mutate(Percentage = 100 * Frequency / sum(Frequency))

xtable_land_usage <- xtable(land_usage_freq,
                            caption = "Frequency Table for Land Usage Category",
                            label = "tab:land_usage_freq")
print(xtable_land_usage,
      type = "latex",
      include.rownames = FALSE,
      digits = c(0, 0, 0, 2))


### Summary Statistics Table for full dataset ----------

# Subset the desired columns from the mainematch dataset
sumtbldata_mainematch <- master_raster_V2_reduced_mainematch %>%
  select(Solar_binary, Land_usage_cat, State, aspect_cardinal, slope, HVline_dist, substation_dist, nsrdb3_ghi, landval_ln)

## Table for continuous variables in the mainematch dataset
stargazer(sumtbldata_mainematch,
          type = "latex",
          title = "Summary Statistics for Mainematch Data",
          summary.stat = c("n", "mean", "sd", "min", "max"),
          digits = 2)

# Create frequency table for the categorical variable 'aspect_cardinal'
aspect_freq2 <- as.data.frame(table(master_raster_V2_reduced_mainematch$aspect_cardinal))
colnames(aspect_freq2) <- c("Aspect", "Frequency")

# Calculate percentages for aspect_cardinal
aspect_freq2 <- aspect_freq2 %>%
  mutate(Percentage = 100 * Frequency / sum(Frequency))

# Convert the frequency table for aspect_cardinal to LaTeX using xtable
xtable_aspect2 <- xtable(aspect_freq,
                        caption = "Frequency Table for Aspect Cardinal (Mainematch)",
                        label = "tab:aspect_freq_mainematch")
print(xtable_aspect2,
      type = "latex",
      include.rownames = FALSE,
      digits = c(0, 0, 0, 2))

# Create frequency table for 'Land_usage_cat'
land_usage_freq2 <- as.data.frame(table(master_raster_V2_reduced_mainematch$Land_usage_cat))
colnames(land_usage_freq2) <- c("Land_Usage_Category", "Frequency")

# Now calculate percentages
land_usage_freq2 <- land_usage_freq2 %>%
  mutate(Percentage = 100 * Frequency / sum(Frequency))

# Convert the frequency table for Land_usage_cat to LaTeX using xtable
xtable_land_usage2 <- xtable(land_usage_freq2,
                            caption = "Frequency Table for Land Usage Category (Mainematch)",
                            label = "tab:land_usage_freq_mainematch")
print(xtable_land_usage2,
      type = "latex",
      include.rownames = FALSE,
      digits = c(0, 0, 0, 2))


### Summary Statistics Table after match (MaineCEMv6) ----------

plot(landval)
plot(borders, add = T)
text(centroids(borders), labels = borders$STUSPS, cex = 0.8)


### CEM Variables Table---------
CEM_Variables_Table <- read_excel("~/Wageningen/University/Thesis/Data/1. Modified Data/1. Plots/FINAL/CEM Variables Table.xlsx")
CEMvars_table <- xtable(CEM_Variables_Table, caption = "Variables Used in CEM Matching")
print(CEMvars_table, type = "latex", include.rownames = FALSE)


### Mean by group table--------
master_raster_V2_reduced <- readRDS("~/Wageningen/University/Thesis/Data/1. Modified Data/master_raster_V2_reduced.rds")
state_region <- read_excel("~/Wageningen/University/Thesis/Data/1. Modified Data/Solar_Wind_LCOE_estimates.xlsx", sheet = "State-Region")
X50_states_and_abbreviations <- read_excel("~/Wageningen/University/Thesis/Data/50 states and abbreviations.xlsx")
Solar_Wind_LCOE_estimates <- read_excel("~/Wageningen/University/Thesis/Data/1. Modified Data/Solar_Wind_LCOE_estimates.xlsx", sheet = "Import")
expanded_gens2 <- readRDS("~/Wageningen/University/Thesis/Data/1. Modified Data/expanded_gens2.rds")
dem_leans_complete <- readRDS("~/Wageningen/University/Thesis/Data/1. Modified Data/dem_leans_complete.rds")

##Prep for merging all vars
Solar_Wind_LCOE_estimates <- Solar_Wind_LCOE_estimates %>%
  filter(Year == 2019) %>%
  mutate(LCOE_difference = Solar_LCOE - Wind_LCOE)

Solar_Wind_LCOE_estimates_with_state <- Solar_Wind_LCOE_estimates %>%
  left_join(state_region, by = "Region")


expanded_gens2 <- expanded_gens2 %>%
  select(`Entity ID`, `Plant ID`, `Plant State`, ID, cell, x, y, `Solar Photovoltaic`, year) %>%
  filter(year == 2019, `Solar Photovoltaic` > 0)

cell_solar_capacities <- expanded_gens2 %>%
  group_by(cell) %>%
  summarise(Total_solar_capacity = sum(`Solar Photovoltaic`),
            x = first(x),
            y = first(y),
            .groups = "drop")

YCOM7_publicdata <- read_excel("~/Wageningen/University/Thesis/Data/Yale Climate Opinions/YCOM7_publicdata.xlsx", sheet = "YCOM_2010_2023")

YCOM7_publicdata <- YCOM7_publicdata %>%
  select(geotype, geoname, varname, x2019) %>%
  filter(geotype == "state", varname == "fundrenewables")

YCOM7_publicdata <- YCOM7_publicdata %>%
  left_join(X50_states_and_abbreviations, by = c("geoname" = "State")) %>%
  rename(State = Abbreviation)

dem_leans_2019 <- dem_leans_complete %>%
  filter(year == 2019)

dem_leans_2019 <- dem_leans_2019 %>%
  rename("State" = "state_po")



##Merging vars
master_raster_V2_reduced <- master_raster_V2_reduced %>%
  left_join(cell_solar_capacities %>% 
              select(x, y, Total_solar_capacity),
            by = c("x", "y")) %>%
  mutate(Total_solar_capacity = coalesce(Total_solar_capacity, 0))

master_raster_V2_reduced <- master_raster_V2_reduced %>%
  left_join(Solar_Wind_LCOE_estimates_with_state %>%
              select(LCOE_difference, State), by = "State")

master_raster_V2_reduced <- master_raster_V2_reduced %>%
  left_join(YCOM7_publicdata %>%
              select(x2019, State), by = "State")

master_raster_V2_reduced <- master_raster_V2_reduced %>%
  rename("YCOM" = "x2019")

master_raster_V2_reduced <- master_raster_V2_reduced %>%
  left_join(dem_leans_2019 %>%
              select(State, dem_lean), by = "State")

##creating actual table

master_raster_V2_reduced <- master_raster_V2_reduced %>%
  mutate(Maine_label = if_else(Maine == 1, "Maine", "Other States"))

summary_stats <- master_raster_V2_reduced %>%
  group_by(Maine_label) %>%
  summarise(
    Total_solar_capacity_mean = mean(Total_solar_capacity, na.rm = TRUE),
    Total_solar_capacity_sd   = sd(Total_solar_capacity, na.rm = TRUE),
    
    slope_mean = mean(slope, na.rm = TRUE),
    slope_sd   = sd(slope, na.rm = TRUE),
    
    HVline_dist_mean = mean(HVline_dist, na.rm = TRUE),
    HVline_dist_sd   = sd(HVline_dist, na.rm = TRUE),
    
    substation_dist_mean = mean(substation_dist, na.rm = TRUE),
    substation_dist_sd   = sd(substation_dist, na.rm = TRUE),
    
    nsrdb3_ghi_mean = mean(nsrdb3_ghi, na.rm = TRUE),
    nsrdb3_ghi_sd   = sd(nsrdb3_ghi, na.rm = TRUE),
    
    landval_ln_mean = mean(landval_ln, na.rm = TRUE),
    landval_ln_sd   = sd(landval_ln, na.rm = TRUE),
    
    LCOE_difference_mean = mean(LCOE_difference, na.rm = TRUE),
    LCOE_difference_sd   = sd(LCOE_difference, na.rm = TRUE),
    
    YCOM_mean = mean(YCOM, na.rm = TRUE),
    YCOM_sd   = sd(YCOM, na.rm = TRUE),
    
    dem_lean_mean = mean(dem_lean, na.rm = TRUE),
    dem_lean_sd   = sd(dem_lean, na.rm = TRUE)
  ) %>%
  ungroup()

# reshape
summary_long <- summary_stats %>%
  pivot_longer(cols = -Maine_label, names_to = "Metric", values_to = "Value") %>%
  separate(Metric, into = c("Variable", "Statistic"), sep = "_(?=[^_]+$)") %>%
  pivot_wider(names_from = c(Maine_label, Statistic), values_from = Value) %>%
  mutate(
    Difference = `Maine_mean` - `Other States_mean`
  ) %>%
  select(Variable, `Maine_mean`, `Maine_sd`, `Other States_mean`, `Other States_sd`, Difference)

#print table
print(xtable(summary_long,
             caption = "Summary Statistics: Means, Standard Deviations, and Differences (Maine – Other States)"),
      type = "latex",
      include.rownames = FALSE,
      digits = 3)


### Land Usage comparisons ----------
table_out <- master_raster_V2_reduced %>%
  mutate(Location = ifelse(State == "ME", "Maine", "Rest of US")) %>%
  group_by(Location, Land_usage_cat) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Location) %>%
  mutate(percentage = 100 * n / sum(n)) %>%
  ungroup() %>%
  select(Land_usage_cat, Location, percentage) %>%
  pivot_wider(
    names_from = Location,
    values_from = percentage,
    values_fill = 0
  ) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
  rename(
    "Land Usage Category" = Land_usage_cat,
    "Maine (%)" = Maine,
    "Rest of US (%)" = `Rest of US`
  )

landxtab <- xtable(table_out, 
               caption = "Land Usage Category Percentages in Maine vs. Rest of US", 
               label = "tab:land_usage_percent")

# Print the LaTeX table code
print(landxtab, include.rownames = FALSE, caption.placement = "top")


### Aspect comparison table -------------
table_out <- master_raster_V2_reduced %>%
  mutate(Location = ifelse(State == "ME", "Maine", "Rest of US")) %>%
  group_by(Location, aspect_cardinal) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Location) %>%
  mutate(percentage = 100 * n / sum(n)) %>%
  ungroup() %>%
  select(aspect_cardinal, Location, percentage) %>%
  pivot_wider(
    names_from = Location,
    values_from = percentage,
    values_fill = list(percentage = 0)
  ) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
  rename(
    "Aspect (Cardinal)" = aspect_cardinal,
    "Maine (%)" = Maine,
    "Rest of US (%)" = `Rest of US`
  )

aspect_xtab <- xtable(
  table_out, 
  caption = "Aspect (Cardinal) Percentages in Maine vs. Rest of US", 
  label = "tab:aspect_cardinal_percent"
)

print(aspect_xtab, include.rownames = FALSE, caption.placement = "top")

###YCOM Map--------------
stateborders <- vect("~/Wageningen/University/Thesis/Data/1. Modified Data/states_vect/states_vect.shp")

YCOM_df <- master_raster_V2_reduced %>%
  select(x, y, x2019)

YCOM_rast <- rast(YCOM_df, type = "xyz", crs = crs(landval))

YCOM_rast
plot(YCOM_rast)
plot(stateborders, add = T)


###Capacity trends by state--------

tech_columns <- c("Conventional Hydroelectric",
                  "Natural Gas Steam Turbine",
                  "Conventional Steam Coal",
                  "Natural Gas Fired Combined Cycle",
                  "Natural Gas Fired Combustion Turbine",
                  "Petroleum Liquids",
                  "Nuclear",
                  "Hydroelectric Pumped Storage",
                  "Natural Gas Internal Combustion Engine",
                  "Batteries",
                  "Solar Photovoltaic",
                  "Geothermal",
                  "Onshore Wind Turbine",
                  "Wood/Wood Waste Biomass",
                  "Coal Integrated Gasification Combined Cycle",
                  "Other Gases",
                  "Petroleum Coke",
                  "Municipal Solid Waste",
                  "Landfill Gas",
                  "Natural Gas with Compressed Air Storage",
                  "All Other",
                  "Other Waste Biomass",
                  "Solar Thermal without Energy Storage",
                  "Other Natural Gas",
                  "Solar Thermal with Energy Storage",
                  "Flywheels",
                  "Offshore Wind Turbine")

state_year_capacity <- expanded_gens2 %>%
  group_by(`Plant State`, year) %>%
  summarise(across(all_of(tech_columns), sum, na.rm = TRUE), .groups = "drop")

renewable_tech <- c("Conventional Hydroelectric",
                    "Hydroelectric Pumped Storage",
                    "Solar Photovoltaic",
                    "Geothermal",
                    "Onshore Wind Turbine",
                    "Wood/Wood Waste Biomass",
                    "Solar Thermal without Energy Storage",
                    "Solar Thermal with Energy Storage",
                    "Offshore Wind Turbine",
                    "Other Waste Biomass",
                    "Municipal Solid Waste",
                    "Landfill Gas")

nonrenewable_tech <- c("Natural Gas Steam Turbine",
                       "Conventional Steam Coal",
                       "Natural Gas Fired Combined Cycle",
                       "Natural Gas Fired Combustion Turbine",
                       "Petroleum Liquids",
                       "Nuclear",
                       "Natural Gas Internal Combustion Engine",
                       "Coal Integrated Gasification Combined Cycle",
                       "Other Gases",
                       "Petroleum Coke",
                       "Natural Gas with Compressed Air Storage",
                       "Other Natural Gas")

# Aggregate to the national level by summing over all generators for each year
national_totals <- expanded_gens2 %>%
  group_by(year) %>%
  summarise(across(all_of(tech_columns), \(x) sum(x, na.rm = TRUE)), .groups = "drop") %>%
  mutate(
    renewable_capacity = rowSums(across(all_of(renewable_tech)), na.rm = TRUE),
    nonrenewable_capacity = rowSums(across(all_of(nonrenewable_tech)), na.rm = TRUE)
  )

# Reshape the data into a long format for plotting
national_totals_long <- national_totals %>%
  select(year, renewable_capacity, nonrenewable_capacity) %>%
  pivot_longer(cols = c(renewable_capacity, nonrenewable_capacity),
               names_to = "type", values_to = "capacity")

# Create the line graph using ggplot2
ggplot(national_totals_long, aes(x = year, y = capacity, color = type)) +
  geom_line(size = 1) +
  labs(title = "National Nameplate Capacity over Time",
       x = "Year",
       y = "Nameplate Capacity",
       color = "Energy Type") +
  theme_minimal()

national_totals_long_2000 <- national_totals_long %>%
  filter(year >= 2000)

# Create the line graph using ggplot2 with the filtered data
ggplot(national_totals_long_2000, aes(x = year, y = capacity, color = type)) +
  geom_line(linewidth = 1) +
  labs(title = "National Nameplate Capacity over Time (2000 Onwards)",
       x = "Year",
       y = "Nameplate Capacity (MW)",
       color = "Energy Type") +
  theme_minimal()

ggplot(national_totals_long_2000, aes(x = year, y = capacity, color = type)) +
  geom_line(linewidth = 1) +
  labs(
    title = "",
    x = "Year",
    y = "Nameplate Capacity",
    color = "Energy Type"
  ) +
  scale_color_discrete(
    labels = c("nonrenewable_capacity" = "Non-Renewables",
               "renewable_capacity" = "Renewables")
  ) +
  theme_minimal()


### Renewables % in Maine vs Every other state---------

state_year <- expanded_gens2 %>%
  filter(year >= 2000) %>%
  group_by(`Plant State`, year) %>%
  summarise(
    across(all_of(renewable_tech), ~ sum(.x, na.rm = TRUE), .names = "ren_{.col}"),
    across(all_of(nonrenewable_tech), ~ sum(.x, na.rm = TRUE), .names = "non_{.col}"),
    .groups = "drop"
  ) %>%
  # Sum across all renewable and nonrenewable columns to get total capacity by state and year
  mutate(
    total_renewable = rowSums(select(., starts_with("ren_")), na.rm = TRUE),
    total_nonrenewable = rowSums(select(., starts_with("non_")), na.rm = TRUE),
    total_capacity = total_renewable + total_nonrenewable,
    renewable_pct = (total_renewable / total_capacity) * 100
  ) %>%
  select(`Plant State`, year, renewable_pct)

# Step 2: Create two datasets: one for Maine and one for all other states (averaged)
maine_data <- state_year %>%
  filter(`Plant State` == "ME") %>%
  select(year, renewable_pct) %>%
  mutate(region = "Maine")

non_maine_data <- state_year %>%
  filter(`Plant State` != "ME") %>%
  group_by(year) %>%
  summarise(avg_renewable_pct = mean(renewable_pct, na.rm = TRUE), .groups = "drop") %>%
  mutate(region = "Other States") %>%
  rename(renewable_pct = avg_renewable_pct)

# Step 3: Combine the two datasets for plotting
renewable_comparison <- bind_rows(maine_data, non_maine_data)

# Step 4: Create the line graph using ggplot2 - Percentage of Energy from Renewables: Maine vs. Other States (2000 Onwards)
ggplot(renewable_comparison, aes(x = year, y = renewable_pct, color = region)) +
  geom_line(linewidth = 1) +
  labs(
    title = "",
    x = "Year",
    y = "Renewable Energy Percentage",
    color = "Region"
  ) +
  theme_minimal()

### Utility Scale Solar PV % in Maine vs Every other state 2000-2024---------

state_year_filtered <- state_year_capacity %>%
  filter(year >= 2000)

# 2. Compute total capacity and calculate the solar percentage.
# We'll assume that columns 3 to 29 are all technology capacity columns.
state_year_filtered <- state_year_filtered %>%
  mutate(
    total_capacity = rowSums(across(-c(`Plant State`, year)), na.rm = TRUE),
    solar_pct = (`Solar Photovoltaic` / total_capacity) * 100
  )

# 3. Create separate datasets:
#    - For Maine: keep the solar percentage for Maine.
#    - For all other states: compute the average solar percentage by year.
maine_solar <- state_year_filtered %>%
  filter(`Plant State` == "ME") %>%
  select(year, solar_pct) %>%
  mutate(region = "Maine")

other_solar <- state_year_filtered %>%
  filter(`Plant State` != "ME") %>%
  group_by(year) %>%
  summarise(avg_solar_pct = mean(solar_pct, na.rm = TRUE), .groups = "drop") %>%
  rename(solar_pct = avg_solar_pct) %>%
  mutate(region = "Other States")

# 4. Combine both datasets for plotting
solar_comparison <- bind_rows(maine_solar, other_solar)

# 5. Create the line graph using ggplot2.
ggplot(solar_comparison, aes(x = year, y = solar_pct, color = region)) +
  geom_line(linewidth = 1) +
  labs(
    title = "",
    x = "Year",
    y = "Solar Photovoltaic Percentage (%)",
    color = "Region"
  ) +
  theme_minimal()


### Political trends Maine vs all others--------
`Election_data_by_state_2008-2024` <- readRDS("~/Wageningen/University/Thesis/Data/1. Modified Data/Election_data_by_state_2008-2024.rds")

# Create Maine subset
maine_elec <- `Election_data_by_state_2008-2024` %>%
  filter(state == "Maine") %>%
  select(year, dem_lean) %>%
  mutate(region = "Maine")

# Create dataset for all other states: average dem_lean by year
other_elec <- `Election_data_by_state_2008-2024` %>%
  filter(state != "Maine") %>%
  group_by(year) %>%
  summarise(avg_dem_lean = mean(dem_lean, na.rm = TRUE), .groups = "drop") %>%
  rename(dem_lean = avg_dem_lean) %>%
  mutate(region = "Other States")

# Combine the two datasets for plotting
elec_plot_data <- bind_rows(maine_elec, other_elec)

# Create the line graph - Democratic Leaning (dem_lean) by Year: Maine vs. Other States
ggplot(elec_plot_data, aes(x = year, y = dem_lean, color = region)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
  scale_color_manual(
    values = c("Maine" = "black", "All other States" = "forestgreen")
  ) +
  labs(
    title = "",
    x = "Year",
    y = "dem_lean",
    color = "Region"
  ) +
  theme_minimal()


### Matched dataset maine vs non-maine solar capacity trends-------
cell_year_panel_v3 <- readRDS("~/Wageningen/University/Thesis/Data/1. Modified Data/3. Panel Data/cell_year_panel_v3.rds")

# Summarize the data: calculate the weighted sum of Solar_Photovoltaic for each year and each group (0 and 1)
group_year_sum <- cell_year_panel_v3 %>%
  group_by(year, group) %>%
  summarise(total_solar = sum(Solar_Photovoltaic * weight, na.rm = TRUE), .groups = "drop")

# Plot the data using ggplot2
ggplot(group_year_sum, aes(x = year, y = total_solar, color = as.factor(group))) +
  geom_line(linewidth = 1) +
  labs(
    title = "Weighted Total Solar Photovoltaic Capacity by Year",
    x = "Year",
    y = "Weighted Total Solar Photovoltaic Capacity",
    color = "Group"
  ) +
  scale_color_manual(
    values = c("0" = "blue", "1" = "forestgreen"),
    labels = c("0" = "Counterfactual", "1" = "Maine")
  ) +
  theme_minimal()


#TRY AGAIN

solar_summary <- cell_year_panel_v3 %>%
  group_by(year, group_factor) %>%
  summarize(total_solar = sum(Solar_Photovoltaic, na.rm = TRUE))

ggplot(solar_summary, aes(x = year, y = total_solar, color = group_factor, group = group_factor)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Trend of Solar Photovoltaic by Group Factor",
    x = "Year",
    y = "Sum of Solar Photovoltaic",
    color = "Group Factor"
  ) +
  theme_minimal()

solar_summary_weighted <- cell_year_panel_v3 %>%
  group_by(year, group_factor) %>%
  summarize(total_weighted_solar = sum(Solar_Photovoltaic * weight, na.rm = TRUE))

ggplot(solar_summary_weighted, aes(x = year, y = total_weighted_solar, color = group_factor, group = group_factor)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Trend of Weighted Solar Photovoltaic by Group Factor",
    x = "Year",
    y = "Weighted Sum of Solar Photovoltaic",
    color = "Group Factor"
  ) +
  theme_minimal()

## For cell_year_panel_v4

cell_year_panel_v4 <- readRDS("~/Wageningen/University/Thesis/Data/1. Modified Data/3. Panel Data/cell_year_panel_v4.rds")

group_year_sum <- cell_year_panel_v4 %>%
  group_by(year, group) %>%
  summarise(total_solar = sum(Solar_Photovoltaic * weight), .groups = "drop")

ggplot(group_year_sum, 
       aes(x = year, y = total_solar, color = factor(group))) +
  geom_line(linewidth = 1) +
  scale_color_discrete(
    name   = "",  # or “Group” if you want a legend title
    labels = c("Counterfactual", "Treated (")
  ) +
  labs(
    x = "Year",
    y = "Weighted Total Solar PV Nameplate Capacity (MW)"
  ) +
  theme_minimal()


### Love Plot (for v7)-----
bal <- bal.tab(MaineCEMv7, 
               data = master_raster_V2_reduced_filtered_v3, 
               un = TRUE,
               continuous = "std",
               binary = "std")

love.plot(bal, 
          stats = "mean.diffs", 
          abs = TRUE, 
          threshold = 0.2, 
          var.order = "unadjusted", 
          stars = TRUE)

bal2 <- bal.tab(MaineCEMv7.1, 
               data = master_raster_V2_reduced_filtered_v3, 
               un = TRUE,
               continuous = "std",
               binary = "std")

love.plot(bal2, 
          stats = "mean.diffs", 
          abs = TRUE, 
          threshold = 0.2, 
          var.order = "unadjusted", 
          stars = TRUE)

bal3 <- bal.tab(MaineCEMv7.2, 
                data = master_raster_V2_reduced_filtered_v3, 
                un = TRUE,
                continuous = "std",
                binary = "std")

love.plot(bal3, 
          stats = "mean.diffs", 
          abs = TRUE, 
          threshold = 0.2, 
          var.order = "unadjusted", 
          stars = TRUE)

bal4 <- bal.tab(MaineCEMv7.3, 
                data = master_raster_V2_reduced_filtered_v3, 
                un = TRUE,
                continuous = "std",
                binary = "std")

love.plot(bal4, 
          stats = "mean.diffs", 
          abs = TRUE, 
          threshold = 0.2, 
          var.order = "unadjusted",
          stars = TRUE)

bal3
bal4


ggplot(renewable_comparison, aes(x = year, y = renewable_pct, color = region)) +
  geom_line(linewidth = 1) +
  labs(
    title = "",
    x = "Year",
    y = "Renewable Energy Percentage",
    color = "Region"
  ) +
  theme_minimal()

#extract table
balance_df <- as.data.frame(bal4$Balance)

# Clean
balance_df <- balance_df %>%
  tibble::rownames_to_column(var = "Variable")

print(xtable(balance_df, 
             caption = "Balance Measures Before and After Matching", 
             digits = 4), 
      include.rownames = FALSE, 
      type = "latex")


### MaineCEMv7.3 Map-----

#read in data
landval <- rast("~/Wageningen/University/Thesis/Data/1. Modified Data/landval.tiff")
MaineCEMv7.3 <- readRDS("~/Wageningen/University/Thesis/Data/1. Modified Data/2. Models/MaineCEM/MaineCEMv7_3.rds")
master_raster_V2_reduced <- readRDS("~/Wageningen/University/Thesis/Data/1. Modified Data/master_raster_V2_reduced.rds")

#Removing states which had major policy changes from 2005-2020
master_raster_V2_reduced_filtered_v3 <- master_raster_V2_reduced %>%
  filter(!State %in% c("DC", "DE", "MT", "CT", "CO", "NV", "TX", "WA", 
                       "AZ", "CA", "HI", "NJ", "NM", "PA", "MI", "MO", 
                       "OH", "MA", "MD", "KS", "IL", "MN", "OR", 
                       "RI", "NY", "NC", "WI", "NH", "VT", "VA"))

#pull match values into a df
MaineCEMv7.3_df <- data.frame(
  strata = MaineCEMv7.3$strata,
  group  = MaineCEMv7.3$groups,
  matched = MaineCEMv7.3$matched,
  weight = MaineCEMv7.3$w
)

#joining with master_raster_v2_reduced_filtered_v3
master_raster_V2_reduced_filtered_v3 <- bind_cols(master_raster_V2_reduced_filtered_v3, MaineCEMv7.3_df)
head(master_raster_V2_reduced_filtered_v3)

#read in original master raster for all cells
Master_raster <- readRDS("~/Wageningen/University/Thesis/Data/1. Modified Data/master_raster_df_v2.rds")

# Thin down Master_raster
Master_raster <- Master_raster %>%
  select(cell, x, y, landval_ln)

gc()

# Join necessary columns to full data set
Master_raster <- Master_raster %>%
  left_join(
    master_raster_V2_reduced_filtered_v3 %>% select(cell, Maine, group, matched),
    by = "cell"
  )

#create CEM_map_df
CEM_map_df <- Master_raster %>%
  mutate(
    match_status = case_when(
      matched == TRUE & group == 1 ~ "Maine Matched",
      matched == TRUE & group == 0 ~ "Non-Maine Matched",
      matched == FALSE & group == 1 ~ "Maine Non-Matched",
      TRUE ~ NA_character_)) %>%
  select(x, y, match_status )

CEM_map_df <- CEM_map_df %>%
  mutate(match_status = as.factor(match_status))

CEM_map_df$match_status <- as.numeric(factor(CEM_map_df$match_status, exclude = NULL))
str(CEM_map_df)
summary(CEM_map_df$match_status)

#rasterizing

CEM_map_rast <- rast(CEM_map_df, type = "xyz", crs = crs(landval))
plot(CEM_map_rast)

#Masking
state_borders <- vect("~/Wageningen/University/Thesis/Data/1. Modified Data/states_vect/states_vect.shp")


us_mask <- rasterize(state_borders, CEM_map_rast, field=1)
us_mask
plot(us_mask)

CEM_map_rast[is.na(us_mask)] <- 5

plot(CEM_map_rast)

plot(CEM_map_rast, 
     col = c("#304529", "#e69f00", "#77ab59", "#dce3c7", "#536878"))

#Finalizing Map
plot(CEM_map_rast, 
     col = c("#304529", "#e69f00", "#77ab59", "#dce3c7", "#536878"),
     legend = FALSE,
     main = "")

legend("bottomleft",
       legend = c("Matched Maine cells", 
                  "Unmatched Maine cells", 
                  "Matched counterfactual cells", 
                  "Unmatched non-Maine cells"),
       fill = c("#304529", "#e69f00", "#77ab59", "#dce3c7"),
       border = "black",
       bty = "n",
       cex = .8,
       inset = c(0.02, 0.08))

plot(state_borders, add= T)


##Actually, lets grey out the states which were removed from matching as well

library(terra)

#states removed from matching
states_to_gray <- c("DC", "DE", "MT", "CT", "CO", "NV", "TX", "WA", 
                    "AZ", "CA", "HI", "NJ", "NM", "PA", "MI", "MO", 
                    "OH", "MA", "MD", "KS", "IL", "MN", "OR", 
                    "RI", "NY", "NC", "WI", "NH", "VT", "VA")

# define colors
fill_colors <- ifelse(state_borders$STUSPS %in% states_to_gray, "lightgray", NA)  # NA means no fill
border_colors <- ifelse(state_borders$STUSPS %in% states_to_gray, "gray60", "black")

#test
plot(state_borders,
     col = fill_colors,
     border = "black",
     lwd = 1)


#Final map with greyed out states
plot(CEM_map_rast, 
     col = c("#304529", "#e69f00", "#77ab59", "#dce3c7", "#536878"),
     legend = FALSE,
     main = "")

legend("bottomleft",
       legend = c("Matched Maine cells", 
                  "Unmatched Maine cells", 
                  "Matched counterfactual cells", 
                  "Unmatched non-Maine cells",
                  "Removed from matching consideration"),
       fill = c("#304529", "#e69f00", "#77ab59", "#dce3c7", "lightgray"),
       border = "black",
       bty = "n",
       cex = .7,
       inset = c(0.02, 0.07))

plot(state_borders,
     col = fill_colors,
     border = "black",
     lwd = 1,
     add = T)




### LCOE graph----
Solar_Wind_LCOE_estimates <- read_excel("~/Wageningen/University/Thesis/Data/1. Modified Data/Solar_Wind_LCOE_estimates.xlsx", sheet = "Import")

Solar_Wind_LCOE_estimates <- Solar_Wind_LCOE_estimates %>%
  mutate(LCOE_diff = Solar_LCOE - Wind_LCOE)

lcoe_long <- Solar_Wind_LCOE_estimates %>%
  pivot_longer(cols = c(Solar_LCOE, Wind_LCOE),
               names_to = "Technology",
               values_to = "LCOE") %>%
  mutate(Technology = recode(Technology,
                             "Solar_LCOE" = "Solar",
                             "Wind_LCOE" = "Wind"))

ggplot(lcoe_long, aes(x = Year, y = LCOE, color = Technology, group = interaction(Region, Technology))) +
  geom_line(linewidth = 1) +
  labs(title = "",
       y = "LCOE ($/MWh)",
       x = "Year",
       color = "Technology") +
  theme_minimal() +
  scale_color_manual(values = c("Solar" = "#E69F00", "Wind" = "#0072B2"))


lcoe_avg <- Solar_Wind_LCOE_estimates %>%
  group_by(Year) %>%
  summarise(
    Solar = mean(Solar_LCOE, na.rm = TRUE),
    Wind  = mean(Wind_LCOE, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = c(Solar, Wind),
               names_to = "Technology",
               values_to = "LCOE")

ggplot(lcoe_avg, aes(x = Year, y = LCOE, color = Technology)) +
  geom_line(size = 1.2) +
  labs(title = "",
       y = "LCOE ($/MWh)",
       x = "Year",
       color = "Technology") +
  scale_color_manual(values = c("Solar" = "#E69F00", "Wind" = "#0072B2")) +
  theme_minimal()


lcoe_isone <- Solar_Wind_LCOE_estimates %>%
  filter(Region == "ISO-NE") %>%
  pivot_longer(cols = c(Solar_LCOE, Wind_LCOE),
               names_to = "Technology",
               values_to = "LCOE") %>%
  mutate(Technology = recode(Technology,
                             "Solar_LCOE" = "Solar",
                             "Wind_LCOE" = "Wind"))


ggplot(lcoe_isone, aes(x = Year, y = LCOE, color = Technology)) +
  geom_line(size = 1.2) +
  labs(title = "",
       y = "LCOE ($/MWh)",
       x = "Year",
       color = "Technology") +
  scale_color_manual(values = c("Solar" = "#E69F00", "Wind" = "#0072B2")) +
  theme_minimal()

lcoe_split <- Solar_Wind_LCOE_estimates %>%
  mutate(Group = if_else(Region == "ISO-NE", "ISO-NE", "Other Regions")) %>%
  pivot_longer(cols = c(Solar_LCOE, Wind_LCOE),
               names_to = "Technology",
               values_to = "LCOE") %>%
  mutate(Technology = recode(Technology,
                             "Solar_LCOE" = "Solar",
                             "Wind_LCOE" = "Wind"))

lcoe_plot_data <- lcoe_split %>%
  group_by(Group, Technology, Year) %>%
  summarise(LCOE = mean(LCOE, na.rm = TRUE), .groups = "drop")

ggplot(lcoe_plot_data, aes(x = Year, y = LCOE, color = Technology, linetype = Group)) +
  geom_line(size = 1.2) +
  labs(title = "",
       y = "LCOE ($/MWh)",
       x = "Year",
       color = "Technology",
       linetype = "Region Group") +
  scale_color_manual(values = c("Solar" = "#E69F00", "Wind" = "#0072B2")) +
  scale_linetype_manual(values = c("All Other Regions" = "solid", "ISO-NE" = "dashed")) +
  theme_minimal()

