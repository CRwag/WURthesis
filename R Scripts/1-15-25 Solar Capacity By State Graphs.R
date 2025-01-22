library(tidyverse)
library(ggplot2)
library(readxl)
library(dplyr)


generators <- read_xlsx("~/Wageningen/University/Thesis/Data/EIA/EIA-860M/Operatingandretired.xlsx")
names(generators)
generators <- generators %>%
  mutate(`Retirement Year` = ifelse(is.na(`Retirement Year`), 2500, `Retirement Year`))




generators_reduced <- generators %>%
  select(`Plant State`, `Nameplate Capacity (MW)`, Technology, `Operating Year`, `Retirement Year`)

names(generators_reduced)
levels(as.factor(generators_reduced$Technology))


# Filter for solar generators and years between 2002 and 2024
solar_generators <- generators_reduced %>%
  filter(Technology == "Solar Photovoltaic", 
         `Operating Year` >= 2002, 
         `Operating Year` <= 2024) %>%
  mutate(`Retirement Year` = ifelse(is.na(`Retirement Year`), 2024, `Retirement Year`))

# Expand the dataset for all years of operation
solar_generators_expanded <- solar_generators %>%
  rowwise() %>%
  mutate(Years = list(seq(`Operating Year`, `Retirement Year`))) %>%
  unnest(Years)

# Count the number of active generators by year and state
solar_generators_by_state <- solar_generators_expanded %>%
  group_by(Years, `Plant State`) %>%
  summarise(Active_Generators = n(), .groups = "drop")

# Plot the results
ggplot(solar_generators_by_state, aes(x = Years, y = Active_Generators, color = `Plant State`)) +
  geom_line() +
  labs(title = "Number of Active Solar Generators Over Time by State",
       x = "Year",
       y = "Number of Active Solar Generators",
       color = "State") +
  theme_minimal() +
  scale_x_continuous(limits = c(2002, 2024), breaks = seq(2002, 2024, 2))

# Filter the data for the last year (2024) for labeling
label_data <- solar_generators_by_state %>%
  filter(Years == 2024)

# Plot with line labels at the end of each line
ggplot(solar_generators_by_state, aes(x = Years, y = Active_Generators, color = `Plant State`)) +
  geom_line() +
  geom_text(data = label_data, aes(label = `Plant State`),
            hjust = -0.2, vjust = 0.5, size = 3) +
  labs(title = "Number of Active Solar Generators Over Time by State",
       x = "Year",
       y = "Number of Active Solar Generators",
       color = "State") +
  theme_minimal() +
  scale_x_continuous(limits = c(2002, 2024), breaks = seq(2002, 2024, 2)) +
  coord_cartesian(clip = "off")  # Allow labels to extend beyond the plot area

# Plot with line labels and no legend
ggplot(solar_generators_by_state, aes(x = Years, y = Active_Generators, color = `Plant State`)) +
  geom_line() +
  geom_text(data = label_data, aes(label = `Plant State`),
            hjust = -0.2, vjust = 0.5, size = 3) +
  labs(title = "Number of Active Solar Generators Over Time by State",
       x = "Year",
       y = "Number of Active Solar Generators") +  # Removed 'color' label
  theme_minimal() +
  scale_x_continuous(limits = c(2002, 2024), breaks = seq(2002, 2024, 2)) +
  coord_cartesian(clip = "off") +  # Allow labels to extend beyond the plot area
  theme(legend.position = "none")  # Remove the legend

# Include only highest states
selected_states <- c("CA", "NC", "MN", "NY", "MA", "NJ")
filtered_data <- solar_generators_by_state %>%
  filter(`Plant State` %in% selected_states)

# Plot the filtered data
ggplot(filtered_data, aes(x = Years, y = Active_Generators, color = `Plant State`)) +
  geom_line() +
  geom_text(data = label_data %>% filter(`Plant State` %in% selected_states),
            aes(label = `Plant State`),
            hjust = -0.2, vjust = 0.5, size = 3) +
  labs(title = "Number of Active Solar Generators Over Time by Selected States",
       x = "Year",
       y = "Number of Active Solar Generators") +
  theme_minimal() +
  scale_x_continuous(limits = c(2002, 2024), breaks = seq(2002, 2024, 2)) +
  coord_cartesian(clip = "off") +
  theme(legend.position = "none")  # Remove the legend


#MW CAPACITY BY STATE

# Group by Plant State and Year to calculate total capacity
solar_capacity_by_state <- solar_generators_expanded %>%
  group_by(`Plant State`, Years) %>%
  summarize(Total_Capacity = sum(`Nameplate Capacity (MW)`), .groups = "drop")

# Filter for years between 2002 and 2024
solar_capacity_by_state <- solar_capacity_by_state %>%
  filter(Years >= 2002, Years <= 2024)

# Plot the data
ggplot(solar_capacity_by_state, aes(x = Years, y = Total_Capacity, color = `Plant State`)) +
  geom_line() +
  geom_text(data = solar_capacity_by_state %>% filter(Years == 2024),
            aes(label = `Plant State`),
            hjust = -0.2, vjust = 0.5, size = 3) +
  labs(
    title = "Total Active Solar Nameplate Capacity Over Time by State",
    x = "Year",
    y = "Total Active Nameplate Capacity (MW)"
  ) +
  theme_minimal() +
  scale_x_continuous(limits = c(2002, 2024), breaks = seq(2002, 2024, 2)) +
  coord_cartesian(clip = "off") +  # Allow labels to extend beyond the plot area
  theme(legend.position = "none")  # Remove the legend

# Filter for specific states
selected_states_cap <- c("CA", "TX", "FL", "NC", "NV", "GA", "AZ", "VA")
solar_capacity_selected <- solar_capacity_by_state %>%
  filter(`Plant State` %in% selected_states_cap)

# Plot the data
ggplot(solar_capacity_selected, aes(x = Years, y = Total_Capacity, color = `Plant State`)) +
  geom_line() +
  geom_text(data = solar_capacity_selected %>% filter(Years == 2024),
            aes(label = `Plant State`),
            hjust = -0.2, vjust = 0.5, size = 3) +
  labs(
    title = "Total Active Solar Nameplate Capacity Over Time by Selected States",
    x = "Year",
    y = "Total Active Nameplate Capacity (MW)"
  ) +
  theme_minimal() +
  scale_x_continuous(limits = c(2002, 2024), breaks = seq(2002, 2024, 2)) +
  coord_cartesian(clip = "off") +  # Allow labels to extend beyond the plot area
  theme(legend.position = "none")  # Remove the legend




# % OF ENERGY PRODUCTION CAPACITY BY UTLITY SCALE SOLAR

# Filter for years of interest and compute total capacity per year per state
generators_filtered <- generators_reduced %>%
  filter(`Retirement Year` >= 2002, `Operating Year` <= 2024)

# Expand data to include all years of operation
generators_expanded <- generators_filtered %>%
  rowwise() %>%
  mutate(Years = list(seq(max(`Operating Year`, 2002), min(`Retirement Year`, 2024)))) %>%
  unnest(cols = c(Years))

# Group by state, year, and technology to calculate total capacity
capacity_by_year_tech <- generators_expanded %>%
  group_by(`Plant State`, Years, Technology) %>%
  summarize(Total_Capacity = sum(`Nameplate Capacity (MW)`, na.rm = TRUE), .groups = "drop")

# Calculate the total capacity for each state and year
total_capacity_by_year <- capacity_by_year_tech %>%
  group_by(`Plant State`, Years) %>%
  summarize(Total_Capacity = sum(Total_Capacity, na.rm = TRUE), .groups = "drop")

# Filter for Solar Photovoltaic and merge with total capacity
solar_capacity_by_year <- capacity_by_year_tech %>%
  filter(Technology == "Solar Photovoltaic") %>%
  rename(Solar_Capacity = Total_Capacity) %>%
  left_join(total_capacity_by_year, by = c("Plant State", "Years")) %>%
  mutate(Percentage_Solar = (Solar_Capacity / Total_Capacity) * 100)

# Plot the data
ggplot(solar_capacity_by_year, aes(x = Years, y = Percentage_Solar, color = `Plant State`)) +
  geom_line() +
  geom_text(data = solar_capacity_by_year %>% filter(Years == 2024),
            aes(label = `Plant State`),
            hjust = -0.2, vjust = 0.5, size = 3) +
  labs(
    title = "Percentage of Utility-Scale Energy Capacity from Solar PV by State Over Time",
    x = "Year",
    y = "Percentage of Nameplate Capacity (%)"
  ) +
  theme_minimal() +
  scale_x_continuous(limits = c(2002, 2024), breaks = seq(2002, 2024, 2)) +
  coord_cartesian(clip = "off") +  # Allows labels to extend outside the plot area
  theme(legend.position = "none")  # Removes the legend

#Individual Graphs
ggplot(solar_capacity_by_year, aes(x = Years, y = Percentage_Solar)) +
  geom_line(color = "blue") +  # Line color can be set manually
  labs(
    title = "Percentage of Solar Photovoltaic Capacity Over Time by State",
    x = "Year",
    y = "Percentage of Nameplate Capacity (%)"
  ) +
  theme_minimal() +
  scale_x_continuous(limits = c(2002, 2024), breaks = seq(2002, 2024, 2)) +
  facet_wrap(~`Plant State`, scales = "free_y") +  # Creates a separate plot for each state
  theme(
    strip.text = element_text(size = 8),  # Adjusts the size of the facet labels
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotates x-axis labels for better readability
  )






