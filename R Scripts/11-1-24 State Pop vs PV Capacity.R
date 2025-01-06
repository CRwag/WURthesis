PVproj_sf <- st_read("~/Wageningen/University/Thesis/Data/USGC Large Scale Solar PV Data/uspvdbSHP/uspvdb_v2_0_20240801.shp")
PVproj_sf

PVMWstate <- PVproj_sf[c('p_state', 'p_cap_ac')] %>%
  st_drop_geometry()

population_data <- get_acs(
  geography = "state",
  variables = "B01003_001",  # Total population variable
  year = 2023,                # Most recent year available
  survey = "acs1"             # 1-year ACS survey
)

StateMWtots <- PVMWstate %>%
  group_by(p_state) %>%
  summarize(p_cap_ac = sum(p_cap_ac))
view(StateMWtots)

STabb <- readxl::read_xlsx('~/Wageningen/University/Thesis/Data/50 states and abbreviations.xlsx')

StateMWtots <- StateMWtots %>%
  left_join(STabb, by = c("p_state" = "Abbreviation"))

MWpop_combo <- StateMWtots %>%
  left_join(population_data, by = c("State" = "NAME"))

ggplot(MWpop_combo, aes(x = estimate, y = p_cap_ac)) +
  geom_point(color = "blue") +
  geom_text(aes(label = p_state), vjust = -0.5, color = "white", size = 2) +
  geom_smooth(method = "lm", color = "cyan", se = FALSE) +
  labs(
    title = "Scatterplot of State Population vs. MW Capacity",
    x = "State Population",
    y = "Total MW Capacity"
  ) +
  theme_dark()