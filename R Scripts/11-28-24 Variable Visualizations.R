library(tidyverse)
library(sf)
library(terra)
library(ggplot2)

master.raster <- read_rds("~/Wageningen/University/Thesis/Data/1. Modified Data/master_raster_df_v2.rds")
names(master.raster)

master.raster <- master.raster %>%
  mutate(landval = exp(landval_ln))
  
#Land Value
#Histogram: Log Land Value for all cells
master.raster %>%
  filter(landval_ln >0, !is.na(landval_ln)) %>%
  ggplot(aes(x = landval_ln)) +
  geom_histogram(fill = "lightgreen", color = "black", bins = 25)

#Scatterplot: Solar Capacity & Log Land Value for all cells that contain solar
master.raster %>%
  filter(Solar_MW > 0, !is.na(Solar_MW)) %>%
  ggplot(aes(x = landval_ln, y = Solar_MW)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue")

#Scatterplot: Solar Capacity & Land Value for all cells that contain solar
master.raster %>%
  filter(Solar_MW > 0, !is.na(Solar_MW)) %>%
  ggplot(aes(x = landval, y = Solar_MW)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue")

#Histogram of Log Landval for all cells that contain solar
master.raster %>%
  filter(Solar_MW > 0, !is.na(Solar_MW)) %>%
  ggplot(aes(x = landval_ln)) +
  geom_histogram(binwidth = 0.5, fill = "lightgreen", color = "black")



#Solar Radiation
#Scatterplot: Solar Capacity & Solar Radiation for all cells that contain solar
master.raster %>%
  filter(Solar_MW > 0, !is.na(Solar_MW)) %>%
  ggplot(aes(x = nsrdb3_ghi, y = Solar_MW)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue")

#Histogram of Solar Radiation for all cells that contain solar
master.raster %>%
  filter(Solar_MW > 0, !is.na(Solar_MW)) %>%
  ggplot(aes(x = nsrdb3_ghi)) +
  geom_histogram(fill = "lightgreen", color = "black") +
  labs(title = "Solar Radiation Values for All Cells That Contain Solar Power Plants",
       x= "GHI (watt/m2)",
       y= "Number of Cells in Bin")


#Substation Distance
#Scatterplot
master.raster %>%
  filter(Solar_MW > 0, !is.na(Solar_MW)) %>%
  ggplot(aes(x = substation_dist, y = Solar_MW)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue")

#Histogram
master.raster %>%
  filter(Solar_MW > 0, !is.na(Solar_MW)) %>%
  ggplot(aes(x = substation_dist)) +
  geom_histogram(binwidth = 500, fill = "lightgreen", color = "black")



#High-voltage Powerline Distance
#Scatterplot
master.raster %>%
  filter(Solar_MW > 0, !is.na(Solar_MW)) %>%
  ggplot(aes(x = HVline_dist, y = Solar_MW)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue")

#Histogram
master.raster %>%
  filter(Solar_MW > 0, !is.na(Solar_MW)) %>%
  ggplot(aes(x = HVline_dist)) +
  geom_histogram(binwidth = 500, fill = "lightgreen", color = "black")


#Slope
#Scatterplot
master.raster %>%
  filter(Solar_MW > 0, !is.na(Solar_MW)) %>%
  ggplot(aes(x = slope, y = Solar_MW)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue")

master.raster %>%
  filter(Solar_MW > 0, !is.na(Solar_MW)) %>%
  ggplot(aes(x = slope)) +
  geom_histogram(fill = "lightgreen", color = "black")

master.raster %>%
  filter(Solar_MW > 0, !is.na(Solar_MW)) %>%
  ggplot(aes(x = slope, y = Solar_MW)) +
  stat_summary(fun = sum, geom = "bar", fill = "blue", color = "black")

#histogram
master.raster %>%
  filter(Solar_MW > 0, !is.na(Solar_MW)) %>%
  ggplot(aes(x = slope)) +
  geom_histogram(fill = "lightgreen", color = "black")


#Aspect
master.raster %>%
  filter(Solar_MW > 0, !is.na(Solar_MW)) %>%
  ggplot(aes(x = aspect, y = Solar_MW)) +
  geom_point(alpha = 0.5)

master.raster %>%
  filter(Solar_MW > 0, !is.na(Solar_MW)) %>%
  ggplot(aes(x = aspect_cardinal, y = Solar_MW)) +
  stat_summary(fun = sum, geom = "bar", fill = "lightgreen", color = "black")

master.raster %>%
  filter(Solar_MW > 0, !is.na(Solar_MW)) %>%
  ggplot(aes(x = aspect_cardinal)) +
  geom_bar(fill = "lightgreen", color = "black") +
  labs(title = "Count of Observations by Aspect Cardinal Direction",
       x = "Aspect (Cardinal Direction)",
       y = "Count of Observations")

#State
#Box Plot
master.raster %>%
  filter(Solar_MW > 0, !is.na(Solar_MW)) %>%
  ggplot(aes(x = State, y = Solar_MW)) +
  geom_boxplot() +
  coord_flip()

#Bar Chart
master.raster %>%
  filter(Solar_MW > 0, !is.na(Solar_MW)) %>%
  ggplot(aes(x = State, y = Solar_MW)) +
  stat_summary(fun = sum, geom = "bar", fill = "lightblue", color = "black") +
  coord_flip()

