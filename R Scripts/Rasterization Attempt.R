pp_csv <- read.csv("~/Wageningen/University/Thesis/Data/EIA/Power Plants geospatial/Power_Plants.csv")
pp_csv <- pp_csv %>%
  filter(!State %in% c("Alaska", "Hawaii", "Puerto Rico"))

                   
#RASTERIZING VECTOR DATA
pp_rast <-rast(all_power, resolution = 480)
pp_rast <- rasterize(all_power, pp_rast, field = "OBJECTID")
pp_rast
pp_rastdf <- as.data.frame(pp_rast, xy=T)

pp_rast2 <-rast(all_power, resolution = 480)
pp_rast2 <- rasterize(all_power, pp_rast2, field = "OBJECTID")