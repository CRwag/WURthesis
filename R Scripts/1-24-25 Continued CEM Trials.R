library(tidyverse)
library(terra)
library(sf)
library(ggplot2)
library(cem)

#Pulling in MaineCEMv3
MaineCEMv3_TK <- readRDS("~/Wageningen/University/Thesis/Data/1. Modified Data/2. Models/MaineCEM/MaineCEMv3_TK.rds")
MaineCEMv3_TK
MaineCEMv3_TK$breaks
MaineCEMv3_TK$call

#Pulling in Master raster reduced
master_raster_V2_reduced <- readRDS("~/Wageningen/University/Thesis/Data/1. Modified Data/master_raster_V2_reduced.rds")

#Giving automated coarsening a go: Freedman-Diaconis' Rule [Can't get this to run, get error "Error in cutpoints[[vnames[i]]] : subscript out of bounds"]
MaineCEMv5 <- cem(
  treatment = "Maine",
  data = master_raster_V2_reduced,
  drop = c("cell", "x", "y", "State", "Solar_binary"),
  cutpoints = "fd",
  eval.imbalance = T,
  keep.all = T
)
  
write_rds(MaineCEMv4, "~/Wageningen/University/Thesis/Data/1. Modified Data/2. Models/MaineCEM/MaineCEMv4")
  
#Imbalance evaluation of MaineCEMv3
  Imb_MaineCEMv3_TK <- imbalance(
    group = master_raster_V2_reduced$Maine,
    data = master_raster_V2_reduced,
    drop = c("cell", "x", "y", "State", "Solar_binary"),
    weights = MaineCEMv3_TK$w
)

Imb_MaineCEMv3_TK
  
  
#Reducing MaineCEMv3 to k2k. Chose Euclidean beacuse it's the simplest and I understand it, and I don't understand how it would even make that much of a difference to choose another method
MaineCEMv3_k2k <- k2k(
    obj = MaineCEMv3_TK,
    data = master_raster_V2_reduced,
    method = "euclidean",
    verbose = 1
)
#doesn't work, "Error in k2k(obj = MaineCEMv3_TK, data = master_raster_V2_reduced, method = "euclidean",  : please first run cem() with option keep.all=TRUE"





MaineCEMv4_TK <- readRDS("~/Wageningen/University/Thesis/Data/1. Modified Data/2. Models/MaineCEM/MaineCEMv4_TK.rds")
MaineCEMv4_TK
MaineCEMv4_TK$breaks


#Rerunning MaineCEMv4 (Maine deciles), with keep.all = T so I can reduce to k2k next
MaineCEMv4.1 <- cem(
  treatment = "Maine",
  data = master_raster_V2_reduced,
  drop = c("cell", "x", "y", "State", "Solar_binary"),
  cutpoints = list(
    slope = c(1.069968e-15, 1.353629e+00, 1.712424e+00, 2.051407e+00, 2.420620e+00, 
              2.849432e+00, 3.378978e+00, 4.101578e+00, 5.214170e+00, 7.359044e+00, 
              3.617275e+01),
    
    HVline_dist = c(0.000, 1517.893, 3655.571, 6146.999, 9319.914, 
                    13310.808, 18397.217, 25458.107, 37034.730, 
                    60047.981, 122372.703),
    
    substation_dist = c(18.99437, 3915.01297, 6163.22994, 8268.04006, 
                        10550.32595, 13217.38435, 16470.47748, 20697.71579, 
                        27911.03502, 47418.43311, 114928.95692),
    
    nsrdb3_ghi = c(3.507047, 3.613267, 3.644786, 3.672163, 3.704348, 
                   3.734091, 3.761863, 3.795134, 3.846721, 3.908165, 
                   4.022752),
    
    landval_ln = c(18.99437, 3915.01297, 6163.22994, 8268.04006, 10550.32595, 
                   13217.38435, 16470.47748, 20697.71579, 27911.03502, 
                   47418.43311, 114928.95692)
  ),
  eval.imbalance = T,
  keep.all = T
)

write_rds(MaineCEMv4.1, "~/Wageningen/University/Thesis/Data/1. Modified Data/2. Models/MaineCEM/MaineCEMv4.1")


#Reducing MaineCEMv4.1 to k2k
MaineCEMv4.1_k2k <- k2k(
  obj = MaineCEMv4.1,
  data = master_raster_V2_reduced,
  method = "euclidean",
  verbose = 1
)

write_rds(MaineCEMv4.1_k2k, "~/Wageningen/University/Thesis/Data/1. Modified Data/2. Models/MaineCEM/MaineCEMv4.1k2k.rds")

MaineCEMv4.1_k2k

#Removing states which had major policy changes from 2017-2020
master_raster_V2_reduced_filtered <- master_raster_V2_reduced %>%
  filter(!State %in% c("AZ", "CA", "CO", "CT", "MA", "MD", 
                       "NH", "NJ", "NM", "NY", "OH", "PA", 
                       "VA", "WA"))

#MaineCEMv5: CEM with states who have had similar policies enacted from 2017-2020  removed
MaineCEMv5 <- cem(
  treatment = "Maine",
  data = master_raster_V2_reduced_filtered,
  drop = c("cell", "x", "y", "State", "Solar_binary"),
  cutpoints = list(
    slope = c(1.069968e-15, 1.353629e+00, 1.712424e+00, 2.051407e+00, 2.420620e+00, 
              2.849432e+00, 3.378978e+00, 4.101578e+00, 5.214170e+00, 7.359044e+00, 
              3.617275e+01),
    
    HVline_dist = c(0.000, 1517.893, 3655.571, 6146.999, 9319.914, 
                    13310.808, 18397.217, 25458.107, 37034.730, 
                    60047.981, 122372.703),
    
    substation_dist = c(18.99437, 3915.01297, 6163.22994, 8268.04006, 
                        10550.32595, 13217.38435, 16470.47748, 20697.71579, 
                        27911.03502, 47418.43311, 114928.95692),
    
    nsrdb3_ghi = c(3.507047, 3.613267, 3.644786, 3.672163, 3.704348, 
                   3.734091, 3.761863, 3.795134, 3.846721, 3.908165, 
                   4.022752),
    
    landval_ln = c(18.99437, 3915.01297, 6163.22994, 8268.04006, 10550.32595, 
                   13217.38435, 16470.47748, 20697.71579, 27911.03502, 
                   47418.43311, 114928.95692)
  ),
  eval.imbalance = T,
  keep.all = T
)
  
write_rds(MaineCEMv5, "~/Wageningen/University/Thesis/Data/1. Modified Data/2. Models/MaineCEM/MaineCEMv5")

MaineCEMv5
summary(MaineCEMv5)


#Removing states which had major policy changes from 2015-2020
master_raster_V2_reduced_filtered_v2 <- master_raster_V2_reduced %>%
  filter(!State %in% c("AZ", "CA", "CO", "CT", "MA", "MD", 
                       "NH", "NJ", "NM", "NY", "OH", "PA", 
                       "VA", "WA", "IL", "KS", "MI", "OR",
                       "RI", "VT"))

#MaineCEMv6: CEM with states who have had similar policies enacted from 2015-2020 removed
MaineCEMv6 <- cem(
  treatment = "Maine",
  data = master_raster_V2_reduced_filtered_v2,
  drop = c("cell", "x", "y", "State", "Solar_binary"),
  cutpoints = list(
    slope = c(1.069968e-15, 1.353629e+00, 1.712424e+00, 2.051407e+00, 2.420620e+00, 
              2.849432e+00, 3.378978e+00, 4.101578e+00, 5.214170e+00, 7.359044e+00, 
              3.617275e+01),
    
    HVline_dist = c(0.000, 1517.893, 3655.571, 6146.999, 9319.914, 
                    13310.808, 18397.217, 25458.107, 37034.730, 
                    60047.981, 122372.703),
    
    substation_dist = c(18.99437, 3915.01297, 6163.22994, 8268.04006, 
                        10550.32595, 13217.38435, 16470.47748, 20697.71579, 
                        27911.03502, 47418.43311, 114928.95692),
    
    nsrdb3_ghi = c(3.507047, 3.613267, 3.644786, 3.672163, 3.704348, 
                   3.734091, 3.761863, 3.795134, 3.846721, 3.908165, 
                   4.022752),
    
    landval_ln = c(18.99437, 3915.01297, 6163.22994, 8268.04006, 10550.32595, 
                   13217.38435, 16470.47748, 20697.71579, 27911.03502, 
                   47418.43311, 114928.95692)
  ),
  eval.imbalance = T,
  keep.all = T
)

write_rds(MaineCEMv6, "~/Wageningen/University/Thesis/Data/1. Modified Data/2. Models/MaineCEM/MaineCEMv6.rds")


#Removing states which had major policy changes from 2005-2020
master_raster_V2_reduced_filtered_v3 <- master_raster_V2_reduced %>%
  filter(!State %in% c("DC", "DE", "MT", "CT", "CO", "NV", "TX", "WA", 
                       "AZ", "CA", "HI", "NJ", "NM", "PA", "MI", "MO", 
                       "OH", "MA", "MD", "KS", "IL", "MN", "OR", 
                       "RI", "NY", "NC", "WI", "NH", "VT", "VA"))

#calculating new decile cutpoints
slope_cutpoints <- quantile(master_raster_V2_reduced_filtered_v3$slope, probs = seq(0, 1, 0.1)) 
HVline_cutpoints <- quantile(master_raster_V2_reduced_filtered_v3$HVline_dist, probs = seq(0, 1, 0.1))
substation_cutpoints <- quantile(master_raster_V2_reduced_filtered_v3$substation_dist, probs = seq(0, 1, 0.1))
nsrdb3_cutpoints <- quantile(master_raster_V2_reduced_filtered_v3$nsrdb3_ghi, probs = seq(0, 1, 0.1))
landval_cutpoints <- quantile(master_raster_V2_reduced_filtered_v3$landval_ln, probs = seq(0, 1, 0.1))


#MaineCEMv7: CEM with states who have had similar policies enacted from 2005-2020 removed
MaineCEMv7 <- cem(
  treatment = "Maine",
  data = master_raster_V2_reduced_filtered_v3,
  drop = c("cell", "x", "y", "State", "Solar_binary"),
  cutpoints = list(
    slope = c(0.9427901, 1.5169287, 2.3574249, 4.9066774, 10.2629126), # cutpoints at following quantiles due to the meaninfulness in differences of slope between 0-10 degrees: 20%, 40%, 60%, 80%, 90%
    
    HVline_dist = c(480, 1357.645, 2036.468, 3219.938, 4528.311, 6258.434,
                    8850.763, 12960.000, 21706.405), # deciles
    
    substation_dist = c(2466.432268, 3896.565904, 5230.093659, 6597.342563,
                        8099.446665, 9878.781321, 12232.150857, 15971.340278, 24303.116622
                        ), # deciles
    
    nsrdb3_ghi = c(4.061901, 4.199900, 4.325478, 4.481868, 4.590551, 4.682681,
                   4.775998, 4.869222, 5.005671), #deciles
    
    landval_ln = c(7.539708, 7.928623, 8.178812, 8.388204, 8.585097, 8.798123, 
                   9.057548, 9.370845, 9.833447) #deciles
  ),
  eval.imbalance = T,
  keep.all = T,
  L1.breaks = list(
    slope = c(0.9427901, 1.5169287, 2.3574249, 4.9066774, 10.2629126),
    
    HVline_dist = c(480, 1357.645, 2036.468, 3219.938, 4528.311, 6258.434,
                    8850.763, 12960.000, 21706.405),
    
    substation_dist = c(2466.432268, 3896.565904, 5230.093659, 6597.342563,
                        8099.446665, 9878.781321, 12232.150857, 15971.340278, 24303.116622),
    
    nsrdb3_ghi = c(4.061901, 4.199900, 4.325478, 4.481868, 4.590551, 4.682681,
                   4.775998, 4.869222, 5.005671),
    
    landval_ln = c(7.539708, 7.928623, 8.178812, 8.388204, 8.585097, 8.798123, 
                   9.057548, 9.370845, 9.833447)
  )
)

MaineCEMv7$imbalance

write_rds(MaineCEMv7, "~/Wageningen/University/Thesis/Data/1. Modified Data/2. Models/MaineCEM/MaineCEMv7.rds")

#MaineCEMv7.1 (all decile): CEM with states who have had similar policies enacted from 2005-2020 removed
MaineCEMv7.1 <- cem(
  treatment = "Maine",
  data = master_raster_V2_reduced_filtered_v3,
  drop = c("cell", "x", "y", "State", "Solar_binary"),
  cutpoints = list(
    slope = slope_cutpoints,
    
    HVline_dist = HVline_cutpoints,
    
    substation_dist = substation_cutpoints,
    
    nsrdb3_ghi = nsrdb3_cutpoints,
    
    landval_ln = landval_cutpoints
  ),
  eval.imbalance = T,
  keep.all = T,
  L1.breaks = list(
    slope = slope_cutpoints,
    
    HVline_dist = HVline_cutpoints,
    
    substation_dist = substation_cutpoints,
    
    nsrdb3_ghi = nsrdb3_cutpoints,
    
    landval_ln = landval_cutpoints
  )
)

write_rds(MaineCEMv7.1, "~/Wageningen/University/Thesis/Data/1. Modified Data/2. Models/MaineCEM/MaineCEMv7_1.rds")


master_raster_V2_reduced_filtered_v3_ME <- master_raster_V2_reduced_filtered_v3 %>%
  filter(State == "ME")

nsrdb3_cutpoints_25ME <- quantile(master_raster_V2_reduced_filtered_v3_ME$nsrdb3_ghi, probs = seq(0, 1, 0.25))
landval_cutpoints_10ME <- quantile(master_raster_V2_reduced_filtered_v3_ME$landval_ln, probs = seq(0, 1, 0.1))
landval_cutpoints_25ME <- quantile(master_raster_V2_reduced_filtered_v3_ME$landval_ln, probs = seq(0, 1, 0.25))


#MaineCEMv7.2: CEM with states who have had similar policies enacted from 2005-2020 removed (custom cutpoints)
MaineCEMv7.2 <- cem(
  treatment = "Maine",
  data = master_raster_V2_reduced_filtered_v3,
  drop = c("cell", "x", "y", "State", "Solar_binary"),
  cutpoints = list(
    slope = c(0, 3, 5, 8, 10, Inf), # Binned into categories based on domain relevant categories [<3 degrees (ideal), 3-5 degrees (very suitable), 5-8 degrees (moderately suitable), 8-10 degrees (less suitable), 10+ (unsuitable)]
    
    HVline_dist = c(0, 1, 5, 7, 10, Inf), # Binned into categories based on domain relevant categories [<1km (ideal), 1-5km (very suitable), 5-7km (moderately suitable), 7-10km (less suitable), 10km+ (unsuitable)]
    
    substation_dist = c(0, 1, 5, 7, 10, Inf), # Binned into categories based on domain relevant categories [<1km (ideal), 1-5km (very suitable), 5-7km (moderately suitable), 7-10km (less suitable), 10km+ (unsuitable)]
    
    nsrdb3_ghi = nsrdb3_cutpoints_25ME, # Binned by quartiles of Maine GHI values
    
    landval_ln = landval_cutpoints_10ME # Binned by deciles of Maine landval_ln values
  ),
  eval.imbalance = T,
  keep.all = T,
  L1.breaks = list(
    slope = c(0, 3, 5, 8, 10, Inf), # Binned into categories based on domain relevant categories [<3 degrees (ideal), 3-5 degrees (very suitable), 5-8 degrees (moderately suitable), 8-10 degrees (less suitable), 10+ (unsuitable)]
    
    HVline_dist = c(0, 1, 5, 7, 10, Inf), # Binned into categories based on domain relevant categories [<1km (ideal), 1-5km (very suitable), 5-7km (moderately suitable), 7-10km (less suitable), 10km+ (unsuitable)]
    
    substation_dist = c(0, 1, 5, 7, 10, Inf), # Binned into categories based on domain relevant categories [<1km (ideal), 1-5km (very suitable), 5-7km (moderately suitable), 7-10km (less suitable), 10km+ (unsuitable)]
    
    nsrdb3_ghi = nsrdb3_cutpoints_25ME, # Binned by quartiles of Maine GHI values
    
    landval_ln = landval_cutpoints_10ME # Binned by deciles of Maine landval_ln values
  )
)

#MaineCEMv7.3: CEM with states who have had similar policies enacted from 2005-2020 removed (custom cutpoints)
MaineCEMv7.3 <- cem(
  treatment = "Maine",
  data = master_raster_V2_reduced_filtered_v3,
  drop = c("cell", "x", "y", "State", "Solar_binary"),
  cutpoints = list(
    slope = c(0, 3, 5, 8, 10, Inf), # Binned into categories based on domain relevant categories [<3 degrees (ideal), 3-5 degrees (very suitable), 5-8 degrees (moderately suitable), 8-10 degrees (less suitable), 10+ (unsuitable)]
    
    HVline_dist = c(0, 1, 5, 7, 10, Inf), # Binned into categories based on domain relevant categories [<1km (ideal), 1-5km (very suitable), 5-7km (moderately suitable), 7-10km (less suitable), 10km+ (unsuitable)]
    
    substation_dist = c(0, 1, 5, 7, 10, Inf), # Binned into categories based on domain relevant categories [<1km (ideal), 1-5km (very suitable), 5-7km (moderately suitable), 7-10km (less suitable), 10km+ (unsuitable)]
    
    nsrdb3_ghi = nsrdb3_cutpoints_25ME, # Binned by quartiles of Maine GHI values
    
    landval_ln = landval_cutpoints_25ME # Binned by quartiles of Maine landval_ln values
  ),
  eval.imbalance = T,
  keep.all = T,
  L1.breaks = list(
    slope = c(0, 3, 5, 8, 10, Inf), # Binned into categories based on domain relevant categories [<3 degrees (ideal), 3-5 degrees (very suitable), 5-8 degrees (moderately suitable), 8-10 degrees (less suitable), 10+ (unsuitable)]
    
    HVline_dist = c(0, 1, 5, 7, 10, Inf), # Binned into categories based on domain relevant categories [<1km (ideal), 1-5km (very suitable), 5-7km (moderately suitable), 7-10km (less suitable), 10km+ (unsuitable)]
    
    substation_dist = c(0, 1, 5, 7, 10, Inf), # Binned into categories based on domain relevant categories [<1km (ideal), 1-5km (very suitable), 5-7km (moderately suitable), 7-10km (less suitable), 10km+ (unsuitable)]
    
    nsrdb3_ghi = nsrdb3_cutpoints_25ME, # Binned by quartiles of Maine GHI values
    
    landval_ln = landval_cutpoints_25ME # Binned by quartiles of Maine landval_ln values
  )
)

write_rds(MaineCEMv7.3, "~/Wageningen/University/Thesis/Data/1. Modified Data/2. Models/MaineCEM/MaineCEMv7_3.rds")



