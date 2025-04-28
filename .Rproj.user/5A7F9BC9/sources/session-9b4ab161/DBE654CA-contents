#I guess I'm doing it by CBSA now

#COMBINE POWER PLANT AND CBSA DATA
combobyCBSA <- intersect(all_power, CBSA_vect)

combobyCBSA_df <- as.data.frame(combobyCBSA)
CBSA_df <- as.data.frame(CBSA_vect)

#CALCULATING AVERAGE GHI FOR EACH CBSA
avg_ghi_by_CBSA <- extract(avg_GHI, CBSA_vect, fun = mean, na.rm = TRUE)

#ASSOCIATING THE NEWLY CALCULATED AVERAGES WITH THEIR CBSA GEOID
avg_ghi_by_CBSA <- cbind(avg_ghi_by_CBSA, CBSA_df$GEOID)
avg_ghi_by_CBSA$ID <- NULL
avg_ghi_by_CBSA <- avg_ghi_by_CBSA %>%
  rename("GEOID" = `CBSA_df$GEOID`)

#MERGING THE CBSA GHI AVERAGES WITH combobyCBSA
combobyCBSA_df <- merge(combobyCBSA_df, avg_ghi_by_CBSA, by = "GEOID", all.x = TRUE)

##############################################################################

#CALCULATING AVERAGE GHI FOR EACH CBSA
avg_landval_by_cbsa <- extract(landval, CBSA_vect, fun = mean, na.rm = TRUE)

#ASSOCIATING THE NEWLY CALCULATED AVERAGES WITH THEIR CBSA GEOID
avg_landval_by_cbsa <- cbind(avg_landval_by_cbsa, CBSA_df$GEOID)
avg_landval_by_cbsa$ID <- NULL
avg_landval_by_cbsa <- avg_landval_by_cbsa %>%
  rename("GEOID" = `CBSA_df$GEOID`)

#MERGING THE CBSA GHI AVERAGES WITH combobyCBSA
combobyCBSA_df <- merge(combobyCBSA_df, avg_landval_by_cbsa, by = "GEOID", all.x = TRUE)

#RENAME TO avg_landval
combobyCBSA_df <- combobyCBSA_df %>%
  rename("avg_landval" = places_fmv_all)

#SAVING combobyCBSA
write.csv(combobyCBSA_df, "~/Wageningen/University/Thesis/Data/combobyCBSA.csv", row.names = FALSE)

#CREATING REDUCED FILE
reduced_combo_df <- combobyCBSA_df %>%
  select(GEOID, OBJECTID, Plant_Name, Plant_Code, sector_name, City, State, PrimSource, Total_MW, Longitude, Latitude, NAMELSAD, nsrdb3_ghi, avg_landval)

#CBSA & POWER PLANT TYPE DEFINED ROWS
reduced_combo_df_aggd <- reduced_combo_df %>%
  group_by(NAMELSAD, PrimSource) %>%
  summarize(sum(Total_MW))

#TOTAL MW PER CBSA
MW_by_CBSA <- reduced_combo_df %>%
  group_by(NAMELSAD) %>%
  summarise(sum(Total_MW))

#TOTAL SOLAR MW PER CBSA
SolarMW_by_CBSA <- reduced_combo_df_aggd %>%
  filter(PrimSource == "solar")

#TOTAL WIND MW PER CBSA
WindMW_by_CBSA <- reduced_combo_df_aggd %>%
  filter(PrimSource == "wind")

SolarMW_by_CBSA <- SolarMW_by_CBSA %>%
  left_join(reduced_combo_df %>% 
  select(NAMELSAD, nsrdb3_ghi, avg_landval), by = "NAMELSAD")

SolarMW_by_CBSA <- SolarMW_by_CBSA %>%
  distinct()

CBSA_names <- CBSA_df %>%
  distinct(NAMELSAD)

SolarMW_by_CBSA <- CBSA_names %>%
  left_join(SolarMW_by_CBSA, by = "NAMELSAD")

SolarMW_by_CBSA <- SolarMW_by_CBSA %>%
  mutate(PrimSource = ifelse(is.na(PrimSource), "solar", PrimSource)) %>%
  mutate(`sum(Total_MW)`= ifelse(is.na(`sum(Total_MW)`), 0, `sum(Total_MW)`))

SolarMW_by_CBSA <- SolarMW_by_CBSA %>%
  left_join(reduced_combo_df %>% select(NAMELSAD, nsrdb3_ghi, avg_landval), by = "NAMELSAD")

SolarMW_by_CBSA$nsrdb3_ghi.x = NULL
SolarMW_by_CBSA$avg_landval.x = NULL

SolarMW_by_CBSA <- SolarMW_by_CBSA %>%
  distinct()

SolarMW_by_CBSA <- SolarMW_by_CBSA %>%
  left_join(CBSA_df %>% select(NAMELSAD, GEOID), by = "NAMELSAD")

SolarMW_by_CBSA <- SolarMW_by_CBSA %>%
  left_join(avg_ghi_by_CBSA, by ="GEOID")

SolarMW_by_CBSA <- SolarMW_by_CBSA %>%
  left_join(avg_landval_by_cbsa, by ="GEOID")

SolarMW_by_CBSA$nsrdb3_ghi.y = NULL
SolarMW_by_CBSA$avg_landval.y = NULL

SolarMW_by_CBSA <- SolarMW_by_CBSA %>%
  rename(avg_landval = places_fmv_all)

SolarMW_by_CBSA <- SolarMW_by_CBSA %>%
  mutate(irr_2 = nsrdb3_ghi^2)

#Simple regression model
reg <- lm(`sum(Total_MW)` ~ log(nsrdb3_ghi) , data = SolarMW_by_CBSA)
summary(reg)

plot(SolarMW_by_CBSA$nsrdb3_ghi, SolarMW_by_CBSA$"sum(Total_MW)")
plot(SolarMW_by_CBSA$nsrdb3_ghi, SolarMW_by_CBSA$avg_landval)

table(SolarMW_by_CBSA$`sum(Total_MW)`
      )

write.csv(SolarMW_by_CBSA, "~/Wageningen/University/Thesis/Data/SolarMW_by_CBSA.csv")



