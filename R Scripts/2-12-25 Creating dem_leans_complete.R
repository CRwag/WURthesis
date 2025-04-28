

###Transforming and adding state election data to cell_year_panel (created in "2-6-25 Creating Cell Panel Data Set")

#Pulling 1976-2020 election data from MIT
Election_df <- read_csv("~/Wageningen/University/Thesis/Data/MIT Election Data and Science Lab/dataverse_files/1976-2020-president.csv")
head(Election_df)

#filtering down
Election_df <- Election_df %>%
  filter(year > 2007) %>%
  select(year, state, state_po, candidatevotes, totalvotes, party_simplified)
head(Election_df)

#aggregating parties which are not Dem/Rep
unique(Election_df$party_simplified)

Election_df_others <- Election_df %>%
  filter(party_simplified %in% c("OTHER", "LIBERTARIAN")) %>%
  group_by(year, state) %>%
  summarise(tp_votes = sum(candidatevotes, na.rm = TRUE), .groups = "drop")

head(Election_df_others)

#removing OTHER and LIBERTARIAN from Election_df
Election_df <- Election_df %>%
  filter(!party_simplified %in% c("OTHER", "LIBERTARIAN"))

unique(Election_df$party_simplified)

#aggregating the dem and rep votes for each state-year (I guess some state in some years had more than one candidate from either party)
Election_df_dem <- Election_df %>%
  filter(party_simplified == "DEMOCRAT") %>%
  group_by(year, state) %>%
  summarise(dem_votes = sum(candidatevotes, na.rm = TRUE), .groups = "drop")

Election_df_rep <- Election_df %>%
  filter(party_simplified == "REPUBLICAN") %>%
  group_by(year, state) %>%
  summarise(rep_votes = sum(candidatevotes, na.rm = TRUE), .groups = "drop")

#creating one dataset
Election_df_wide <- Election_df %>%
  select(year, state, state_po, totalvotes) %>%
  distinct(state, year, .keep_all = TRUE)

Election_df_wide <- Election_df_wide %>%
  left_join(Election_df_dem, by = c("state", "year")) %>%
  left_join(Election_df_rep, by = c("state", "year")) %>%
  left_join(Election_df_others, by = c("state", "year"))
  
#checking all the votes sum to totalvotes
Election_df_wide %>%
  mutate(check = totalvotes - (dem_votes + rep_votes + tp_votes)) %>%
  filter(check != 0)

#pulling in 2024 election data from UC Santa Barbara
Election_2024_df <- read_excel("~/Wageningen/University/Thesis/Data/UCSB - The American Presidency Project/2024 election results by state.xlsx", skip = 2)

#cleaning
Election_2024_df <- Election_2024_df %>%
  slice(-61) %>%
  select(STATE, `TOTAL VOTES`, `KAMALA HARRISDemocratic_votes`, `KAMALA HARRISDemocratic_%`, `DONALD J. TRUMPRepublican_votes`, `DONALD J. TRUMPRepublican_%`, OTHERSVarious_votes, `OTHERSVarious_%`) %>%
  filter(!is.na(STATE))

Election_2024_df <- Election_2024_df %>%
  filter(!str_starts(STATE, "CD")) %>%
  rename(state = STATE,
         totalvotes = `TOTAL VOTES`,
         dem_votes = `KAMALA HARRISDemocratic_votes`,
         rep_votes = `DONALD J. TRUMPRepublican_votes`,
         dem_votes_percent = `KAMALA HARRISDemocratic_%`,
         rep_votes_percent = `DONALD J. TRUMPRepublican_%`,
         tp_votes = OTHERSVarious_votes,
         tp_votes_percent = `OTHERSVarious_%`)

#giving year column
Election_2024_df <- Election_2024_df %>%
  mutate(year = 2024)

#fixing Election_df_wide state values
Election_df_wide <- Election_df_wide %>%
  mutate(state = str_to_title(state))

#creaing percentage columns for Election_df_wide
Election_df_wide <- Election_df_wide %>%
  mutate(
    dem_votes_percent = (dem_votes / totalvotes) * 100,
    rep_votes_percent = (rep_votes / totalvotes) * 100,
    tp_votes_percent  = (tp_votes / totalvotes) * 100)

#checking and fixing structure of Election_2024_df
str(Election_2024_df)

Election_2024_df <- Election_2024_df %>%
  mutate(
    totalvotes = as.numeric(gsub(",", "", totalvotes)),
    dem_votes = as.numeric(gsub(",", "", dem_votes)),
    rep_votes = as.numeric(gsub(",", "", rep_votes)),
    tp_votes = as.numeric(gsub(",", "", tp_votes)),
    dem_votes_percent = as.numeric(gsub("%", "", dem_votes_percent)),
    rep_votes_percent = as.numeric(gsub("%", "", rep_votes_percent)),
    tp_votes_percent = as.numeric(gsub("%", "", tp_votes_percent)))
str(Election_2024_df)

#adding state_po to Election_2024_df and rearraging columns
Election_2024_df <- Election_2024_df %>%
  mutate(state_po = state.abb[match(state, state.name)]) %>%
  select(year, state, state_po, totalvotes, dem_votes, rep_votes, tp_votes, 
         dem_votes_percent, rep_votes_percent, tp_votes_percent)
head(Election_2024_df)

#combining into one dataset
Election_df_wide <- bind_rows(Election_df_wide, Election_2024_df)

#creating lean variable
Election_df_wide <- Election_df_wide %>%
  mutate(dem_lean = dem_votes_percent - rep_votes_percent)

#correcting DC name and state_po
Election_df_wide <- Election_df_wide %>%
  mutate(
    state = if_else(state == "District Of Columbia", "District of Columbia", state),
    state_po = if_else(state == "District of Columbia", "DC", state_po))


write_rds(Election_df_wide, "~/Wageningen/University/Thesis/Data/1. Modified Data/Election_data_by_state_2008-2024.rds")

#creating dem_leans_df
dem_leans_df <- Election_df_wide %>%
  select(year, state, state_po, dem_lean)


##Linear interpolation of values for years between election years

#removing totals rows
dem_leans_df <- dem_leans_df %>%
  filter(state != "Totals")

#creating full years dataset
full_years <- expand.grid(
  state = unique(dem_leans_df$state),
  year = 2008:2024)

dem_leans_complete <- full_years %>%
  left_join(dem_leans_df, by = c("state", "year"))

#linear interpolation
dem_leans_complete <- dem_leans_complete %>%
  group_by(state) %>%
  arrange(year) %>%
  mutate(dem_lean = approx(year, dem_lean, year, method = "linear", rule = 2)$y) %>%
  ungroup()

#fixing state_po
dem_leans_complete <- dem_leans_complete %>%
  left_join(dem_leans_df %>% select(state, state_po) %>% distinct(), by = "state")

dem_leans_complete <- dem_leans_complete %>%
  select(state, state_po.y, year, dem_lean) %>%
  rename(state_po = state_po.y)

write_rds(dem_leans_complete, "~/Wageningen/University/Thesis/Data/1. Modified Data/dem_leans_complete.rds")


