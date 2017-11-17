
# Intitialize and Import --------------------------------------------------

library(tidycensus)
library(tidyverse)
library(haven)

# vars <- load_variables(2014, "acs5")
# census_api_key("api key", install = TRUE)


# Query Census API --------------------------------------------------------

acs_data <- get_acs(
  geography = "block group",
  year = 2011,
  variables = c(
    "B17017_001E", # pov household (denom)
    "B17017_002E", # below pov household
    "B19057_001E", # pub assistance (denom)
    "B19057_002E", # with pub assistance
    "B11001_002E", # fhh (denom)
    "B11001_006E", # fhh
    "B99051_001E", # citizenship stat (denom)
    "B99051_005E", # foreign born
    "B25003_001E", # tenure (denom)
    "B25003_003E", # tenure renter occupied
    "B25002_001E", # property (denom)
    "B25002_003E", # vacant property
    "B23025_003E", # civil labor force (denom)
    "B23025_005E" # unemployed
    ),
  state = "26",
  county = "049",
  output = "wide",
  survey = "acs5"
)


# Clean ACS Output --------------------------------------------------------

acs_data <- acs_data %>%
  select(contains("E")) %>%
  mutate(
    pct_povt = B17017_002E / B17017_001E * 100,
    pct_puba = B19057_002E / B19057_001E * 100,
    pct_rent = B25003_003E / B25003_001E * 100,
    pct_vact = B25002_003E / B25002_001E * 100,
    pct_ffhh = B11001_006E / B11001_002E * 100,
    pct_unem = B23025_005E / B23025_003E * 100,
    pct_fobo = B99051_005E / B99051_001E * 100
  ) %>%
  select(NAME:pct_fobo)


# Create Neighborhood Disadvantage ----------------------------------------

acs_data$nbhood_disadv <- acs_data[3:8] %>%
  lapply(scale) %>% 
  as.data.frame() %>%
  rowMeans()


# Calculate Block Group-Study Condition Overlaps --------------------------

bgcep <- read.table("U:/HBHE/PRC/GIS/Projects/CPTED/Block Group- Condition Overlap/blockgroup-cpted-overlap.txt", header = T, sep = ",")
bgbyr <- read.table("U:/HBHE/PRC/GIS/Projects/CPTED/Block Group- Condition Overlap/blockgroup-byrne-overlap.txt", header = T, sep = ",")
bgcom <- read.table("U:/HBHE/PRC/GIS/Projects/CPTED/Block Group- Condition Overlap/blockgroup-comp-overlap.txt", header = T, sep = ",")

# bgcep
bgcep <- bgcep %>%
  select(GEOID10, Area_1, Area_2, Area_OL) %>%
  mutate(
    bg_cep_frac = Area_OL/Area_2,
    GEOID = as.character(GEOID10)
  ) %>%
  filter(
    bg_cep_frac != Inf,
    !is.na(GEOID)
  )

# bgbyr
bgbyr <- bgbyr %>%
  select(GEOID10, Area_1, Area_2, Area_OL) %>%
  mutate(
    Area_2_tot = sum(unique(Area_2)),
    bg_byr_frac = Area_OL/Area_2_tot,
    GEOID = as.character(GEOID10)
  ) %>%
  filter(
    bg_byr_frac != Inf,
    Area_2 != 0,
    !is.na(GEOID)
  )

# bgcom
bgcom <- bgcom %>%
  select(GEOID10, Area_1, Area_2, Area_OL) %>%
  mutate(
    bg_com_frac = Area_OL/Area_2,
    GEOID = as.character(GEOID10)
  ) %>%
  filter(
    bg_com_frac != Inf,
    !is.na(GEOID)
  )


# Scale Neighborhood Disadvantage -----------------------------------------

scaled_nd <- acs_data %>%
  select(GEOID, nbhood_disadv) %>%
  left_join(bgcep %>% select(GEOID, bg_cep_frac), by = "GEOID") %>%
  left_join(bgbyr %>% select(GEOID, bg_byr_frac), by = "GEOID") %>%
  left_join(bgcom %>% select(GEOID, bg_com_frac), by = "GEOID")

scaled_nd <- filter(scaled_nd, !is.na(nbhood_disadv))
scaled_nd[is.na(scaled_nd)] <- 0

weighted.mean(scaled_nd$nbhood_disadv, scaled_nd$bg_cep_frac)
weighted.mean(scaled_nd$nbhood_disadv, scaled_nd$bg_byr_frac)
weighted.mean(scaled_nd$nbhood_disadv, scaled_nd$bg_com_frac)


# Pull in PMOT Data -------------------------------------------------------

pmot_2014 <- read_spss("U:/HBHE/PRC/Projects/Flint Byrne/Data/Parcel Assessments/2014 Parcel Assessments/Cleaned Data/Cleaning_In_Progress/PMOT_Data_2014_All_Cases_Joined_Correct_Map_N_6457_Final_Cleaned_File.sav")
pmot_2015 <- read_spss("U:/HBHE/PRC/Projects/Flint Byrne/Data/Parcel Assessments/2015 Parcel Assessments/Cleaned Data/Data Cleaning In Progress/2015_PMOT_Data_CPTED_Byrne_Comparison_w_Reliability_Parcels_Corrected_Map_N6457_Final_Cleaned_File.sav")
pmot_2016 <- read_spss("U:/HBHE/PRC/Projects/Flint Byrne/Data/Parcel Assessments/2016 Parcel Assessments/Cleaned Data/Data Cleaning In Progress/2016_PMOT_CPTED_Byrne_Comparison_&_Reliability_Spatial_Data_Final_Cleaned_File.sav")
pmot_2017 <- read_spss("U:/HBHE/PRC/Projects/Flint Byrne/Data/Parcel Assessments/2017 Parcel Assessments/Cleaning/Byrne17_PMOT_20170922_Cleaning_v1.sav")

# 2014
pmot_2014 <- pmot_2014 %>%
  mutate(
    GEOID  = as.character(Study_Condition),
    vacant = ifelse(ParcelType == 3, TRUE, FALSE)
  ) %>%
  group_by(GEOID) %>%
  summarise(
    vacancies = sum(vacant),
    tot_parcels = n(),
    pct_vacant_2014 = vacancies/tot_parcels * 100
  ) %>%
  select(GEOID, pct_vacant_2014)

# 2015
pmot_2015 <- pmot_2015 %>%
  mutate(
    GEOID  = as.character(Study_Condition),
    vacant = ifelse(ParcelType == 3, TRUE, FALSE)
  ) %>%
  group_by(GEOID) %>%
  summarise(
    vacancies = sum(vacant),
    tot_parcels = n(),
    pct_vacant_2015 = vacancies/tot_parcels * 100
  ) %>%
  select(GEOID, pct_vacant_2015)

# 2016
pmot_2016 <- pmot_2016 %>%
  mutate(
    GEOID  = as.character(Study_Condition),
    vacant = ifelse(Permanent_Structure == 0, TRUE, FALSE)
  ) %>%
  group_by(GEOID) %>%
  summarise(
    vacancies = sum(vacant),
    tot_parcels = n(),
    pct_vacant_2016 = vacancies/tot_parcels * 100
  ) %>%
  select(GEOID, pct_vacant_2016)

# 2017
pmot_2017 <- pmot_2017 %>%
  mutate(
    GEOID  = as.character(Study_Condition),
    vacant = ifelse(Branching_Logic_0 == 2, TRUE, FALSE)
  ) %>%
  group_by(GEOID) %>%
  summarise(
    vacancies = sum(vacant),
    tot_parcels = n(),
    pct_vacant_2017 = vacancies/tot_parcels * 100
  ) %>%
  select(GEOID, pct_vacant_2017)

pmot <- pmot_2014 %>%
  left_join(pmot_2015, by = "GEOID") %>%
  left_join(pmot_2016, by = "GEOID") %>%
  left_join(pmot_2017, by = "GEOID")
