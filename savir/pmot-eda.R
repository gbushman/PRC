
# initialize and import ---------------------------------------------------

# libraries
library(tidyverse)
library(magrittr)
library(readxl)
library(haven)
library(sf)
library(lubridate)
library(tidycensus)
library(lmerTest)

# relevant crimes
relevant_crimes <- read_excel("C:/Users/gbushman/Documents/Projects/pmot/crime-codes.xlsx") %>%
  filter(relevance == 1) %$%
  offense_codes

# gun incidences
weapon_type_guns = c("11", "12", "13", "14", "15", "11A", "12A", "13A", "14A", "15A")

# cleaned crime data
crime16 <- read_spss("C:/Users/gbushman/Box/YVPC Crime Data/Flint/FL16_Crime_Data_20180321_Cleaned_v1.sav") %>% zap_labels()
crime15 <- read_spss("C:/Users/gbushman/Box/YVPC Crime Data/Flint/FL15_Crime_Data_20180321_Cleaned_v1.sav") %>% zap_labels()
mic15 <- read_excel("U:/HBHE/PRC/Projects/YVPC 2000-2005/Crime Data/Original Crime Data Files/2013-2015 Orginal Text Files from MSP & MSU/2015 files/Excel Versions/MICR1 2015.xlsx")
off15 <- read_excel("U:/HBHE/PRC/Projects/YVPC 2000-2005/Crime Data/Original Crime Data Files/2013-2015 Orginal Text Files from MSP & MSU/2015 files/Excel Versions/MICR OFFNS 2015.xlsx")

# flint boundary
flint_sf <- st_read("C:/Users/gbushman/Documents/Projects/flint-roads/Flint Roads/Flint Roads.gdb", "Flint_Boundary")
flint_sf <- st_transform(flint_sf, crs = 6498)

# parcel layer
parcels_sf <- read_sf("U:/HBHE/PRC/GIS/Projects/MI-YVPC 2015-2020/Base Data/2018/Flint_MI_2018.gdb", "FL18_MIYVPC_Parcels_v11")
parcels_sf <- st_transform(parcels_sf, crs = 6498)


# 2016 crime data ---------------------------------------------------------

# filter to relevant crimes
# clean up 2016 crime data, pull out relevant columns
# create indicator for gun crimes
crime16 <- crime16 %>%
  mutate(MIC1_INCIDENTNO = as.character(MIC1_INCIDENTNO)) %>%
  filter(OFFNS_OFFENSE_CO %in% as.numeric(relevant_crimes)) %>%
  select(
    inc_number = MIC1_INCIDENTNO,
    long = Crime_Incident_Longitude,
    lat = Crime_Incident_Latitude,
    weapon = OFFNS_WEAPON,
    date = MIC1_INC_DATE
  ) %>%
  mutate(
    gun_crime = FALSE,
    gun_crime = ifelse(weapon %in% weapon_type_guns == T & !weapon %in% c(NA, ""), TRUE, gun_crime),
    date = as.Date(date)
  ) %>% 
  distinct()

colnames(crime16) <- tolower(colnames(crime16))


# 2015 crime data ---------------------------------------------------------

# filter to relevant crimes
# clean up 2015 crime data, pull out relevant columns
# create indicator for gun crimes
crime15 <- crime15 %>%
  mutate(INCIDENT_NUMBER = as.character(INCIDENT_NUMBER)) %>%
  left_join(mic15 %>% transmute(INCIDENT_NUMBER = as.character(MIC1_INCIDENTNO), MIC1_NUMBER = as.character(MIC1_NUMBER))) %>%
  left_join(off15 %>% transmute(MIC1_NUMBER = as.character(MIC1_NUMBER), OFFNS_OFFENSE_CO = as.character(OFFNS_OFFENSE_CO))) %>% 
  filter(OFFNS_OFFENSE_CO %in% relevant_crimes) %>%
  select(
    inc_number = INCIDENT_NUMBER,
    lat = Crime_Incident_Longitude,
    long = Crime_Incident_Latitude,
    weapon = OFFNS_WEAPON,
    date = MIC1_INC_DATE
  ) %>%
  mutate(
    weapon = as.character(weapon),
    gun_crime = FALSE,
    gun_crime = ifelse(weapon %in% weapon_type_guns == T & !weapon %in% c(NA, ""), TRUE, gun_crime),
    date = as.character(date),
    date = ifelse(nchar(date) == 5, paste0("0", date), date),
    date = as.Date(date, format = "%m%d%y")
  ) %>% 
  distinct()

colnames(crime15) <- tolower(colnames(crime15))


# aggregate crime data ----------------------------------------------------

crimes <- bind_rows(crime15, crime16) %>%
  filter(!is.na(inc_number))

# create spatial (simple features) data frame
crimes_sf <- crimes %>%
  filter(long != "", lat != "") %>%
  mutate(
    long = as.numeric(long),
    lat = as.numeric(lat)
  ) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  st_transform(crs = 6498)

# map to parcel layer -----------------------------------------------------

# pull out intervention parcels
int_parcels <- parcels_sf %>%
  filter(Intervention_Parcel_Year %in% c(2016,2017), Intervention_Parcel == 1) %>%
  select(
    parcel_id = Parcel_ID,
    # Community_Engagement_2017,
    # Professional_Mow_2017,
    # NoTreatment_2017,
    Community_Engagement_2016,
    Professional_Mow_2016,
    NoTreatment_2016
  ) %>%
  gather(key = "study_condition", value = "value", -c(parcel_id, Shape)) %>%
  mutate(
    condition_year = as.numeric(str_extract(study_condition, "\\d{4}")),
    study_condition = str_remove_all(study_condition, "_|\\d")
  ) %>%
  filter(value == 1) %>%
  select(-value)

# find centroids
int_parcel_cents <- st_centroid(int_parcels)

# create buffers around centroids
int_parcel_buffs <- st_buffer(int_parcel_cents, dist = 150)

# spatial join crime and buffer data
# filter crimes to those that fall within buffers
# create time group variable based on year and month
study_crimes <- st_join(crimes_sf, int_parcel_buffs) %>%
  filter(!is.na(study_condition)) %>%
  mutate(
    year = year(date),
    trimester = NA,
    trimester = ifelse(month(date) %in% c(1:4), 0/3, trimester),
    trimester = ifelse(month(date) %in% c(5:8), 1/3, trimester),
    trimester = ifelse(month(date) %in% c(9:12), 2/3, trimester),
    trimester = year + trimester,
    quarter = NA,
    quarter = ifelse(month(date) %in% c(1:3), 0/4, quarter),
    quarter = ifelse(month(date) %in% c(4:6), 1/4, quarter),
    quarter = ifelse(month(date) %in% c(7:9), 2/4, quarter),
    quarter = ifelse(month(date) %in% c(10:12), 3/4, quarter),
    quarter = year + quarter
  )


# get census data for controls --------------------------------------------

# census api query
# pull neighborhood disadvantage variables for
# Genesee County block groups, from 2017 
acs_data <- get_acs(
  geography = "block group",
  year = 2016,
  variables = c(
    "B17017_001E", # pov household (denom)
    "B17017_002E", # below pov household
    "B19057_001E", # pub assistance (denom)
    "B19057_002E", # with pub assistance
    #"B11001_002E", # fhh (denom)
    #"B11001_006E", # fhh
    #"B99051_001E", # citizenship stat (denom)
    #"B99051_005E", # foreign born
    "B25003_001E", # tenure (denom)
    "B25003_003E", # tenure renter occupied
    "B25002_001E", # property (denom)
    "B25002_003E" # vacant property
  ),
  state = "26",
  county = "049",
  output = "wide",
  survey = "acs5"
)

# create neighborhood disadvantage factors
acs_data <- acs_data %>%
  select(contains("E")) %>%
  mutate(
    pct_povt = B17017_002E / B17017_001E * 100,
    pct_puba = B19057_002E / B19057_001E * 100,
    pct_rent = B25003_003E / B25003_001E * 100,
    pct_vact = B25002_003E / B25002_001E * 100 #,
    #pct_ffhh = B11001_006E / B11001_002E * 100,
    #pct_fobo = B99051_005E / B99051_001E * 100
  ) %>%
  select(GEOID, pct_povt:pct_vact)

# create neighborhood disadvantage index
acs_data$nbhood_disadv <- acs_data[2:5] %>%
  lapply(scale) %>% 
  as.data.frame() %>%
  rowMeans()

# census api query
# pull total population estimate variable for
# Genesee County block groups, from 2017 
acs_pop <- get_acs(
  geography = "block group",
  year = 2016,
  variables = c("B01003_001E"),
  state = "26",
  county = "049",
  output = "wide",
  survey = "acs5",
  geometry = TRUE
) %>% 
  rename(tot_pop = B01003_001E) %>%
  select(GEOID, tot_pop, geometry)

# merge the population and disadvantage data
flint_acs <- acs_pop %>%
  left_join(acs_data %>% select(GEOID, nbhood_disadv)) %>%
  st_transform(crs = 6498) %>%
  filter(
    st_intersects(., flint_sf, sparse = FALSE),
    GEOID != "260499801001"
  ) %>%
  mutate(bg_area = st_area(geometry))


# calculate intersections of buffers and block groups ---------------------

acs_by_parcel <- as_tibble(st_intersection(flint_acs, int_parcel_buffs)) %>%
  select(GEOID, bg_area, tot_pop, nbhood_disadv, parcel_id, geometry) %>%
  mutate(intersection_area = st_area(geometry)) %>%
  group_by(parcel_id) %>%
  mutate(
    intersection_area = as.numeric(intersection_area),
    buffer_area = sum(intersection_area)
  ) %>%
  ungroup() %>%
  mutate(
    bg_area       = as.numeric(bg_area),
    nbdis_pct_int = intersection_area / buffer_area,
    pop_pct_int   = intersection_area / bg_area,
    scaled_nbdis  = nbhood_disadv * nbdis_pct_int,
    scaled_pop    = tot_pop * pop_pct_int
  ) %>%
  as.data.frame() %>%
  select(-geometry) %>%
  group_by(parcel_id) %>%
  summarise(
    nbhood_disadv = sum(scaled_nbdis),
    total_pop = sum(scaled_pop)
  )


# crime analysis ----------------------------------------------------------

# visualization
# aggregated treatment conditions
study_crimes %>%
  as.data.frame() %>%
  mutate(study_condition_agg = ifelse(study_condition == "ProfessionalMow" | study_condition == "CommunityEngagement", "Treatment", study_condition)) %>%
  count(parcel_id, trimester, study_condition_agg) %>%
  ggplot(aes(x = trimester, y = n, color = study_condition_agg)) +
  geom_smooth(method = "lm", se = F) +
  coord_cartesian(ylim = c(0, 10))

# visualization
# disaggregated treatment conditions
study_crimes %>%
  as.data.frame() %>%
  count(parcel_id, trimester, study_condition) %>%
  ggplot(aes(x = trimester, y = n, color = study_condition)) +
  geom_smooth(method = "lm", se = F) +
  coord_cartesian(ylim = c(0, 10))

# visualization
# disaggregated treatment conditions
study_crimes %>%
  as.data.frame() %>%
  count(parcel_id, trimester, study_condition) %>%
  ungroup() %>%
  left_join(acs_by_parcel, by = "parcel_id") %>%
  mutate(crime_rate = n / total_pop * 1000) %>%
  ggplot(aes(x = trimester, y = crime_rate, color = study_condition)) +
  geom_smooth(method = "lm", se = F) +
  coord_cartesian(ylim = c(0, 80)) +
  scale_x_continuous(breaks = seq(2015, 2017, by = 1/3))

# analysis, MLM / repeated measures ANOVA
# crime rate ~ study condition (disaggregated) + time (quarter)
model_crimes_df <- study_crimes %>% 
  as.data.frame() %>%
  count(trimester, study_condition, parcel_id) %>% 
  ungroup() %>%
  left_join(acs_by_parcel, by = "parcel_id") %>%
  mutate(
    crime_rate = n / total_pop * 1000,
    time = trimester - 2015,
    study_condition = as.factor(study_condition),
    study_condition = relevel(study_condition, ref = "NoTreatment")
  )

model_crimes <- lmer(
  crime_rate ~ 
    time + 
    study_condition + 
    time*study_condition + 
    total_pop + 
    nbhood_disadv + 
    (1 | parcel_id), 
  data = model_crimes_df, 
  REML = FALSE
)

summary(model_crimes)


# nls data ----------------------------------------------------------------

# NLS data
nls16 <- read_spss("U:/HBHE/PRC/Projects/YVPC 2015-2020/Data Management/NLS/Flint/Cleaned/SAVIR Analysis/FL16_NLS_20181220_Merged_GeoFull_v5.sav") %>% zap_labels()
nls17 <- read_spss("U:/HBHE/PRC/Projects/YVPC 2015-2020/Data Management/NLS/Flint/Cleaned/SAVIR Analysis/FL17_NLS_20171030_Merged_GeoFull_v4.sav") %>% zap_labels()
nls18 <- read_spss("U:/HBHE/PRC/Projects/YVPC 2015-2020/Data Management/NLS/Flint/Cleaned/SAVIR Analysis/FL18_NLS_20181119_Merged_GeoFull_v5.sav") %>% zap_labels()

# 2016 data
nls16 <- nls16 %>%
  select(
    Survey_ID, 
    MIYVPC_Parcel_ID,
    Study_Site_Original,
    Study_Site_2016_A_Dist,
    Community_Engagement_2016, 
    Professional_Mow_2016,
    NoTreatment_2016, 
    FL_16_PRE_Fear_crime,
    FL_16_PRE_Compare_crime,
    FL_16_PRE_Walk_dark,
    FL_16_PRE_Walk_day,
    FL_16_POST_Fear_crime,
    FL_16_POST_Compare_crime,
    FL_16_POST_Walk_dark,
    FL_16_POST_Walk_day,
    FL_16_1YRFU_Fear_crime,
    FL_16_1YRFU_Compare_crime,
    FL_16_1YRFU_Walk_dark,
    FL_16_1YRFU_Walk_day,
    FL_16_PRE_Neigh_help,
    FL_16_PRE_Neigh_trust,
    FL_16_PRE_Neigh_know,
    FL_16_PRE_Neigh_children,
    FL_16_POST_Neigh_help,
    FL_16_POST_Neigh_trust,
    FL_16_POST_Neigh_know,
    FL_16_POST_Neigh_children,
    FL_16_1YRFU_Neigh_help,
    FL_16_1YRFU_Neigh_trust,
    FL_16_1YRFU_Neigh_know,
    FL_16_PRE_Neigh_children,
    FL_16_PRE_Injury,
    FL_16_PRE_Victim_crime,
    FL_16_POST_Injury,
    FL_16_POST_Victim_crime,
    FL_16_1YRFU_Injury,
    FL_16_1YRFU_Victim_crime
  ) %>%
  mutate(
    treatment = NA,
    treatment = ifelse(Community_Engagement_2016 == 1, "CE", treatment),
    treatment = ifelse(Professional_Mow_2016 == 1, "PM", treatment),
    treatment = ifelse(NoTreatment_2016 == 1, "NT", treatment),
    tx_agg    = NA,
    tx_agg    = ifelse(treatment %in% c("CE", "PM") & !is.na(treatment), "Tx", tx_agg),
    tx_agg    = ifelse(treatment == "NT" & !is.na(treatment), "NT", tx_agg)
  ) %>%
  select(-Community_Engagement_2016, -Professional_Mow_2016, -NoTreatment_2016) %>%
  gather(var, val, -Survey_ID, -MIYVPC_Parcel_ID, -treatment, -tx_agg, -Study_Site_Original, -Study_Site_2016_A_Dist) %>%
  mutate(
    year = str_extract(var, "\\d{2}"),
    pre_post = str_extract(var, "PRE|POST|1YRFU"),
    var = str_remove_all(var, "FL_16_[[:alnum:]]+_"),
    val = as.numeric(val)
  ) %>% 
  spread(var, val) %>%
  mutate(
    victimization = Injury + Victim_crime,
    fear_of_crime = (scale(Fear_crime)[ ,1] + scale(Compare_crime)[ ,1] + scale(Walk_day)[ ,1] + scale(Walk_dark)[ ,1]) / 4,
    soccapco      = (Neigh_help + Neigh_trust + Neigh_know + Neigh_children) / 4,
    mental_health = NA
  ) %>%
  select(-c(Compare_crime:Walk_day))

# 2017 data
nls17 <- nls17 %>%
  select(
    Survey_ID, 
    MIYVPC_Parcel_ID,
    Study_Site_Original,
    Study_Site_2017_A_Dist,
    Community_Engagement_2017, 
    Professional_Mow_2017,
    NoTreatment_2017, 
    FL_17_PRE_Fear_crime,
    FL_17_PRE_Compare_crime,
    FL_17_PRE_Walk_dark,
    FL_17_PRE_Walk_day,
    FL_17_POST_Fear_crime,
    FL_17_POST_Compare_crime,
    FL_17_POST_Walk_dark,
    FL_17_POST_Walk_day,
    FL_17_1YRFU_Fear_crime,
    FL_17_1YRFU_Compare_crime,
    FL_17_1YRFU_Walk_dark,
    FL_17_1YRFU_Walk_day,
    FL_17_PRE_Neigh_help,
    FL_17_PRE_Neigh_trust,
    FL_17_PRE_Neigh_know,
    FL_17_PRE_Neigh_children,
    FL_17_POST_Neigh_help,
    FL_17_POST_Neigh_trust,
    FL_17_POST_Neigh_know,
    FL_17_POST_Neigh_children,
    FL_17_1YRFU_Neigh_help,
    FL_17_1YRFU_Neigh_trust,
    FL_17_1YRFU_Neigh_know,
    FL_17_PRE_Neigh_children,
    FL_17_PRE_Injury,
    FL_17_PRE_Victim_crime,
    FL_17_POST_Injury,
    FL_17_POST_Victim_crime,
    FL_17_1YRFU_Injury,
    FL_17_1YRFU_Victim_crime,
    FL_17_PRE_Upset,
    FL_17_PRE_Nervous,
    FL_17_PRE_Problems,
    FL_17_PRE_Lonely,
    FL_17_PRE_Sad,
    FL_17_PRE_No_interest,
    FL_17_POST_Upset,
    FL_17_POST_Nervous,
    FL_17_POST_Problems,
    FL_17_POST_Lonely,
    FL_17_POST_Sad,
    FL_17_POST_No_interest,
    FL_17_1YRFU_Upset,
    FL_17_1YRFU_Nervous,
    FL_17_1YRFU_Problems,
    FL_17_1YRFU_Lonely,
    FL_17_1YRFU_Sad,
    FL_17_1YRFU_No_interest
  ) %>%
  mutate(
    treatment = NA,
    treatment = ifelse(Community_Engagement_2017 == 1, "CE", treatment),
    treatment = ifelse(Professional_Mow_2017 == 1, "PM", treatment),
    treatment = ifelse(NoTreatment_2017 == 1, "NT", treatment),
    tx_agg    = NA,
    tx_agg    = ifelse(treatment %in% c("CE", "PM") & !is.na(treatment), "Tx", tx_agg),
    tx_agg    = ifelse(treatment == "NT" & !is.na(treatment), "NT", tx_agg)
  ) %>%
  select(-Community_Engagement_2017, -Professional_Mow_2017, -NoTreatment_2017) %>%
  gather(var, val, -Survey_ID, -MIYVPC_Parcel_ID, -treatment, -tx_agg, -Study_Site_Original, -Study_Site_2017_A_Dist) %>%
  mutate(
    year = str_extract(var, "\\d{2}"),
    pre_post = str_extract(var, "PRE|POST|1YRFU"),
    var = str_remove_all(var, "FL_17_[[:alnum:]]+_"),
    val = as.numeric(val)
  ) %>% 
  spread(var, val) %>%
  mutate(
    victimization = Injury + Victim_crime,
    fear_of_crime = (scale(Fear_crime)[ ,1] + scale(Compare_crime)[ ,1] + scale(Walk_day)[ ,1] + scale(Walk_dark)[ ,1]) / 4,
    soccapco      = (Neigh_help + Neigh_trust + Neigh_know + Neigh_children) / 4,
    mental_health = (Upset + Nervous + Problems + Lonely + Sad + No_interest) / 6
  ) %>%
  select(-c(Compare_crime:Walk_day))

# 2018 data
nls18 <- nls18 %>%
  select(
    Survey_ID, 
    MIYVPC_Parcel_ID,
    Study_Site_Original,
    Study_Site_2018_A_Dist,
    Community_Engagement_2018, 
    Professional_Mow_2018,
    NoTreatment_2018, 
    FL_18_PRE_Fear_crime,
    FL_18_PRE_Compare_crime,
    FL_18_PRE_Walk_dark,
    FL_18_PRE_Walk_day,
    FL_18_POST_Fear_crime,
    FL_18_POST_Compare_crime,
    FL_18_POST_Walk_dark,
    FL_18_POST_Walk_day,
    FL_18_PRE_Neigh_help,
    FL_18_PRE_Neigh_trust,
    FL_18_PRE_Neigh_know,
    FL_18_PRE_Neigh_children,
    FL_18_POST_Neigh_help,
    FL_18_POST_Neigh_trust,
    FL_18_POST_Neigh_know,
    FL_18_POST_Neigh_children,
    FL_18_PRE_Neigh_children,
    FL_18_PRE_Injury,
    FL_18_PRE_Victim_crime,
    FL_18_POST_Injury,
    FL_18_POST_Victim_crime,
    FL_18_PRE_Upset,
    FL_18_PRE_Nervous,
    FL_18_PRE_Problems,
    FL_18_PRE_Lonely,
    FL_18_PRE_Sad,
    FL_18_PRE_No_interest,
    FL_18_POST_Upset,
    FL_18_POST_Nervous,
    FL_18_POST_Problems,
    FL_18_POST_Lonely,
    FL_18_POST_Sad,
    FL_18_POST_No_interest
  ) %>%
  mutate(
    treatment = NA,
    treatment = ifelse(Community_Engagement_2018 == 1, "CE", treatment),
    treatment = ifelse(Professional_Mow_2018 == 1, "PM", treatment),
    treatment = ifelse(NoTreatment_2018 == 1, "NT", treatment),
    tx_agg    = NA,
    tx_agg    = ifelse(treatment %in% c("CE", "PM") & !is.na(treatment), "Tx", tx_agg),
    tx_agg    = ifelse(treatment == "NT" & !is.na(treatment), "NT", tx_agg)
  ) %>%
  select(-Community_Engagement_2018, -Professional_Mow_2018, -NoTreatment_2018) %>%
  gather(var, val, -Survey_ID, -MIYVPC_Parcel_ID, -treatment, -tx_agg, -Study_Site_Original, Study_Site_2018_A_Dist) %>%
  mutate(
    year = str_extract(var, "\\d{2}"),
    pre_post = str_extract(var, "PRE|POST|1YRFU"),
    var = str_remove_all(var, "FL_18_[[:alnum:]]+_"),
    val = as.numeric(val)
  ) %>% 
  spread(var, val) %>%
  mutate(
    victimization = Injury + Victim_crime,
    fear_of_crime = (scale(Fear_crime)[ ,1] + scale(Compare_crime)[ ,1] + scale(Walk_day)[ ,1] + scale(Walk_dark)[ ,1]) / 4,
    soccapco      = (Neigh_help + Neigh_trust + Neigh_know + Neigh_children) / 4,
    mental_health = (Upset + Nervous + Problems + Lonely + Sad + No_interest) / 6
  ) %>%
  select(-c(Compare_crime:Walk_day))

# combine data
nls <- bind_rows(nls16, nls17, nls18) %>%
  mutate(
    year = as.numeric(year),
    treatment = factor(treatment, levels = c("NT", "CE", "PM")),
    pre_post = factor(pre_post, levels = c("PRE", "POST", "1YRFU")),
    pre_post_num = as.numeric(pre_post)
  ) %>%
  filter(
    !is.na(treatment),
    pre_post != "1YRFU"
  )


# nls analysis ------------------------------------------------------------

# visualization
nls %>%
  gather(var, val, c(victimization:mental_health)) %>%
  group_by(pre_post, var, treatment) %>%
  summarise(val = mean(val, na.rm = T)) %>%
  ungroup() %>%
  ggplot(aes(x = pre_post, y = val, group = treatment)) +
  geom_point(aes(color = treatment)) +
  geom_line(aes(color = treatment)) +
  facet_grid(. ~ var)

# Social capital and cohesion
# Two-Way Anova, assumes independent samples
lm(
  soccapco ~ 
    pre_post * treatment, 
  data = nls
) %>% 
  aov() %>% 
  summary()

# Repeated measures ANOVA, assumes dependent samples
soccapco <- nls %>% 
  group_by(Survey_ID) %>% 
  mutate(one_waves = any(is.na(soccapco))) %>% 
  ungroup() %>% 
  filter(one_waves == FALSE)

lmer(
  soccapco ~ 
    pre_post_num + 
    treatment + 
    pre_post_num * treatment + 
    (1 | Survey_ID), 
  data = soccapco, 
  REML = FALSE
) %>% 
  Anova()

# MLM
lmer(
  soccapco ~ 
    pre_post_num + 
    treatment + 
    pre_post_num * treatment + 
    (1 + pre_post_num | Study_Site_Original), 
  data = nls, 
  REML = FALSE
) %>% 
  summary()

#Fear of crime
# Two-Way Anova, assumes independent samples
lm(
  fear_of_crime ~ 
    pre_post * treatment, 
  data = nls
) %>% 
  aov() %>% 
  summary()

# Repeated measures ANOVA, assumes dependent samples
foc <- nls %>% 
  group_by(Survey_ID) %>% 
  mutate(one_waves = any(is.na(fear_of_crime))) %>% 
  ungroup() %>% 
  filter(one_waves == FALSE)

lmer(
  fear_of_crime ~ 
    pre_post_num + 
    treatment + 
    pre_post_num * treatment + 
    (1 | Survey_ID), 
  data = foc, 
  REML = FALSE
) %>% 
  Anova()

# MLM
lmer(
  fear_of_crime ~ 
    pre_post_num + 
    treatment + 
    pre_post_num * treatment + 
    (1 + pre_post_num | Study_Site_Original), 
  data = nls, 
  REML = FALSE
) %>%
summary()

# Victimization
# Two-Way Anova, assumes independent samples
lm(
  victimization ~ 
    pre_post * treatment, 
  data = nls
) %>% 
  aov() %>% 
  summary()

# Repeated measures ANOVA, assumes dependent samples
victimization <- nls %>% 
  group_by(Survey_ID) %>% 
  mutate(one_waves = any(is.na(victimization))) %>% 
  ungroup() %>% 
  filter(one_waves == FALSE)

lmer(
  victimization ~ 
    pre_post_num + 
    treatment + 
    pre_post_num * treatment + 
    (1 | Survey_ID), 
  data = victimization, 
  REML = FALSE
) %>% 
  Anova()

# MLM
lmer(
  victimization ~ 
    pre_post_num + 
    treatment + 
    pre_post_num * treatment + 
    (1 + pre_post_num | Study_Site_Original), 
  data = nls, 
  REML = FALSE
) %>%
  summary()

# Mental Health
# Two-Way Anova, assumes independent samples
lm(
  mental_health ~ 
    pre_post * treatment, 
  data = nls
) %>% 
  aov() %>% 
  summary()

# Repeated measures ANOVA, assumes dependent samples
mental_health <- nls %>% 
  group_by(Survey_ID) %>% 
  mutate(one_waves = any(is.na(mental_health))) %>% 
  ungroup() %>% 
  filter(one_waves == FALSE)

lmer(
  mental_health ~ 
    pre_post_num + 
    treatment + 
    pre_post_num * treatment + 
    (1 | Survey_ID), 
  data = mental_health, 
  REML = FALSE
) %>% 
  Anova()

# MLM
model_mh <- lmer(
  mental_health ~ 
    pre_post_num + 
    treatment + 
    pre_post_num * treatment + 
    (1 + pre_post_num | Study_Site_Original), 
  data = nls, 
  REML = FALSE
)

summary(model_mh)
