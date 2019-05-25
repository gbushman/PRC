
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
  filter(community == 1) %$%
  offense_codes

# gun incidences
weapon_type_guns = c("11", "12", "13", "14", "15", "11A", "12A", "13A", "14A", "15A")

# cleaned crime data
## 2017
crime17 <- read_spss("C:/Users/gbushman/Box/YVPC Crime Data/Flint/FL17_Crime_Data_20190128_Cleaned_v1.sav") %>% zap_labels()
## 2016
crime16 <- read_spss("C:/Users/gbushman/Box/YVPC Crime Data/Flint/FL16_Crime_Data_20180321_Cleaned_v1.sav") %>% zap_labels()
## 2015
crime15 <- read_spss("C:/Users/gbushman/Box/YVPC Crime Data/Flint/FL15_Crime_Data_20180321_Cleaned_v1.sav") %>% zap_labels()
mic15 <- read_excel("U:/HBHE/PRC/Projects/YVPC 2000-2005/Crime Data/Original Crime Data Files/2013-2015 Orginal Text Files from MSP & MSU/2015 files/Excel Versions/MICR1 2015.xlsx")
off15 <- read_excel("U:/HBHE/PRC/Projects/YVPC 2000-2005/Crime Data/Original Crime Data Files/2013-2015 Orginal Text Files from MSP & MSU/2015 files/Excel Versions/MICR OFFNS 2015.xlsx")

# flint boundary
flint_sf <- st_read("C:/Users/gbushman/Documents/Projects/flint-roads/Flint Roads/Flint Roads.gdb", "Flint_Boundary")
flint_sf <- st_transform(flint_sf, crs = 6498)

# parcel layer
parcels_sf <- read_sf("U:/HBHE/PRC/GIS/Projects/MI-YVPC 2015-2020/Base Data/2018/Flint_MI_2018.gdb", "FL18_MIYVPC_Parcels_v11")
parcels_sf <- st_transform(parcels_sf, crs = 6498)


# 2017 crime data ---------------------------------------------------------

# filter to relevant crimes
# clean up 2016 crime data, pull out relevant columns
# create indicator for gun crimes
crime17 <- crime17 %>%
  mutate(MIC1_INCIDENTNO = as.character(MIC1_INCIDENTNO)) %>%
  select(
    inc_number = MIC1_INCIDENTNO,
    long = LONGITUDE,
    lat = LATITUDE,
    weapon = OFFNS_WEAPON,
    date = MIC1_INC_DATE,
    offense = OFFNS_OFFENSE_CO
  ) %>%
  mutate(
    long = as.numeric(long),
    lat = as.numeric(lat),
    gun_crime = FALSE,
    gun_crime = ifelse(weapon %in% weapon_type_guns == T & !weapon %in% c(NA, ""), TRUE, gun_crime),
    date = ifelse(nchar(date) == 5, paste0("0", date), date),
    date = as.Date(date, format = "%m%d%y")
  ) %>% 
  distinct()

colnames(crime17) <- tolower(colnames(crime17))


# 2016 crime data ---------------------------------------------------------

# filter to relevant crimes
# clean up 2016 crime data, pull out relevant columns
# create indicator for gun crimes
crime16 <- crime16 %>%
  mutate(MIC1_INCIDENTNO = as.character(MIC1_INCIDENTNO)) %>%
  select(
    inc_number = MIC1_INCIDENTNO,
    long = Crime_Incident_Longitude,
    lat = Crime_Incident_Latitude,
    weapon = OFFNS_WEAPON,
    date = MIC1_INC_DATE,
    offense = OFFNS_OFFENSE_CO
  ) %>%
  mutate(
    gun_crime = FALSE,
    gun_crime = ifelse(weapon %in% weapon_type_guns == T & !weapon %in% c(NA, ""), TRUE, gun_crime),
    date = as.Date(date),
    offense = as.character(offense)
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
  select(
    inc_number = INCIDENT_NUMBER,
    lat = Crime_Incident_Longitude,
    long = Crime_Incident_Latitude,
    weapon = OFFNS_WEAPON,
    date = MIC1_INC_DATE,
    offense = OFFNS_OFFENSE_CO
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

# bind rows
# drop rows with empty incident numbers
crimes <- bind_rows(crime17, crime16, crime15) %>%
  filter(
    !is.na(inc_number),
    offense %in% relevant_crimes
  ) %>%
  select(-offense) %>%
  distinct()

# drop rows where there is missing geospatial information
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
# filter to parcels that were had a study condition in 2016
int_parcels <- parcels_sf %>%
  filter(Intervention_Parcel_Year %in% c(2016, 2017), Intervention_Parcel == 1) %>%
  select(
    parcel_id = MIYVPC_Parcel_ID,
    Community_Engagement_2017,
    Professional_Mow_2017,
    NoTreatment_2017,
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

# find centroids of 2016 intervention parcels
int_parcel_cents <- st_centroid(int_parcels)

# create 150m buffers around centroids of intervention parcels
int_parcel_buffs <- st_buffer(int_parcel_cents, dist = 150)

# spatial join crime data and intervention parcel buffer data
# filter crimes to those that fall within buffers 
# (i.e., those that are labelled with a study condition)
# create time grouping variables based on year and month
study_crimes <- st_join(crimes_sf, int_parcel_buffs) %>%
  filter(!is.na(study_condition)) %>%
  mutate(
    year = year(date),
    timepoint = year - (condition_year - 1),
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
# Genesee County block groups, from 2016
acs_data <- get_acs(
  geography = "block group",
  year = 2016,
  variables = c(
    "B17017_001E", # pov household (denom)
    "B17017_002E", # below pov household
    "B19057_001E", # pub assistance (denom)
    "B19057_002E", # with pub assistance
    "B25003_001E", # tenure (denom)
    "B25003_003E", # tenure renter occupied
    "B25002_001E", # property (denom)
    "B25002_003E"  # vacant property
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
    pct_vact = B25002_003E / B25002_001E * 100
  ) %>%
  select(GEOID, pct_povt:pct_vact)

# create neighborhood disadvantage index
# z-score each factor
# take mean of z-scores
acs_data$nbhood_disadv <- acs_data[2:5] %>%
  lapply(scale) %>% 
  as.data.frame() %>%
  rowMeans()

# census api query
# pull total population estimate variable for
# Genesee County block groups, from 2017 
# include Flint block group geometry
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
# filter block groups to those that fall within Flint
# calculate the area of each block group
flint_acs <- acs_pop %>%
  left_join(acs_data %>% select(GEOID, nbhood_disadv)) %>%
  st_transform(crs = 6498) %>%
  filter(
    st_intersects(., flint_sf, sparse = FALSE),
    GEOID != "260499801001"
  ) %>%
  mutate(bg_area = st_area(geometry))


# calculate intersections of buffers and block groups ---------------------

# calculate intersection of Flint block groups and intervention parcel buffers
# calculate the land area of each intersecting area
# group by parcel and calculate the area of each buffer zone
# calculate weighted estimates of nbdis and total pop
# convert to plain data frame
# summarise nbdis and total_pop estimates for each intervention parcel area
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

# visualization: 2016 parcels, 2 year viz
# disaggregated treatment conditions
# count all the crimes within unique combos of parcel/trimester/study_condition
# left join on population estimate
# calculate crime rate (per 1,000)
# plot
windowsFonts(Helv = windowsFont("Helvetica"))

study_crimes %>%
  as.data.frame() %>%
  filter(trimester < 2017, condition_year == 2016) %>%
  count(parcel_id, trimester, study_condition) %>%
  ungroup() %>%
  left_join(acs_by_parcel, by = "parcel_id") %>%
  mutate(crime_rate = n / total_pop * 1000) %>%
  ggplot() +
  geom_line(stat = "summary", aes(x = trimester, y = crime_rate, color = study_condition), alpha = 0.75) +
  geom_smooth(aes(x = trimester, y = crime_rate, color = study_condition), method = "lm", se = F, size = 1.1) +
  coord_cartesian(ylim = c(0, 100)) +
  scale_x_continuous(
    breaks = seq(2015, 2016.667, by = 1/3),
    labels = c(
      "Jan-Apr\n2015", "May-Aug\n2015", "Sept-Dec\n2015",
      "Jan-Apr\n2016", "May-Aug\n2016", "Sept-Dec\n2016"
      #,"Jan-Apr\n2017", "May-Aug\n2017", "Sept-Dec\n2017"
    )
  ) +
  labs(x = "Time", y = "Crime Incident Rate (per 1,000)", color = "Study Condition") +
  theme_classic() +
  theme(
    text = element_text(size = 12, colour = "black", family = "Helv"),
    axis.text = element_text(size = 10, colour = "black", family = "Helv"),
    legend.text = element_text(size = 10, colour = "black", family = "Helv"),
    legend.position = c(.8, .8)
  )

# prep data for modelling
# calculate crime rate from population estimates
model_crimes_df <- study_crimes %>%
  as.data.frame() %>%
  filter(trimester < 2017, condition_year == 2016) %>%
  count(trimester, study_condition, parcel_id) %>% 
  ungroup() %>%
  left_join(acs_by_parcel, by = "parcel_id") %>%
  mutate(
    time = trimester - 2015,
    crime_rate = n / total_pop * 1000,
    study_condition = as.factor(study_condition),
    study_condition = relevel(study_condition, ref = "NoTreatment")
  )

m1 <- lmer(
  crime_rate ~ 
    time + 
    study_condition + 
    time*study_condition + 
    total_pop + 
    nbhood_disadv + 
    (1 | parcel_id), 
  data = model_crimes_df, 
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

summary(m1)

# supplemental crime analysis ---------------------------------------------

# visualization: 2016 parcels, 3 year viz
# disaggregated treatment conditions
# count all the crimes within unique combos of parcel/trimester/study_condition
# left join on population estimate
# calculate crime rate (per 1,000)
# plot
windowsFonts(Helv = windowsFont("Helvetica"))

study_crimes %>%
  as.data.frame() %>%
  filter(condition_year == 2016) %>%
  count(parcel_id, trimester, study_condition) %>%
  ungroup() %>%
  left_join(acs_by_parcel, by = "parcel_id") %>%
  mutate(crime_rate = n / total_pop * 1000) %>%
  ggplot() +
  geom_line(stat = "summary", aes(x = trimester, y = crime_rate, color = study_condition), alpha = 0.75) +
  geom_smooth(aes(x = trimester, y = crime_rate, color = study_condition), method = "lm", se = F, size = 1.1) +
  coord_cartesian(ylim = c(0, 100)) +
  scale_x_continuous(
    breaks = seq(2015, 2017.667, by = 1/3),
    labels = c(
      "Jan-Apr\n2015", "May-Aug\n2015", "Sept-Dec\n2015",
      "Jan-Apr\n2016", "May-Aug\n2016", "Sept-Dec\n2016"
      ,"Jan-Apr\n2017", "May-Aug\n2017", "Sept-Dec\n2017"
    )
  ) +
  labs(x = "Time", y = "Crime Incident Rate (per 1,000)", color = "Study Condition") +
  theme_classic() +
  theme(
    text = element_text(size = 12, colour = "black", family = "Helv"),
    axis.text = element_text(size = 10, colour = "black", family = "Helv"),
    legend.text = element_text(size = 10, colour = "black", family = "Helv"),
    legend.position = c(.8, .8)
  )


# point pattern analysis --------------------------------------------------

# spatial libraries
library(maptools)
library(spatstat)

# create analytic window from flint boundary
flint_owin <- flint_sf %>%
  as_Spatial() %>%
  as.owin()

# create ppp data type from crimes_sf
crimes_ppp <- ppp(
  x = st_coordinates(crimes_sf)[ ,1],
  y = st_coordinates(crimes_sf)[ ,2],
  window = flint_owin,
  marks = crimes_sf$trimester
)

# split data based on mark (trimester)
crimes_ppp <- split(crimes_ppp)

# calculate density for each trimester
crimes_sp_ds <- density(crimes_ppp, sigma = 0.5)
