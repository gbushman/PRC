
# Intitialize and Import --------------------------------------------------

library(tidycensus)
library(tidyverse)
library(readxl)
library(writexl)
library(readr)
library(magrittr)
library(lubridate)
library(haven)
library(gtools)
library(sf)

#vars <- load_variables(2015, "acs5")
census_api_key("48ffcd789ff9d55a06a7a11d71abe3d57b60bea4")


# Query Census API --------------------------------------------------------

acs_data <- get_acs(
  geography = "block group",
  year = 2017,
  variables = c(
    "B17017_001E", # pov household (denom)
    "B17017_002E", # below pov household
    "B19057_001E", # pub assistance (denom)
    "B19057_002E", # with pub assistance
    # "B11001_002E", # fhh (denom)
    # "B11001_006E", # fhh
    # "B99051_001E", # citizenship stat (denom)
    # "B99051_005E", # foreign born
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

acs_pop <- get_acs(
    geography = "block group",
    year = 2017,
    variables = c("B01003_001E"),
    state = "26",
    county = "049",
    output = "wide",
    survey = "acs5",
    geometry = TRUE
  ) %>% 
  rename(
    tot_pop = B01003_001E,
    GEOID10 = GEOID
  )

acs_pop <- acs_pop %>%
  mutate(
    bg_area = st_area(geometry),
    bg_area = as.numeric(bg_area) / 1000000,
    pop_dens = tot_pop / bg_area
  ) %>%
  as.data.frame()

# Clean ACS Output --------------------------------------------------------

acs_data <- acs_data %>%
  select(contains("E")) %>%
  mutate(
    pct_povt = B17017_002E / B17017_001E * 100,
    pct_puba = B19057_002E / B19057_001E * 100,
    pct_rent = B25003_003E / B25003_001E * 100,
    pct_vact = B25002_003E / B25002_001E * 100
  ) %>%
  select(GEOID10 = GEOID, pct_povt:pct_vact)


# Create Neighborhood Disadvantage ----------------------------------------

acs_data$nbhood_disadv <- acs_data[2:5] %>%
  lapply(scale) %>% 
  as.data.frame() %>%
  rowMeans()


# Calculate Block Group-Study Condition Overlaps --------------------------

crimes <- read_excel("C:/Users/gbushman/Box/CPTED Project (PRC)/CPTED Analysis/20190410-crimes-bg.xlsx")

crimes <- left_join(crimes, acs_data %>% select(GEOID10, nbhood_disadv), by = c("GEOID10"))
crimes <- left_join(crimes, acs_pop %>% select(GEOID10, tot_pop, pop_dens), by = c("GEOID10"))

# Scale Neighborhood Disadvantage -----------------------------------------

crimes <- crimes %>%
  mutate(
    scaled_nbdis = nbdis_pct_int * nbhood_disadv,
    scaled_pop   = pop_pct_int * tot_pop,
    scaled_pop_dens = nbdis_pct_int * pop_dens
  )

# Final Extract -----------------------------------------------------------
# clean up CPTED area data
c1 <- crimes %>%
  mutate(
    year = year(inc_date), 
    year = ifelse(month(inc_date) %in% c(1:3), year, year),
    year = ifelse(month(inc_date) %in% c(4:6), year + 0.25, year),
    year = ifelse(month(inc_date) %in% c(7:9), year + 0.5, year),
    year = ifelse(month(inc_date) %in% c(10:12), year + 0.75, year)
  ) %>%
  rename(area = Place) %>%
  filter(
    !is.na(area),
    year >= 2011,
    year < 2018
  ) %>%
  group_by(area) %>%
  summarise(
    nbdis    = sum(unique(scaled_nbdis)),
    tot_pop  = sum(unique(scaled_pop)),
    pop_dens = sum(unique(pop_dens))
  ) %>%
  ungroup()

c <- crimes %>%
  mutate(
    year = year(inc_date), 
    year = ifelse(month(inc_date) %in% c(1:3), year, year),
    year = ifelse(month(inc_date) %in% c(4:6), year + 0.25, year),
    year = ifelse(month(inc_date) %in% c(7:9), year + 0.5, year),
    year = ifelse(month(inc_date) %in% c(10:12), year + 0.75, year)
  ) %>%
  rename(area = Place) %>%
  filter(
    !is.na(area),
    year >= 2011,
    year < 2018
  ) %>%
  group_by(area, year) %>%
  summarise(
    n_crimes = n()
  ) %>%
  ungroup() %>%
  left_join(c1) %>%
  mutate(cpted = TRUE)

quarters <- data.frame(area = c(rep("Atwood Stadium", 28), rep("Einstein's Bagels", 28), rep("Farah's Food Market", 28), rep("Sunset Village Apartments", 28), rep("University Square", 28)), year = rep(seq(2011.00, 2017.75, 0.25), 5))

c <- left_join(quarters, c, by = c("area", "year"))

c <- c %>%
  group_by(area) %>%
  mutate(
    n_crimes = ifelse(is.na(n_crimes), 0, n_crimes),
    nbdis = ifelse(is.na(nbdis), first(nbdis), nbdis),
    tot_pop = ifelse(is.na(tot_pop), first(tot_pop), tot_pop),
    pop_dens = ifelse(is.na(pop_dens), first(pop_dens), pop_dens),
    cpted = TRUE
  ) %>%
  ungroup()

# clean up other block group area data
excluded_bgs <- crimes %>% filter(exclude == T) %$% unique(GEOID10)

other_c <- crimes %>%
  mutate(
    year = year(inc_date), 
    year = ifelse(month(inc_date) %in% c(1:3), year, year),
    year = ifelse(month(inc_date) %in% c(4:6), year + 0.25, year),
    year = ifelse(month(inc_date) %in% c(7:9), year + 0.5, year),
    year = ifelse(month(inc_date) %in% c(10:12), year + 0.75, year)
  ) %>%
  filter(
    year >= 2011,
    year < 2018
  ) %>%
  group_by(GEOID10, year) %>%
  summarise(
    n_crimes = n(),
    nbdis    = mean(nbhood_disadv),
    tot_pop  = mean(tot_pop),
    pop_dens = mean(pop_dens)
  ) %>%
  ungroup() %>%
  mutate(cpted = FALSE) %>%
  filter(!GEOID10 %in% excluded_bgs)

quarters <- data.frame(GEOID10 = unlist(lapply(other_c %>% distinct(GEOID10) %$% GEOID10, function(x){rep(x, 28)})), year = rep(seq(2011.00, 2017.75, 0.25), 124))

other_c <- left_join(quarters, other_c, by = c("GEOID10", "year"))

other_c <- other_c %>%
  group_by(GEOID10) %>%
  mutate(
    n_crimes = ifelse(is.na(n_crimes), 0, n_crimes),
    nbdis = ifelse(is.na(nbdis), first(nbdis), nbdis),
    tot_pop = ifelse(is.na(tot_pop), first(tot_pop), tot_pop),
    pop_dens = ifelse(is.na(pop_dens), first(pop_dens), pop_dens),
    cpted = FALSE
  ) %>%
  ungroup()

other_c$area <- as.character(group_indices(other_c, GEOID10))

other_c <- other_c %>% select(-GEOID10)

out <- bind_rows(c, other_c)

# clean up data for export
# original <- read_csv("C:/Users/Greg/Box/CPTED Project (PRC)/CPTED Analysis/mlm-final-extract2.csv") %>%
#   select(area, year = Year_, cpted:intensity)

out <- out %>% 
  mutate(
    crime_rate = n_crimes / tot_pop*1000,
    crime_rate_2 = n_crimes / pop_dens*1000,
    area = as.factor(area)
  ) 

# create crime rate cats
cats <- out %>%
  filter(year == 2011) %>%
  select(area, crime_rate, cpted) %>%
  mutate(
    crime_rate_cat = quantcut(crime_rate, 4),
    crime_rate_cat = ifelse(cpted == 1, "CPTED", crime_rate_cat)
  ) %>%
  select(area, crime_rate_cat)

# merge on crime rate cats
out <- left_join(out, cats, by = "area")


# export ------------------------------------------------------------------

write_xlsx(out, "C:/Users/gbushman/Box/CPTED Project (PRC)/CPTED Analysis/20190410-cpted-extract.xlsx")

rm(list = ls())
