
# Intitialize and Import --------------------------------------------------

library(tidycensus)
library(tidyverse)

# vars <- load_variables(2014, "acs5")
# census_api_key("api key", install = TRUE)


# Query Census API --------------------------------------------------------

acs_data <- get_acs(
  geography = "block group",
  year = 2014,
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