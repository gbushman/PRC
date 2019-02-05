# Initialize --------------------------------------------------------------

library(censusapi)
library(tidyverse)

# Query Census API --------------------------------------------------------

api_key <- "..." # your census api key

df <- map_df(2012:2017,
  # census api query
  ~getCensus(
      name = "acs1",           # ACS 1-year
      vintage = .,             # 2013 - 2017 (mapped years)
      vars = c(
        "B01003_001E",         # total population
        "B01003_001M"          # margin of error
      ),
      region = "place:29000",  # place: Flint
      regionin = "state:26",   # state: Michigan
      key = api_key),
  .id = "year"
  ) %>%
  # mutate columns for "city" and "year"
  mutate(
    city = "Flint",
    year = as.numeric(year) + 2011
  ) %>%
  # rename columns of census data
  rename(
    total_pop  = B01003_001E,
    tot_pop_er = B01003_001M
  )

# Plot Population by Year -------------------------------------------------

df %>% 
  ggplot(aes(x = year, y = total_pop)) + 
  geom_point() + 
  geom_smooth(method = "lm")
