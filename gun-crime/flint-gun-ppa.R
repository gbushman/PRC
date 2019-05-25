
# initialize and import ---------------------------------------------------

# libraries
library(tidyverse)
library(sf)
# library(maptools)
# library(spatstat)

# load data
crimes_sf <- read_rds("C:/Users/gbushman/Documents/Projects/facts/gun-crimes/flint-gun-crimes.rds")
flint     <- read_rds("C:/Users/gbushman/Documents/Projects/facts/gun-crimes/flint-boundaries.rds")


# yearly crime trends -----------------------------------------------------

# gun crimes
crimes_sf %>%
  as.data.frame() %>%
  filter(
    gun_crime == T,     # gun crimes
    !is.na(victim_age), # victimizations
    victim_age < 18     # youth victims
  ) %>%
  select(year, inc_id) %>%
  distinct() %>%
  count(year) %>%
  ggplot(aes(x = year, y = n)) +
  geom_point() +
  geom_line()


# point pattern analysis --------------------------------------------------

# spatial libraries
library(maptools)
library(spatstat)

# create analytic window from flint boundary
flint_owin <- flint %>%
  as_Spatial() %>%
  as.owin()

# filter crime data down to gun crimes with a victim
# create ppp data type from crimes_sf
crimes_ppp <- crimes_sf %>%
  filter(gun_crime == T, !is.na(victim_age)) %>%
  mutate(
    gun_vic_type = factor(ifelse(victim_age >= 18, "adult", "youth"))
  )

crimes_ppp <- ppp(
  x = st_coordinates(crimes_ppp)[ ,1],
  y = st_coordinates(crimes_ppp)[ ,2],
  window = flint_owin,
  marks = crimes_ppp$gun_vic_type
)

# split data based on gun crime victim type
crimes_ppp_sp <- split(crimes_ppp)

# calculate density
crimes_sp_ds <- density(crimes_ppp_sp)
plot(crimes_sp_ds)