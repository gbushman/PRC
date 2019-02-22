
# initialize and import ---------------------------------------------------

library(tidyverse)
library(lubridate)
library(spatialEco)
library(spatstat)
library(maptools)
library(sf)

gun_crimes <- read_rds("C:/Users/gbushman/Documents/Projects/facts/gun-crimes/flint-gun-crimes.rds")
flint      <- read_rds("C:/Users/gbushman/Documents/Projects/facts/gun-crimes/flint-boundaries.rds")


# gun crimes eda ----------------------------------------------------------

# crime counts by year
gun_crimes %>%
  mutate(year = year(inc_date)) %>%
  ggplot(aes(x = year)) +
  geom_bar(stat = "count") +
  scale_x_continuous(breaks = seq(2012, 2017, 1))

# plot of gun crimes (points)
ggplot() +
  geom_sf(data = flint) +
  geom_sf(data = gun_crimes) +
  scale_x_continuous(breaks = seq(-83.78, -83.61, 0.03)) +
  theme_minimal() +
  labs(x = "Longitude", y = "Latitude")

# plot of gun crimes (contours)
gun_crimes_density <- cbind(gun_crimes, st_coordinates(gun_crimes)) %>%
  mutate(year = year(inc_date))

ggplot() +
  stat_density_2d(data = gun_crimes_density, aes(X, Y, fill = stat(density)), geom = "raster", contour = FALSE) +
  geom_sf(data = flint, aes(color = "red"), alpha = 0.01) +
  scale_fill_viridis_c() +
  scale_x_continuous(breaks = seq(-83.78, -83.61, 0.03)) +
  theme_minimal() +
  labs(x = "Longitude", y = "Latitude")

# kernel density plot (spatstat)
flint_window <- as(flint, "Spatial")
flint_window <- as(flint_window, "SpatialPolygons")
flint_window <- as.owin(flint_window)

crimes_ppp <- ppp(st_coordinates(gun_crimes)[ ,"X"], st_coordinates(gun_crimes)[ ,"Y"], flint_window)
crime_density <- density(crimes_ppp, kernel = "gaussian", edge = T, diggle = T, adjust = 0.6, scalekernel = T)

plot(crime_density)