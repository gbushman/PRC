
# Initialize and Import ---------------------------------------------------
## packages
library(tidyverse)
library(forcats)
library(haven)
library(magrittr)
library(spatstat)
library(leaflet)
library(Hmisc)
library(ggmap)
library(sf)

## load data
crime <- read_spss("U:/HBHE/PRC/Projects/YVPC 2000-2005/Crime Data/Jianan's Working File/2015 Flint/2015 Crime Data/Flint_Crime_Data_2015.sav")

colnames(crime) <- colnames(crime) %>% tolower


# Visualize Data ----------------------------------------------------------
## kernel density
# crime <- crime %>%
#   filter(!is.na(latitude),
#          !is.na(longitude)) %>%
#   select(latitude, longitude) %>%
#   as.data.frame()
# 
# sp <- as(crime, "SpatialPoints")


## mapping
leaflet(data = crime) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(
    ~longitude, 
    ~latitude,
    radius = 0.1,
    color = "black"
  )
