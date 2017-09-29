
# Initialize and Import ---------------------------------------------------
## packages
library(tidyverse)
library(forcats)
library(haven)
library(magrittr)
library(leaflet)
library(Hmisc)
library(ggmap)
library(sf)

## load data
crime <- read_spss("U:/HBHE/PRC/Projects/YVPC 2000-2005/Crime Data/Jianan's Working File
                   /2015 Flint/2015 Crime Data/Flint_Crime_Data_2015.sav")

colnames(crime) <- colnames(crime) %>% tolower
crime$filec_nibrs_desc <- factor(crime$filec_nibrs_desc)


# Visualize Data ----------------------------------------------------------
## kernel density
X <- cbind(lng,lat)
kde2d <- bkde2D(X, bandwidth=c(bw.ucv(X[,1]),bw.ucv(X[,2])))

## mapping
leaflet(data = crime ) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(
    ~longitude, 
    ~latitude,
    radius = 0.1,
    color = "black"
  )
