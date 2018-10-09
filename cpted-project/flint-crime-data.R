
# initialize and import ---------------------------------------------------

library(tidyverse)
library(readxl)
library(lubridate)
library(rgdal)
library(sf)
library(sp)


# flint shapefile ---------------------------------------------------------

# The input file geodatabase
fgdb <- "G:/gbushman/cpted/Final-IS.gdb"

# List all feature classes in a file geodatabase
subset(ogrDrivers(), grepl("GDB", name))
fc_list <- ogrListLayers(fgdb)
print(fc_list)

# Read the feature class
flint <- readOGR(dsn=fgdb,layer="Flint_Limits")

# set projection
flint <- spTransform(flint, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))


# 2016 crime data ---------------------------------------------------------

# read in 2016 crime data
mic16 <- read_excel("U:/HBHE/PRC/Projects/YVPC 2000-2005/Crime Data/Original Crime Data Files/2016 Original Data from MSP/Excel Version/2016MICR1.xlsx")
add16 <- read_excel("U:/HBHE/PRC/Projects/YVPC 2000-2005/Crime Data/Original Crime Data Files/2016 Original Data from MSP/Excel Version/2016MICR1_Address.xlsx")

# merge
yr16 <- left_join(
    mic16 %>% filter(MIC1_COUNTY == 25) %>% select(MIC1_NUMBER, MIC1_INC_DATE),
    add16 %>% select(MIC1_NUMBER, LONGITUDE, LATITUDE),
    by = "MIC1_NUMBER"
  ) %>%
  distinct()

# filter blanks in spatial data
yr16 <- yr16 %>% filter(!is.na(LONGITUDE), !is.na(LATITUDE))

# create spatial points data frame
yr16 <- SpatialPointsDataFrame(coords = yr16[, c(3,4)], data = yr16, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))


# merge data --------------------------------------------------------------

# filter points based on shapefile boundaries
yr16 <- yr16[flint, ]

# plot
plot(flint)
points(yr16)
