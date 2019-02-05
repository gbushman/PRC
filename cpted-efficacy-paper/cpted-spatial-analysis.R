
# initialize and import ---------------------------------------------------

library(tidyverse)
library(readxl)
library(writexl)
library(lubridate)
library(maptools)
library(leaflet)
library(rgdal)
library(rgeos)
library(sf)
library(sp)


# point data --------------------------------------------------------------

crimes <- read_excel("C:/Users/Greg/Box/CPTED Project (PRC)/CPTED Analysis/20181009-crimes-12-17.xlsx")
crimes <- SpatialPointsDataFrame(coords = crimes[ , 4:5], data = crimes[ , 1:3], proj4string = CRS("+proj=longlat +datum=WGS84"))
crimes <- spTransform(crimes, CRS("+init=epsg:26916"))

# hotspots <- read_excel("C:/Users/gbushman/Desktop/hotspot-locations.xlsx")
# hotspots <- SpatialPointsDataFrame(coords = hotspots[ , 2:3], data = hotspots[ , 1], proj4string = CRS("+proj=longlat +datum=WGS84"))
# hotspots <- spTransform(hotspots, CRS("+init=epsg:26916"))

atwood <- matrix(
  c(
    -83.701828, 43.018054,
    -83.703614, 43.017288,
    -83.702959, 43.016477,
    -83.701168, 43.017239,
    -83.701828, 43.018054
  ),
  ncol = 2,
  byrow = TRUE
)

einstein <- matrix(
  c(
    -83.712268, 43.013416,
    -83.712053, 43.013153,
    -83.711727, 43.013305,
    -83.711947, 43.013562,
    -83.712268, 43.013416
  ),
  ncol = 2,
  byrow = TRUE
)

farrah <- matrix(
  c(
    -83.708559, 43.018922,
    -83.708240, 43.019070,
    -83.708415, 43.019183,
    -83.708665, 43.019067,
    -83.708559, 43.018922
  ),
  ncol = 2,
  byrow = TRUE
)

sunset <- matrix(
  c(
    -83.724995, 43.012701,
    -83.723343, 43.012075,
    -83.723869, 43.010488,
    -83.725897, 43.011165,
    -83.724995, 43.012701
  ),
  ncol = 2,
  byrow = TRUE
)

university <- matrix(
  c(
    -83.699063, 43.020082,
    -83.698551, 43.019662,
    -83.699696, 43.019158,
    -83.700111, 43.019647
  ),
  ncol = 2,
  byrow = TRUE
)


hs1 <- Polygon(atwood)
hs2 <- Polygon(einstein)
hs3 <- Polygon(farrah)
hs4 <- Polygon(sunset)
hs5 <- Polygon(university)


hotspots <- SpatialPolygons(
  list(
    Polygons(list(hs1), ID = "Atwood Stadium"), 
    Polygons(list(hs2), ID = "Einstein's Bagels"),
    Polygons(list(hs3), ID = "Farah's Food Market"),
    Polygons(list(hs4), ID = "Sunset Village Apartments"),
    Polygons(list(hs5), ID = "University Square")
  ), 
  proj4string=CRS("+proj=longlat +datum=WGS84")
)

hotspots <- spTransform(hotspots, CRS("+init=epsg:26916"))


# flint shapefiles --------------------------------------------------------

# The input file geodatabase
fgdb <- "G:/gbushman/cpted/Final-IS.gdb"
flint <- readOGR(dsn = fgdb,layer = "Flint_Limits")
flint_bg <- readOGR(dsn = fgdb, layer = "Flint_Block_Groups")
#ogrListLayers(fgdb)

# specify projection
flint <- spTransform(flint, CRS("+init=epsg:26916"))
flint_bg <- spTransform(flint_bg, CRS("+init=epsg:26916"))


# hotspot buffer areas ----------------------------------------------------

hs_buffer <- gBuffer(hotspots, byid = TRUE, width = 150)

atwood_buff <- hs_buffer[1]
einstein_buff <- hs_buffer[2]
farrah_buff <- hs_buffer[3]
sunset_buff <- hs_buffer[4]
usquare_buff <- hs_buffer[5]

atwood_buff_clip <- gDifference(atwood_buff, usquare_buff)
usquare_buff_clip <- gDifference(usquare_buff, atwood_buff)

hs_buffer <- rbind(atwood_buff_clip, einstein_buff, farrah_buff, sunset_buff, usquare_buff_clip, makeUniqueIDs = TRUE)


# spatial joins for crimes ------------------------------------------------

crimes <- spCbind(crimes, over(crimes, flint_bg["GEOID10"]))
crimes <- spCbind(crimes, over(crimes, hs_buffer))


# spatial joins for hotspot-block group intersections ---------------------

bg_sf <- st_as_sf(flint_bg)
hs_sf <- st_as_sf(hs_buffer)
hs_sf$Place <- c("Atwood Stadium", "Einstein's Bagels", "Farah's Food Market", "Sunset Village Apartments", "University Square")

int <- as_tibble(st_intersection(bg_sf, hs_sf)) %>%
  select(GEOID10, Place, geometry) %>%
  mutate(int_area = st_area(geometry)) %>%
  group_by(Place) %>%
  mutate(
    int_area = as.numeric(int_area),
    place_area = sum(int_area)
  ) %>%
  ungroup() %>%
  select(-geometry) %>%
  as.data.frame()

bg_int <- bg_sf %>%
  mutate(
    bg_area = st_area(geometry),
    bg_area = as.numeric(bg_area)
  ) %>%
  select(GEOID10, bg_area) %>%
  left_join(int, by = "GEOID10") %>%
  as.data.frame() %>%
  select(
    GEOID10,
    Place, 
    bg_area,
    place_area,
    int_area
  ) %>%
  mutate(
    nbdis_pct_int = int_area / place_area,
    nbdis_pct_int = ifelse(is.na(nbdis_pct_int), 1, nbdis_pct_int),
    pop_pct_int   = int_area / bg_area,
    pop_pct_int   = ifelse(is.na(pop_pct_int), 1, pop_pct_int)
  ) %>%
  select(GEOID10, Place, nbdis_pct_int, pop_pct_int, bg_area)


# summary stats -----------------------------------------------------------

int %>% 
  group_by(Place) %>% 
  arrange(place_area) %>% 
  slice(1) %>% 
  ungroup() %>% 
  summarise(mean_area = mean(place_area), sd_area = sd(place_area)) %>% 
  as.data.frame()

bg_int %>% 
  filter(!GEOID10 %in% c("260490028001", "260499801001", "260499800001", "260490015001", "260490028002", "260490015003", "260490037002", "260490016001", "260490016003", "260490016005", "260490037003", "260490015002")) %>%
  group_by(GEOID10) %>% 
  arrange(bg_area) %>% 
  slice(1) %>% 
  ungroup() %>% 
  summarise(mean_area = mean(bg_area), sd_area = sd(bg_area)) %>% 
  as.data.frame()

# process output ----------------------------------------------------------

crimes_df <- as.data.frame(crimes) %>% 
  rename(Place = over.crimes..hs_buffer.) %>% 
  mutate(Place = as.character(Place))

Place_lu <- c(
  "1" = "Atwood Stadium", 
  "2" = "Einstein's Bagels", 
  "3" = "Farah's Food Market", 
  "4" = "Sunset Village Apartments", 
  "5" = "University Square"
)

crimes_df$Place <- Place_lu[crimes_df$Place] 

crimes_df <- left_join(crimes_df, bg_int, by = c("GEOID10", "Place")) %>%
  mutate(exclude = ifelse(is.na(nbdis_pct_int) & is.na(pop_pct_int), TRUE, FALSE))


# plot hotspots and buffers -----------------------------------------------

hotspots_lflt  <- spTransform(hotspots,  CRS("+ellps=WGS84 +proj=longlat +datum=WGS84 +no_defs"))
hs_buffer_lflt <- spTransform(hs_buffer,  CRS("+ellps=WGS84 +proj=longlat +datum=WGS84 +no_defs"))
flint_lflt     <- spTransform(flint, CRS("+ellps=WGS84 +proj=longlat +datum=WGS84 +no_defs"))
flint_bg_lflt  <- spTransform(flint_bg, CRS("+ellps=WGS84 +proj=longlat +datum=WGS84 +no_defs"))

hotspots_map <- leaflet() %>%
  addProviderTiles("Esri.WorldImagery", options = providerTileOptions(opacity = 0.5)) %>%
  addPolygons(data = flint_lflt, color = "red", weight = 5, fillColor = "transparent") %>%
  addPolygons(data = flint_bg_lflt, color = "red", weight = 2, fillColor = "transparent") %>%
  addPolygons(data = hs_buffer_lflt, fillColor = "blue") %>%
  addPolygons(data = hotspots_lflt, fillColor = "blue")


# export ------------------------------------------------------------------

write_xlsx(crimes_df, "C:/Users/Greg/Box/CPTED Project (PRC)/CPTED Analysis/20181009-crimes-bg.xlsx")

#rm(list = ls())