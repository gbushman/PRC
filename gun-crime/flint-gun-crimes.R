
# initialize and import ---------------------------------------------------

#libraries
library(tidyverse)
library(readxl)
library(lubridate)
library(sf)

# projection
mi_projection = "+proj=lcc +lat_1=42.1 +lat_2=43.66666666666666 +lat_0=41.5 +lon_0=-84.36666666666666 +x_0=4000000 +y_0=0 +ellps=GRS80 +units=m +no_defs"

# gun incidences
weapon_type_guns = c("11", "12", "13", "14", "15", "11A", "12A", "13A", "14A", "15A")


# flint shapefile ---------------------------------------------------------

# The input file geodatabase
fgdb <- "C:/Users/gbushman/Documents/Projects/flint-roads/Flint Roads/Flint Roads.gdb"
flint <- st_read(dsn = fgdb,layer = "Flint_Boundary")

# specify projection
flint <- st_transform(flint, crs = 4326)


# 2012 crime data ---------------------------------------------------------

# read in 2012 crime data
crm12 <- read_excel("U:/HBHE/PRC/Projects/YVPC 2000-2005/Crime Data/Original Crime Data Files/2012 Original Excel Files/1ExportCases.xls")
off12 <- read_excel("U:/HBHE/PRC/Projects/YVPC 2000-2005/Crime Data/Original Crime Data Files/2012 Original Excel Files/2ExportCase Offense.xls")
add12 <- st_read(dsn = "U:/HBHE/PRC/Projects/YVPC 2000-2005/Crime Data/2012WorkingFiles/2012Geocoding/2012FlintCrimeData.gdb",layer = "Geocoding_2012_2")

# join
yr12 <- left_join(
  crm12 %>% select(CaseNumber, OccurredDate),
  off12 %>% select(CaseNumber, CrimeCode, Description),
  by = "CaseNumber"
) %>%
  left_join(
    add12 %>% select(CaseNumber, X, Y),
    by = "CaseNumber"
  ) %>%
  distinct() %>%
  select(
    inc_id        = CaseNumber,    
    inc_date      = OccurredDate,    
    offns_code    = CrimeCode, 
    crime_desc    = Description,
    long          = X,        
    lat           = Y  
  ) %>%
  mutate(
    inc_date = as.Date(inc_date, format = "%m/%d/%y"),
    crime_desc = tolower(crime_desc)
  )

# clean up workspace
rm(crm12, off12, add12)


# 2013 crime data ---------------------------------------------------------

# read in 2013 crime data
crm13 <- read_excel("U:/HBHE/PRC/Projects/YVPC 2000-2005/Crime Data/Original Crime Data Files/2013 Original Excel Files/1ExportCases.xls")
off13 <- read_excel("U:/HBHE/PRC/Projects/YVPC 2000-2005/Crime Data/Original Crime Data Files/2013 Original Excel Files/2ExportCase Offense.xls")
add13 <- read_excel("U:/HBHE/PRC/Projects/YVPC 2000-2005/Crime Data/2013WorkingFiles/Geocoding_2013_CaseNums_Addresses.xlsx")

# join
yr13 <- left_join(
  crm13 %>% select(CaseNumber, OccurredDate),
  off13 %>% select(CaseNumber, CrimeCode, Description),
  by = "CaseNumber"
) %>%
  left_join(
    add13 %>% select(CaseNumber, Longitude, Latitude),
    by = "CaseNumber"
  ) %>%
  distinct() %>%
  select(
    inc_id        = CaseNumber,    
    inc_date      = OccurredDate,    
    offns_code    = CrimeCode,
    crime_desc    = Description,
    long          = Longitude,        
    lat           = Latitude  
  ) %>%
  mutate(
    inc_date = as.Date(inc_date, format = "%m/%d/%y"),
    crime_desc = tolower(crime_desc),
    long = as.double(long),
    lat = as.double(lat)
  )

# clean up workspace
rm(crm13, off13, add13)

# 2014 crime data ---------------------------------------------------------

# read in 2014 crime data
mic14 <- read_excel("U:/HBHE/PRC/Projects/YVPC 2000-2005/Crime Data/Original Crime Data Files/2013-2015 Orginal Text Files from MSP & MSU/2014 files/Excel Versions/2014 MICR1.xlsx")
off14 <- read_excel("U:/HBHE/PRC/Projects/YVPC 2000-2005/Crime Data/Original Crime Data Files/2013-2015 Orginal Text Files from MSP & MSU/2014 files/Excel Versions/2014 OFFNS.xlsx")
add14 <- read_excel("U:/HBHE/PRC/Projects/YVPC 2000-2005/Crime Data/Original Crime Data Files/2013-2015 Orginal Text Files from MSP & MSU/2014 files/Excel Versions/2014 ADDRESS.xlsx")

# join
yr14 <- left_join(
  mic14 %>% select(MIC1_NUMBER, MIC1_INC_DATE),
  off14 %>% select(MIC1_NUMBER, OFFNS_OFFENSE_CO, OFFNS_WEAPON),
  by = "MIC1_NUMBER"
) %>%
  left_join(
    add14 %>% select(MIC1_NUMBER, LONGITUDE, LATITUDE),
    by = "MIC1_NUMBER"
  ) %>%
  distinct() %>%
  select(
    inc_id        = MIC1_NUMBER,    
    inc_date      = MIC1_INC_DATE,    
    offns_code    = OFFNS_OFFENSE_CO, 
    offns_weapon  = OFFNS_WEAPON,
    long          = LONGITUDE,        
    lat           = LATITUDE  
  ) %>%
  mutate(
    inc_date = as.character(inc_date),
    inc_date = ifelse(nchar(inc_date) == 5, paste0("0", inc_date), inc_date),
    inc_date = as.Date(inc_date, format = "%m%d%y"),
    inc_id = as.character(inc_id),
    offns_code = as.character(offns_code)
  )

# clear up workspace
rm(mic14, off14, add14)


# 2015 crime data ---------------------------------------------------------

# read in 2015 crime data
mic15 <- read_excel("U:/HBHE/PRC/Projects/YVPC 2000-2005/Crime Data/Original Crime Data Files/2013-2015 Orginal Text Files from MSP & MSU/2015 files/Excel Versions/MICR1 2015.xlsx")
off15 <- read_excel("U:/HBHE/PRC/Projects/YVPC 2000-2005/Crime Data/Original Crime Data Files/2013-2015 Orginal Text Files from MSP & MSU/2015 files/Excel Versions/MICR OFFNS 2015.xlsx")
add15 <- read_excel("U:/HBHE/PRC/Projects/YVPC 2000-2005/Crime Data/Original Crime Data Files/2013-2015 Orginal Text Files from MSP & MSU/2015 files/Excel Versions/MICR ADDRESS 2015.xlsx")

# join
yr15 <- left_join(
  mic15 %>% select(MIC1_NUMBER, MIC1_INC_DATE),
  off15 %>% select(MIC1_NUMBER, OFFNS_OFFENSE_CO, OFFNS_WEAPON),
  by = "MIC1_NUMBER"
) %>%
  left_join(
    add15 %>% select(MIC1_NUMBER, LONGITUDE, LATITUDE),
    by = "MIC1_NUMBER"
  ) %>%
  distinct() %>%
  select(
    inc_id        = MIC1_NUMBER,    
    inc_date      = MIC1_INC_DATE,    
    offns_code    = OFFNS_OFFENSE_CO,
    offns_weapon  = OFFNS_WEAPON,
    long          = LONGITUDE,        
    lat           = LATITUDE  
  ) %>%
  mutate(
    inc_date = as.character(inc_date),
    inc_date = ifelse(nchar(inc_date) == 5, paste0("0", inc_date), inc_date),
    inc_date = as.Date(inc_date, format = "%m%d%y"),
    inc_id = as.character(inc_id),
    offns_code = as.character(offns_code)
  )

# clear up workspace
rm(mic15, off15, add15)


# 2016 crime data ---------------------------------------------------------

# read in 2016 crime data
mic16 <- read_excel("U:/HBHE/PRC/Projects/YVPC 2000-2005/Crime Data/Original Crime Data Files/2016 Original Data from MSP/Excel Version/2016MICR1.xlsx")
off16 <- read_excel("U:/HBHE/PRC/Projects/YVPC 2000-2005/Crime Data/Original Crime Data Files/2016 Original Data from MSP/Excel Version/2016MICR1_OFFNS.xlsx")
add16 <- read_excel("U:/HBHE/PRC/Projects/YVPC 2000-2005/Crime Data/Original Crime Data Files/2016 Original Data from MSP/Excel Version/2016MICR1_Address.xlsx")

# join
yr16 <- left_join(
  mic16 %>% select(MIC1_NUMBER, MIC1_INC_DATE),
  off16 %>% select(MIC1_NUMBER, OFFNS_OFFENSE_CO, OFFNS_WEAPON),
  by = "MIC1_NUMBER"
) %>%
  left_join(
    add16 %>% select(MIC1_NUMBER, LONGITUDE, LATITUDE),
    by = "MIC1_NUMBER"
  ) %>%
  distinct() %>%
  select(
    inc_id        = MIC1_NUMBER,    
    inc_date      = MIC1_INC_DATE,    
    offns_code    = OFFNS_OFFENSE_CO, 
    offns_weapon  = OFFNS_WEAPON,
    long          = LONGITUDE,        
    lat           = LATITUDE  
  ) %>%
  mutate(
    inc_date = as.character(inc_date),
    inc_date = ifelse(nchar(inc_date) == 5, paste0("0", inc_date), inc_date),
    inc_date = as.Date(inc_date, format = "%m%d%y"),
    inc_id = as.character(inc_id),
    offns_code = as.character(offns_code),
    offns_weapon = as.character(offns_weapon)
  )

# clear up workspace
rm(mic16, off16, add16)


# 2017 crime data ---------------------------------------------------------

# read in 2017 crime data
mic17 <- read.csv("U:/HBHE/PRC/Projects/YVPC 2000-2005/Crime Data/Original Crime Data Files/2017 Original Data from MSP/MICR1.txt", quote = "", row.names = NULL, stringsAsFactors = FALSE)
off17 <- read.csv("U:/HBHE/PRC/Projects/YVPC 2000-2005/Crime Data/Original Crime Data Files/2017 Original Data from MSP/MICR_OFFNS.txt", quote = "", row.names = NULL, stringsAsFactors = FALSE)
add17 <- read.csv("U:/HBHE/PRC/Projects/YVPC 2000-2005/Crime Data/Original Crime Data Files/2017 Original Data from MSP/MICR_ADDRESS.txt", quote = "", row.names = NULL, stringsAsFactors = FALSE)

add17 <- add17 %>%
  mutate(
    MIC1_NUMBER = ifelse(grepl("[[:alpha:]]|[[:punct:]]", MIC1_NUMBER) == T, NA, MIC1_NUMBER),
    MIC1_NUMBER = as.integer(MIC1_NUMBER)
  )

# join
yr17 <- left_join(
  mic17 %>% select(MIC1_NUMBER, MIC1_INC_DATE),
  off17 %>% select(MIC1_NUMBER, OFFNS_OFFENSE_CO, OFFNS_WEAPON),
  by = "MIC1_NUMBER"
) %>%
  left_join(
    add17 %>% select(MIC1_NUMBER, LONGITUDE, LATITUDE) %>% mutate(MIC1_NUMBER = as.integer(MIC1_NUMBER)),
    by = "MIC1_NUMBER"
  ) %>%
  distinct() %>%
  select(
    inc_id        = MIC1_NUMBER,    
    inc_date      = MIC1_INC_DATE,    
    offns_code    = OFFNS_OFFENSE_CO, 
    offns_weapon  = OFFNS_WEAPON,
    long          = LONGITUDE,        
    lat           = LATITUDE  
  ) %>%
  mutate(
    inc_date = as.character(inc_date),
    inc_date = ifelse(nchar(inc_date) == 5, paste0("0", inc_date), inc_date),
    inc_date = as.Date(inc_date, format = "%m%d%y"),
    long = as.double(long),
    lat = as.double(lat),
    inc_id = as.character(inc_id),
    offns_code = as.character(offns_code)
  ) 

# clear up workspace
rm(mic17, off17, add17)


# combine data, create spatial frame --------------------------------------

crimes <- bind_rows(yr12, yr13, yr14, yr15, yr16, yr17)

# indicator for gun crimes
crimes <- crimes %>%
  mutate(
    gun_crime = FALSE,
    gun_crime = ifelse(offns_weapon %in% weapon_type_guns == T & !offns_weapon %in% c(NA, ""), TRUE, gun_crime),
    gun_crime = ifelse(str_detect(crime_desc, "gun\\b|firearm|pistol") == T & !crime_desc %in% c(NA, ""), TRUE, gun_crime)
  )

# filter blanks in spatial data
crimes_cl <- crimes %>% 
  filter(
    !is.na(long), !is.na(lat),
    gun_crime == T
  )

# create spatial points data frame
crimes_cl <- st_as_sf(crimes_cl, coords = c("long", "lat"), crs = 4326)

# filter points based on shapefile boundaries
crimes_cl <- crimes_cl %>% filter(st_intersects(., flint, sparse = FALSE))

# plot
plot(flint)
points(crimes_cl)

# convert to data frame
crimes_cl_df <- crimes_cl %>%
  filter(year(inc_date) %in% 2012:2017) %>%
  mutate(year = year(inc_date)) %>%
  group_by(inc_id) %>%
  arrange(inc_date) %>%
  slice(1) %>%
  ungroup()

# plot
crimes_cl_df %>%
  count(year) %>%
  ggplot(aes(x = year, y = n)) +
  geom_col() +
  labs(x = "Year", y = "Number of Crimes")

# plot % NAs
crimes %>% 
  mutate(
    year = year(inc_date),
    long = ifelse(long == 0, NA, long),
    lat = ifelse(lat == 0, NA, lat)
  ) %>%
  group_by(year) %>%
  summarise(
    tot = n(),
    nas = sum(is.na(long)|is.na(lat)),
    mis = nas / tot
  ) %>%
  filter(year %in% 2012:2017) %>%
  ggplot(aes(x = year, y = mis)) +
  geom_col() +
  scale_y_continuous(limits = c(0, 0.5)) +
  labs(x = "Year", y = "Fraction Missing")


# export ------------------------------------------------------------------

write_rds(crimes_cl_df, "C:/Users/gbushman/Documents/Projects/facts/gun-crimes/flint-gun-crimes.rds")
write_rds(flint, "C:/Users/gbushman/Documents/Projects/facts/gun-crimes/flint-boundaries.rds")

# st_write(crimes_cl_df, dsn = "C:/Users/gbushman/Documents/Projects/facts/gun-crimes/fl_gun_crimes.shp", layer = "fl_gun_crimes.shp", driver = "ESRI Shapefile", delete_layer = T)
# st_write(flint, "C:/Users/gbushman/Documents/Projects/facts/gun-crimes/flint_boundary.shp", layer = "flint_boundary.shp", driver = "ESRI Shapefile", delete_layer = T)
