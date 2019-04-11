
# initialize and import ---------------------------------------------------

library(tidyverse)
library(readxl)
library(lubridate)
library(haven)
library(rgdal)
library(sf)
library(sp)


# flint shapefile ---------------------------------------------------------

# The input file geodatabase
flint_sf <- st_read("C:/Users/gbushman/Documents/Projects/flint-roads/Flint Roads/Flint Roads.gdb", "Flint_Boundary")
flint_sf <- st_transform(flint_sf, crs = 6498)


# 2011 crime data ---------------------------------------------------------

# read in 2012 crime data
crm11 <- read_excel("U:/HBHE/PRC/Projects/YVPC 2000-2005/Crime Data/Original Crime Data Files/2011 Original Excel Files/1ExportCases.xls")
off11 <- read_excel("U:/HBHE/PRC/Projects/YVPC 2000-2005/Crime Data/Original Crime Data Files/2011 Original Excel Files/2ExportCase Offense.xls")
add11 <- read_spss("C:/Users/gbushman/Box/YVPC Crime Data/Flint/FL11_Crime_Data_20180323_Cleaned_v1.sav") %>% zap_labels()

# join
yr11 <- left_join(
  crm11 %>% select(CaseNumber, OccurredDate),
  off11 %>% select(CaseNumber, CrimeCode),
  by = "CaseNumber"
) %>%
  left_join(
    add11 %>% select(CaseNumber, Crime_Incident_Longitude, Crime_Incident_Latitude),
    by = "CaseNumber"
  ) %>%
  distinct() %>%
  select(
    inc_id        = CaseNumber,    
    inc_date      = OccurredDate,    
    offns_code    = CrimeCode, 
    lat           = Crime_Incident_Longitude,        
    long          = Crime_Incident_Latitude  
  ) %>%
  mutate(
    inc_date = as.Date(inc_date, format = "%m/%d/%y")
  )

# clean up workspace
rm(crm11, off11, add11)


# 2012 crime data ---------------------------------------------------------

# read in 2012 crime data
crm12 <- read_excel("U:/HBHE/PRC/Projects/YVPC 2000-2005/Crime Data/Original Crime Data Files/2012 Original Excel Files/1ExportCases.xls")
off12 <- read_excel("U:/HBHE/PRC/Projects/YVPC 2000-2005/Crime Data/Original Crime Data Files/2012 Original Excel Files/2ExportCase Offense.xls")
add12 <- readOGR(dsn = "U:/HBHE/PRC/Projects/YVPC 2000-2005/Crime Data/2012WorkingFiles/2012Geocoding/2012FlintCrimeData.gdb",layer = "Geocoding_2012_2") %>% as.data.frame()

# join
yr12 <- left_join(
  crm12 %>% select(CaseNumber, OccurredDate),
  off12 %>% select(CaseNumber, CrimeCode),
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
    long          = X,        
    lat           = Y  
  ) %>%
  mutate(
    inc_date = as.Date(inc_date, format = "%m/%d/%y")
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
  off13 %>% select(CaseNumber, CrimeCode),
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
    long          = Longitude,        
    lat           = Latitude  
  ) %>%
  mutate(
    inc_date = as.Date(inc_date, format = "%m/%d/%y"),
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
  off14 %>% select(MIC1_NUMBER, OFFNS_OFFENSE_CO),
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
  off15 %>% select(MIC1_NUMBER, OFFNS_OFFENSE_CO),
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
    off16 %>% select(MIC1_NUMBER, OFFNS_OFFENSE_CO),
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
  off17 %>% select(MIC1_NUMBER, OFFNS_OFFENSE_CO),
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

crimes <- bind_rows(yr11, yr12, yr13, yr14, yr15, yr16, yr17)

# filter blanks in spatial data
crimes_cl <- crimes %>% filter(!is.na(long), !is.na(lat))

# create spatial points data frame
crimes_cl <- crimes_cl %>%
  mutate(
    long = as.numeric(long),
    lat = as.numeric(lat)
  ) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  st_transform(crs = 6498)

# filter points based on shapefile boundaries
flint_crimes <- crimes_cl[flint_sf, ]

# convert to data frame
crimes_cl_df <- crimes %>%
  filter(
    inc_id %in% flint_crimes$inc_id,
    year(inc_date) %in% 2011:2017
  ) %>%
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
  filter(year %in% 2011:2017) %>%
  ggplot(aes(x = year, y = mis)) +
  geom_col() +
  scale_y_continuous(limits = c(0, 0.5)) +
  labs(x = "Year", y = "Fraction Missing")


# export ------------------------------------------------------------------

write_xlsx(crimes_cl_df, "C:/Users/gbushman/Box/CPTED Project (PRC)/CPTED Analysis/20190410-crimes-11-17.xlsx")
