
# initialize and import ---------------------------------------------------

# libraries
library(tidyverse)
library(lubridate)
library(haven)
library(sf)

# parcel file
parcels_sf <- read_sf("U:/HBHE/PRC/GIS/Projects/MI-YVPC 2015-2020/Base Data/2018/Flint_MI_2018.gdb", "FL18_MIYVPC_Parcels_v11")
parcels_sf <- st_transform(parcels_sf, crs = 6498)

# 2016 mow data
mows_16 <- read_spss("U:/HBHE/PRC/Projects/YVPC 2015-2020/Data Management/Mow_Maintenance/Flint/Cleaned/Archived/FL16_MowMaint_20190320_Cleaned_GeoFull_v3.sav")

# 2017 mow data
mows_17_pm <- read_spss("U:/HBHE/PRC/Projects/YVPC 2015-2020/Data Management/Mow_Maintenance/Flint/Cleaned/Archived/FL17_MowMaint_ProfessionalMow_20180228_Cleaned_GeoFull_v3.sav")
mows_17_ce <- read_spss("U:/HBHE/PRC/Projects/YVPC 2015-2020/Data Management/Mow_Maintenance/Flint/Cleaned/Archived/FL17_MowMaint_Community_Engaged_20180628_Cleaned_GeoFull_v2.sav")


# pull out relevant parcel information ------------------------------------

# create base file of all parcel-year combinations
all_parcels <- parcels_sf %>% 
  as.data.frame() %>%
  select(MIYVPC_Parcel_ID) %>%
  mutate(`2016` = 0, `2017` = 0, `2018` = 0) %>%
  gather("year", "trash", -MIYVPC_Parcel_ID) %>%
  select(-trash)

# pull out intervention parcels
# keep study condtion-year combinations
mow_parcels <- parcels_sf %>% 
  as.data.frame() %>%
  filter(Intervention_Parcel == 1) %>%
  select(
    MIYVPC_Parcel_ID, Community_Engagement_2016, Professional_Mow_2016, NoTreatment_2016,
    Community_Engagement_2017, Professional_Mow_2017, NoTreatment_2017,
    Community_Engagement_2018, Professional_Mow_2018, NoTreatment_2018
  ) %>% 
  gather(key = "condition", val = "indicator", Community_Engagement_2016:NoTreatment_2018) %>%
  filter(indicator == 1) %>%
  mutate(
    year            = str_extract(condition, "\\d{4}"),
    study_condition = NA,
    study_condition = ifelse(str_detect(condition, "Community"), "CE", study_condition),
    study_condition = ifelse(str_detect(condition, "Professional"), "PM", study_condition),
    study_condition = ifelse(str_detect(condition, "NoTreatment"), "NT", study_condition)
  ) %>%
  select(MIYVPC_Parcel_ID, year, study_condition)


# clean 2016 mow data -----------------------------------------------------

# clean mow event data
mows_16 <- mows_16 %>%
  select(MIYVPC_Parcel_ID, Mow_1:Mow_10) %>%
  gather(key = "mow_round", val = "mow_date_exact", Mow_1:Mow_10) %>%
  mutate(
    year            = "2016",
    mow_round       = str_remove_all(mow_round, "Mow_"),
    mow_round       = as.numeric(mow_round),
    mow_round_start = NA,
    mow_round_end   = NA,
    mow_date_exact  = as.Date(mow_date_exact, origin = "1970-01-01")
  ) %>%
  filter(!is.na(mow_date_exact))

# combine mow data with parcel data
mows_16_out <- all_parcels %>%
  filter(year == "2016") %>%
  left_join(mow_parcels %>% filter(year == "2016"), by = c("MIYVPC_Parcel_ID", "year")) %>%
  left_join(mows_16, by = c("MIYVPC_Parcel_ID", "year")) %>%
  mutate(mowed = !is.na(mow_date_exact)) %>%
  distinct() %>%
  group_by(MIYVPC_Parcel_ID) %>%
  mutate(total_mows = sum(mowed)) %>%
  ungroup() %>%
  mutate(
    mower      = NA,
    mower      = ifelse(!is.na(mow_date_exact) & study_condition == "PM", "Professional", mower),
    mower      = ifelse(!is.na(mow_date_exact) & study_condition %in% c("CE", "NT", NA), "GCLB", mower),
    total_mows = ifelse(is.na(total_mows), 0, total_mows),
    mow_type   = NA,
    mow_type   = ifelse(total_mows > 0, "Unk", mow_type),
    mow_type   = ifelse(total_mows > 0 & study_condition == "PM", "Full Mow", mow_type)
  ) %>%
  select(-mowed)

# create spatial version of mow data
mows_16_out_sf <- parcels_sf %>%
  select(MIYVPC_Parcel_ID, Shape) %>%
  left_join(mows_16_out, by = "MIYVPC_Parcel_ID")

# join mow data to 2016 cohort parcel buffer areas
mow_parcels_2016 <- parcels_sf %>%
  select(MIYVPC_Parcel_ID, Shape) %>%
  left_join(mow_parcels, by = "MIYVPC_Parcel_ID") %>%
  st_as_sf(sf_column_name = "Shape") %>%
  filter(year == 2016) %>%
  st_centroid() %>%
  st_buffer(150)


# clean 2017 mow data -----------------------------------------------------

# create frame of CE mow round dates
ce_mow_round_dates <- tribble(
  ~mow_round, ~mow_round_start, ~mow_round_end,
  1, "04/10/2017", "05/12/2017",
  2, "05/13/2017", "06/02/2017",
  3, "06/03/2017", "06/23/2017",
  4, "06/24/2017", "07/14/2017",
  5, "07/15/2017", "08/04/2017",
  6, "08/05/2017", "08/25/2017",
  7, "08/26/2017", "09/15/2017"
) %>%
  mutate(
    mow_round_start = as.Date(mow_round_start, format = "%m/%d/%Y"),
    mow_round_end   = as.Date(mow_round_end, format = "%m/%d/%Y")
  )

# clean CE mow data
mows_17_ce <- mows_17_ce %>%
  select(MIYVPC_Parcel_ID, Round_1:Round_7_Service_Type) %>%
  gather("var", "val", -MIYVPC_Parcel_ID) %>%
  mutate(
    mow_round = str_extract(var, "\\d{1}"),
    mow_round = as.numeric(mow_round),
    var       = str_remove_all(var, "\\d")  
  ) %>%
  spread(var, val) %>%
  filter(Round_ == 1) %>%
  left_join(ce_mow_round_dates, by = "mow_round") %>%
  mutate(
    mow_date_exact  = NA,
    mower           = "GCLB",
    mow_type        = NA,
    mow_type        = ifelse(Round__Service_Type == 1, "Mow Strip", mow_type),
    mow_type        = ifelse(Round__Service_Type == 2, "Half Mow", mow_type),
    mow_type        = ifelse(Round__Service_Type == 3, "Full Mow", mow_type),
    mow_type        = ifelse(Round__Service_Type == 4, "Verified Maintained", mow_type),
    year            = "2017"
  ) %>%
  select(-Round_, -Round__Service_Type)
  
# clean PM mow data
mows_17_pm <- mows_17_pm %>%
  select(MIYVPC_Parcel_ID, Service_Date_1:Service_Date_11) %>%
  gather(key = "mow_round", val = "mow_date_exact", -MIYVPC_Parcel_ID) %>%
  mutate(
    mow_round       = str_remove_all(mow_round, "Service_Date_"),
    mow_round       = as.numeric(mow_round),
    mow_round_start = NA,
    mow_round_end   = NA,
    mow_date_exact  = as.Date(mow_date_exact, origin = "1970-01-01"),
    mower           = "Professional",
    mow_type        = "Full Mow",
    year            = "2017"
  ) %>%
  filter(!is.na(mow_date_exact)) %>%
  add_count(MIYVPC_Parcel_ID, name = "total_mows")

# bind rows (CE + PM)
mows_17 <- bind_rows(mows_17_ce, mows_17_pm)

# merge
mows_17_out <- all_parcels %>%
  filter(year == "2017") %>%
  left_join(mow_parcels %>% filter(year == "2017"), by = c("MIYVPC_Parcel_ID", "year")) %>%
  left_join(mows_17, by = c("MIYVPC_Parcel_ID", "year")) %>%
  mutate(mowed = !is.na(mow_round)) %>%
  distinct() %>%
  group_by(MIYVPC_Parcel_ID) %>%
  mutate(total_mows = sum(mowed)) %>%
  ungroup() %>%
  select(-mowed)


# merge years of data -----------------------------------------------------

all_mows_out <- bind_rows(mows_16_out, mows_17_out)


# create spatial version of mow data --------------------------------------

# all_mows_count <- all_mows_out %>%
#   group_by(MIYVPC_Parcel_ID) %>%
#   summarise(total_mows = sum(unique(total_mows))) %>%
#   ungroup()
# 
# all_mows_sf <- parcels_sf %>% 
#   select(MIYVPC_Parcel_ID, Shape) %>%
#   right_join(all_mows_count)


# write out data ----------------------------------------------------------

# write_xlsx(all_mows_out, "U:/HBHE/PRC/Projects/YVPC 2015-2020/Data Management/Mow_Maintenance/Flint/Cleaned/FL16+17_MowMaint_20190424_Cleaned.xlsx")
# arc.write("C:/Users/gbushman/Documents/Projects/mow/mow-data-gis/mow-data-gis.gdb/fc", all_mows_sf)
