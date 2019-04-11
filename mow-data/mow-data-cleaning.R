
# initialize and import ---------------------------------------------------

# libraries
library(tidyverse)
library(lubridate)
library(magrittr)
library(readxl)
library(sf)

# mow data
pm_16 <- read_spss("U:/HBHE/PRC/Projects/YVPC 2015-2020/Data Management/Mow_Maintenance/Flint/Original/FL16_MowMaint_ProfessionalMow_20190215_Original.sav") %>% zap_labels()
pm_17 <- read_spss("U:/HBHE/PRC/Projects/YVPC 2015-2020/Data Management/Mow_Maintenance/Flint/Original/FL17_MowMaint_ProfessionalMow_20190215_Original.sav")
ce_16 <- read_excel("U:/HBHE/PRC/Projects/YVPC 2015-2020/Data Management/Mow_Maintenance/Flint/Original/Copy of 2016_CG_Parcel_Maintenance_sf (2).xls")
ce_17 <- read_excel("U:/HBHE/PRC/Projects/YVPC 2015-2020/Data Management/Mow_Maintenance/Flint/Original/FL17_MowMain_CommunityEngaged_Original.xlsx")

# full parcel file
parcels_sf <- read_sf("U:/HBHE/PRC/GIS/Projects/MI-YVPC 2015-2020/Base Data/2018/Flint_MI_2018.gdb", "FL18_MIYVPC_Parcels_v10")
parcels_sf <- st_transform(parcels_sf, crs = 6498)
parcels_df <- as.data.frame(parcels_sf)

# create address suffixes vector
suffixes <- c("st", "dr", "ave", "rd", "pl", "dr", "blvd", "hwy", "ct", "cir", "pkwy", "blv") %>%
  str_replace(., "(.+)", "\\\\b\\1\\\\b")


# clean/format each file --------------------------------------------------

parcels_df <- parcels_df %>%
  select(
    MIYVPC_Parcel_ID, Parcel_ID, Full_Address,
    Community_Engagement_2016, Community_Engagement_2017, Community_Engagement_2018,
    Professional_Mow_2016, Professional_Mow_2017, Professional_Mow_2018,
    NoTreatment_2016, NoTreatment_2017, NoTreatment_2018
  ) %>%
  gather("condition", "indicator", Community_Engagement_2016:NoTreatment_2018) %>%
  filter(indicator == 1) %>%
  mutate(
    year = str_extract(condition, "\\d{4}$"),
    condition = str_remove_all(condition, "[[:punct:]]|[[:digit:]]")
  ) %>%
  select(-indicator) %>%
  mutate(
    street     = tolower(Full_Address),
    street     = str_remove_all(street, "[[:punct:]]"),
    street     = str_remove_all(street, str_c(suffixes, collapse = "|")),
    street_num = str_extract(street, "^\\d{1,5}\\b"),
    street     = str_remove_all(street, "^\\d{1,5}\\b"),
    street     = trimws(street)
  )

# professional mow data
pm_16 %>% 
  mutate(
    street     = tolower(Address),
    street     = str_remove_all(street, "[[:punct:]]"),
    street     = str_remove_all(street, str_c(suffixes, collapse = "|")),
    street_num = str_extract(street, "^\\d{1,5}\\b"),
    street     = str_remove_all(street, "^\\d{1,5}\\b"),
    street     = trimws(street)
  ) %>%
  left_join(parcels_df, by = c("street", "street_num"))
