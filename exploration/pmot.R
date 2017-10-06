
# Initialize and Import ---------------------------------------------------
## packages
library(tidyverse)
library(forcats)
library(haven)
library(magrittr)
library(ggmap)
library(sf)

## load data
df <- read_spss("U:/HBHE/PRC/Projects/Flint Byrne/Data/Parcel Assessments/
                Analyses/2014-2016 Merge/Merged_PMOT_Cases_2014_2015_2016_Final.sav")

## create df for each year
### 2014
df.2014 <- df %>% 
  select(Parcel_ID:Census_Tract_2010, 
         Assess_Year_2014:General_Parcel_Blight_Index_2014)

colnames(df.2014) <- colnames(df.2014) %>% str_replace("_2014", "")

### 2015
df.2015 <- df %>% 
  select(Parcel_ID:Census_Tract_2010, 
         Assess_Year_2015:General_Parcel_Blight_Index_2015)
colnames(df.2015) <- colnames(df.2015) %>% str_replace("_2015", "")

### 2016
df.2016 <- df %>% 
  select(Parcel_ID:Census_Tract_2010, 
         Assess_Year_2016:General_Parcel_Blight_Index_2016)
colnames(df.2016) <- colnames(df.2016) %>% str_replace("_2016", "")

## bind dfs
df <- bind_rows(df.2014, df.2015, df.2016, .id = "Year")

df <- df %>%
  mutate(
    Year = factor(Year),
    Year = fct_recode(Year, "2014" = "1", "2015" = "2", "2016" = "3")
  )

## remove dfs
rm(list = setdiff(ls(), "df"))