
# initialize and import ---------------------------------------------------

library(tidyverse)
library(readxl)
library(lubridate)
library(magrittr)

inj <- read_excel("U:/HBHE/PRC/Projects/YVPC 2015-2020/Data Management/Injury Data/Flint/Original/FL17_Injury_Data_20180620_Original.xlsx")


# clean up injury data ----------------------------------------------------

colnames(inj) <- tolower(colnames(inj))

inj <- inj %>%
  mutate(
   enterdate  = str_remove_all(enterdate, " "),
   enterdate  = as.Date(enterdate, format = "%m/%d/%Y"),
   triagedate = str_remove_all(triagedate, " "),
   triagedate = as.Date(triagedate, format = "%m/%d/%Y")
  )


# eda ---------------------------------------------------------------------

# why is the enterdate so long after the triagedate?
inj %>%
  mutate(triage_to_entry = as.numeric(enterdate - triagedate)) %>%
  ggplot(aes(triage_to_entry)) +
  geom_histogram() +
  theme_classic()

# plot of injuries per week, aggregated across years
inj %>%
  mutate(
    week_td = strftime(triagedate, format = "%V"),
    year_td = year(triagedate)
  ) %>% 
  count(week_td) %>%
  ungroup() %>%
  mutate(week_td = as.numeric(week_td)) %>%
  ggplot(aes(x = week_td, y = n)) +
  geom_point() + 
  geom_line(aes(group = 1)) +
  scale_x_continuous(limits = c(0, 52), breaks = seq(1, 52, 4)) +
  labs(x = "week num.", y = "injury count") +
  theme_classic()