
# Initialize and Import ---------------------------------------------------
## packages
library(tidyverse)
library(haven)
library(magrittr)
library(Hmisc)

## data sources
nls <- read_spss("U:/HBHE/PRC/Projects/Flint Byrne/Data/Survey/2014-2016-2017_Byrne Analyses/Merged Data & Final Analyses/2014-2016-2017 merged datav1_3.sav")

# NLS Data ----------------------------------------------------------------

## visualize the data
nls_means <- nls %>%
  group_by(Study_Condition, Survey_Wave) %>%
  summarise(
    NHDis            = mean(NHDis, na.rm = T),
    PolicePercep     = mean(PolicePercep, na.rm = T),
    PoliceRelation   = mean(PoliceRelation, na.rm = T),
    Victim           = mean(Victim, na.rm = T),
    Parks            = mean(Parks, na.rm = T),
    MentalHealth     = mean(MentalHealth, na.rm = T)
  ) %>%
  ungroup() %>%
  filter(Survey_Wave != 2) %>%
  gather(key = "Variable", value = "Group Mean", -c(Study_Condition, Survey_Wave))

nls_means_plots <- nls_means %>% 
  ggplot(aes(x = Study_Condition, y = `Group Mean`, fill = factor(Survey_Wave))) + 
  geom_col(position = "dodge") +
  facet_wrap(~Variable)

nls_means_plots

## Run ANCOVA models to see if there were significant improvements in resident
## perceptions, between pre- and post- tests, by study condition. Outcome 
## variable is the `Group Mean`, factor variable is the Study_Condition, and 
## covariate is the original score. So, controlling for the original score,
## did the intervention have an impact on the mean level of resident perceptions?



