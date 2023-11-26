# elix.R
# Scores patients based on comorbidity risk

library(comorbidity)
library(tidyverse)
library(readr)

# Read in dx_detail
dx_detail <- read_csv("./etc/summary_tables/dx_detail.csv") 

# Reshape data for elixhauser scoring - this takes one minute to run for every
# 16k records in dx_detail
elix_inputs <- dx_detail %>% 
  select(DUPERSID, ICD10CM) %>% 
  mutate(ID = DUPERSID) %>% 
  group_by(DUPERSID) %>% 
  nest() %>% 
  ungroup() %>% 
  mutate(com = map(data, ~comorbidity(x = .x,
                                      id = "ID",
                                      code = "ICD10CM",
                                      map = "elixhauser_icd10_quan",
                                      assign0 = F),
                   .progress = T),
         score = map_int(com, ~score(.x, weights="vw", assign0 = F),
                         .progress = T)) 

elix_outputs <- elix_inputs %>% 
  unnest(cols = c(com)) %>% 
  select(-data, -ID) %>% 
  unnest(cols = c(score))

elix_outputs_tidy <- elix_outputs %>% 
  as_tibble() %>% 
  pivot_longer(cols = chf:score,
               names_to = "Field_Name",
               values_to = "Field_Value")

# Now join to a description for each of the comorbidity names
# This crossmap is developed from the package source files on Github
# https://github.com/ellessenne/comorbidity

elix_dsc_map <- read_csv("./etc/elix_dsc_map.csv") 

elix_outputs_final <- elix_outputs_tidy %>% 
  inner_join(elix_dsc_map) %>% 
  write_csv("./etc/summary_tables/elix_detail.csv")
