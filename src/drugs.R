library(tidyverse)
library(MEPS)
library(survey)
library(srvyr)

# Download and process by-group drug utilization data in the FYC files
# https://meps.ahrq.gov/mepsweb/data_stats/download_data_files.jsp
fyc21 <- MEPS::read_MEPS(year = 2021, type = "FYC")
fyc20 <- MEPS::read_MEPS(year = 2020, type = "FYC")
fyc19 <- MEPS::read_MEPS(year = 2019, type = "FYC")
link <- MEPS::read_MEPS(type = "Pooled linkage")

drugs21 <- MEPS::read_MEPS(year = 2021, type = "RX")
drugs20 <- MEPS::read_MEPS(year = 2020, type = "RX")
drugs19 <- MEPS::read_MEPS(year = 2019, type = "RX")

# Select relevant fields from our FYC files and pool respondent files.
# Pooling caveats/instructions are located here:
# https://meps.ahrq.gov/data_stats/download_data/pufs/h036/h36u20doc.shtml#2

fyc21_demos <- fyc21 %>% 
  select(DUPERSID, RXTOT21, RXEXP21, RXSLF21, VARPSU, VARSTR, PERWT21F, RACETHX, 
         AGE21X, SEX, PANEL) %>% 
  rename(RXTOTYY=RXTOT21,
         RXEXPYY=RXEXP21,
         RXSLFYY=RXSLF21,
         PERWTYYF=PERWT21F,
         AGEYYX=AGE21X) %>% 
  mutate(MEPS_DATA_YEAR = 2021) %>% 
  mutate(ID = paste0(DUPERSID, MEPS_DATA_YEAR))

fyc20_demos <- fyc20 %>% 
  select(DUPERSID, RXTOT20, RXEXP20, RXSLF20, VARPSU, VARSTR, PERWT20F, RACETHX, 
         AGE20X, SEX, PANEL) %>% 
  rename(RXTOTYY=RXTOT20,
         RXEXPYY=RXEXP20,
         RXSLFYY=RXSLF20,
         PERWTYYF=PERWT20F,
         AGEYYX=AGE20X) %>% 
  mutate(MEPS_DATA_YEAR = 2020) %>% 
  mutate(ID = paste0(DUPERSID, MEPS_DATA_YEAR))

fyc19_demos <- fyc19 %>% 
  select(DUPERSID, RXTOT19, RXEXP19, RXSLF19, VARPSU, VARSTR, PERWT19F, RACETHX, 
         AGE19X, SEX, PANEL) %>% 
  rename(RXTOTYY=RXTOT19,
         RXEXPYY=RXEXP19,
         RXSLFYY=RXSLF19,
         PERWTYYF=PERWT19F,
         AGEYYX=AGE19X) %>% 
  mutate(MEPS_DATA_YEAR = 2019) %>% 
  mutate(ID = paste0(DUPERSID, MEPS_DATA_YEAR))

fyc_combined <- fyc19_demos %>% 
  union_all(fyc20_demos) %>% 
  union_all(fyc21_demos) %>% 
  mutate(POOLWTYY = PERWTYYF / 3) 

link_join <- link %>% 
  select(DUPERSID, PANEL, STRA9621, PSU9621)

fyc_combined <- fyc_combined %>% 
  left_join(link_join, by = c("DUPERSID", "PANEL"))

# Retrieve base file created by "match_simulated_ids.R"
final_mapping <- read_csv("./etc/summary_tables/final_mapping.csv")

# Identify person-level total utilization, OOP spend, and total spend
drugs_pers <- fyc_combined %>% 
  select(ID, DUPERSID, RXTOTYY, RXEXPYY, RXSLFYY, PSU9621, STRA9621, POOLWTYY, 
         RACETHX, AGEYYX, SEX, MEPS_DATA_YEAR) %>% 
  mutate(AGE_GRP_9 = case_when(AGEYYX < 5 ~ "Under 5",
                               AGEYYX >= 5 & AGEYYX <= 17 ~ "5 - 17",
                               AGEYYX >= 18 & AGEYYX <= 29 ~ "18 - 29",
                               AGEYYX >= 30 & AGEYYX <= 39 ~ "30 - 39",
                               AGEYYX >= 40 & AGEYYX <= 49 ~ "40 - 49",
                               AGEYYX >= 50 & AGEYYX <= 59 ~ "50 - 59",
                               AGEYYX >= 60 & AGEYYX <= 69 ~ "60 - 69",
                               AGEYYX >= 70 & AGEYYX <= 79 ~ "70 - 79",
                               AGEYYX >= 80 ~ "80 and over",
                               T ~ as.character(AGEYYX))) %>%
  mutate(RACETHX_DSC = case_when(RACETHX == 1 ~ "Hispanic",
                                 RACETHX == 2 ~ "Non-Hispanic White Only",
                                 RACETHX == 3 ~ "Non-Hispanic Black Only",
                                 RACETHX == 4 ~ "Non-Hispanic Asian Only",
                                 RACETHX == 5 ~ "Non-Hispanic Other Race Or Multiple Race",
                                 T ~ as.character(RACETHX))) %>% 
  mutate(SEX_DSC = if_else(SEX == 1, "Male", if_else(SEX == 2, "Female", "Unknown"))) %>% 
  write_csv("./etc/summary_tables/individual_drug_data.csv")

# VALIDATE each sample cut is n >= 60
attrib_cartesian_1 <- drugs_pers %>% 
  filter(POOLWTYY > 0) %>% 
  filter(AGEYYX >= 18) %>% 
  group_by(AGE_GRP_9, SEX_DSC) %>% 
  summarize(count = n())

attrib_cartesian_2 <- drugs_pers %>% 
  filter(POOLWTYY > 0) %>% 
  filter(AGEYYX >= 18) %>% 
  group_by(AGE_GRP_9, RACETHX_DSC) %>% 
  summarize(count = n())
  
# Compute estimates for Subset 1, Age >= 21 only
mean_values_subset_1 <- drugs_pers %>% 
  as_survey_design(ids = PSU9621,
                   strata = STRA9621,
                   weights = POOLWTYY,
                   nest = T) %>%
  filter(AGEYYX >= 18) %>% 
  summarize(mean_util = survey_mean(RXTOTYY, vartype=c("ci")),
            mean_oop_spend = survey_mean(RXSLFYY, vartype=c("ci")),
            mean_tot_spend = survey_mean(RXEXPYY, vartype=c("ci"))) %>% 
  write_csv("./etc/summary_tables/mean_rx_util_spend_all.csv")

# Compute estimates for Subset 2, Age >= 21 + GENDER + AGE
mean_values_subset_2 <- drugs_pers %>% 
  as_survey_design(ids = PSU9621,
                   strata = STRA9621,
                   weights = POOLWTYY,
                   nest = T) %>%
  filter(AGEYYX >= 18) %>% 
  group_by(AGE_GRP_9, SEX_DSC) %>% 
  summarize(mean_util = survey_mean(RXTOTYY, vartype=c("ci")),
            mean_oop_spend = survey_mean(RXSLFYY, vartype=c("ci")),
            mean_tot_spend = survey_mean(RXEXPYY, vartype=c("ci"))) %>% 
  write_csv("./etc/summary_tables/mean_rx_util_spend_byage,gender.csv")

# Compute estimates for Subset 3, Age>= 21 + RACE/ETH
mean_values_subset_3 <- drugs_pers %>% 
  as_survey_design(ids = PSU9621,
                   strata = STRA9621,
                   weights = POOLWTYY,
                   nest = T) %>%
  filter(AGEYYX >= 18) %>% 
  group_by(AGE_GRP_9, RACETHX_DSC) %>% 
  summarize(mean_util = survey_mean(RXTOTYY, vartype=c("ci", "se")),
            mean_oop_spend = survey_mean(RXSLFYY, vartype=c("ci", "se")),
            mean_tot_spend = survey_mean(RXEXPYY, vartype=c("ci", "se"))) %>% 
  ungroup() %>% 
  mutate(mean_util_rse = mean_util_se / mean_util,
         mean_oop_spend_rse = mean_oop_spend_se / mean_oop_spend,
         mean_tot_spend_rse = mean_tot_spend_se / mean_tot_spend) %>% 
  write_csv("./etc/summary_tables/mean_rx_util_spend_byrace,eth.csv")

# Now build a pooled drug details file to get personal level details
drugs21_mod <- drugs21 %>% 
  filter(RXNDC != -15, RXNAME != -15) %>% 
  select(DUPERSID, RXNAME, RXBEGYRX, RXRECIDX, RXNDC,
         RXSFYYX=RXSF21X, 
         RXXPYYX=RXXP21X) %>% 
  mutate(MEPS_DATA_YEAR = 2021) %>% 
  mutate(ID = paste0(DUPERSID, MEPS_DATA_YEAR))

drugs20_mod <- drugs20 %>% 
  filter(RXNDC != -15, RXNAME != -15) %>% 
  select(DUPERSID, RXNAME, RXBEGYRX, RXRECIDX, RXNDC,
         RXSFYYX=RXSF20X, 
         RXXPYYX=RXXP20X) %>% 
  mutate(MEPS_DATA_YEAR = 2020) %>% 
  mutate(ID = paste0(DUPERSID, MEPS_DATA_YEAR))

drugs19_mod <- drugs19 %>% 
  filter(RXNDC != -15, RXNAME != -15) %>% 
  select(DUPERSID, RXNAME, RXBEGYRX, RXRECIDX, RXNDC,
         RXSFYYX=RXSF19X, 
         RXXPYYX=RXXP19X) %>% 
  mutate(MEPS_DATA_YEAR = 2019) %>% 
  mutate(ID = paste0(DUPERSID, MEPS_DATA_YEAR))

drugs <- drugs21_mod %>% 
  union_all(drugs20_mod) %>% 
  union_all(drugs19_mod)

drugs_detail <- drugs %>% 
  group_by(ID, RXNAME, RXBEGYRX, MEPS_DATA_YEAR) %>% 
  summarize(count_unique = n_distinct(RXRECIDX),
            cost_oop = sum(RXSFYYX),
            cost_tot = sum(RXXPYYX)) %>% 
  ungroup() %>% 
  arrange(ID) %>% 
  mutate(cost_plan = cost_tot - cost_oop) %>% 
  mutate(diff = if_else(RXBEGYRX < 0, -1, MEPS_DATA_YEAR - RXBEGYRX),
         yrs_on_rx = case_when(
           diff == -1 ~ "Unknown",
           diff == 0 ~ "Less than a year",
           .default = paste0(diff, " to ", diff+1, " years"))) %>% 
  select(-RXBEGYRX, -diff) %>% 
  write_csv("./etc/summary_tables/drugs_detail.csv")

#### IN THIS SECTION, we will map all NDCs to RxNorm to get those details
all_ndcs <- drugs %>% 
  group_by(ID, RXNDC) %>% 
  summarize(count = n()) %>% 
  write_csv("./etc/summary_tables/drugs_detail_allNDCs.csv")



  

  


