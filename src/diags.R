# diags.R

# This script retrieves the medical conditions from the Medical Conditions MEPS
# PUF and "completes" the ICD10 code for each condition to its full, non-censored
# glory using full ICD10 frequency stats from publicly available CA datasets.

library(tidyverse) 
library(MEPS)
library(readxl)

# Retrieve base file created by "match.R"
final_mapping <- read_csv("./etc/summary_tables/final_mapping.csv")

# Download conditions files for 2019-2021
cond21 <- MEPS::read_MEPS(year = 2021, type = "COND")
cond20 <- MEPS::read_MEPS(year = 2020, type = "COND")
cond19 <- MEPS::read_MEPS(year = 2019, type = "COND")

# Filter out ICD-10 codes that are suppressed for all three files
conds_filtered21 <- cond21 %>% 
  select(DUPERSID, AGEDIAG, CONDN, ICD10CDX, CCSR1X, CCSR2X, CCSR3X, ERCOND, IPCOND, OPCOND) %>% 
  filter(ICD10CDX != -15) %>% 
  mutate(CCSR1X = if_else(CCSR1X == "-1", NA_character_, CCSR1X),
         CCSR2X = if_else(CCSR2X == "-1", NA_character_, CCSR2X),
         CCSR3X = if_else(CCSR3X == "-1", NA_character_, CCSR3X))

conds_filtered20 <- cond20 %>% 
  select(DUPERSID, AGEDIAG, CONDN, ICD10CDX, CCSR1X, CCSR2X, CCSR3X, ERNUM, IPNUM, OPNUM) %>% 
  filter(ICD10CDX != -15) %>% 
  mutate(CCSR1X = if_else(CCSR1X == "-1", NA_character_, CCSR1X),
         CCSR2X = if_else(CCSR2X == "-1", NA_character_, CCSR2X),
         CCSR3X = if_else(CCSR3X == "-1", NA_character_, CCSR3X))

conds_filtered19 <- cond19 %>% 
  select(DUPERSID, AGEDIAG, CONDN, ICD10CDX, CCSR1X, CCSR2X, CCSR3X, ERNUM, IPNUM, OPNUM) %>% 
  filter(ICD10CDX != -15) %>% 
  mutate(CCSR1X = if_else(CCSR1X == "-1", NA_character_, CCSR1X),
         CCSR2X = if_else(CCSR2X == "-1", NA_character_, CCSR2X),
         CCSR3X = if_else(CCSR3X == "-1", NA_character_, CCSR3X))
        

# Retrieve ICD10CM to CCSR mapping file - get archived v2021.2, v2020.3, and v2019.1 versions here:
# https://hcup-us.ahrq.gov/toolssoftware/ccsr/ccsr_archive.jsp#ccsr
# This code will not work without the CSV crossmap files from these versions of CCSR being present
# in the /etc/ccsr_crosswalks/ directory of this project.

input <- str_replace_all(read_file("./etc/ccsr_crosswalks/DXCCSR_v2021-2.csv"), "\'", "")
icd10_ccsr_map_2021 <- read_csv(input, guess_max = 60000) 

icd10_ccsr3_map_2021 <- icd10_ccsr_map_2021 %>% 
  select(ICD10CM = `ICD-10-CM CODE`,
         ICD10_DSC = `ICD-10-CM CODE DESCRIPTION`,
         CCSR1X = `CCSR CATEGORY 1`,
         CCSR2X = `CCSR CATEGORY 2`,
         CCSR3X = `CCSR CATEGORY 3`)

input <- str_replace_all(read_file("./etc/ccsr_crosswalks/DXCCSR_v2020-3.csv"), "\'", "")
icd10_ccsr_map_2020 <- read_csv(input, guess_max = 60000) 

icd10_ccsr3_map_2020 <- icd10_ccsr_map_2020 %>% 
  select(ICD10CM = `ICD-10-CM CODE`,
         ICD10_DSC = `ICD-10-CM CODE DESCRIPTION`,
         CCSR1X = `CCSR CATEGORY 1`,
         CCSR2X = `CCSR CATEGORY 2`,
         CCSR3X = `CCSR CATEGORY 3`)

input <- str_replace_all(read_file("./etc/ccsr_crosswalks/DXCCSR2019_1.csv"), "\'", "")
icd10_ccsr_map_2019 <- read_csv(input, guess_max = 60000) 

icd10_ccsr3_map_2019 <- icd10_ccsr_map_2019 %>% 
  select(ICD10CM = `ICD-10-CM CODE`,
         ICD10_DSC = `ICD-10-CM CODE DESCRIPTION`,
         CCSR1X = `CCSR CATEGORY 1`,
         CCSR2X = `CCSR CATEGORY 2`,
         CCSR3X = `CCSR CATEGORY 3`)

# Read in state of california ICD 10 diagnosis code frequency data for 2019 - 2021
# ED
# Inpatient not ED
# Outpatient
# All else

# Datasets available here:
# ED
# 2019: https://data.chhs.ca.gov/dataset/hospital-emergency-department-diagnosis-procedure-and-external-cause-codes/resource/9cb3e590-408b-4ad6-87ea-f6cbe5315755
# 2020: https://data.chhs.ca.gov/dataset/hospital-emergency-department-diagnosis-procedure-and-external-cause-codes/resource/d1837662-3ea0-4624-ad6b-0cfaa6a59962
# 2021: https://data.chhs.ca.gov/dataset/hospital-emergency-department-diagnosis-procedure-and-external-cause-codes/resource/1de63074-3b1c-41a6-91e2-a3b412ac3cbb

# IP
# 2019: https://data.chhs.ca.gov/dataset/hospital-inpatient-diagnosis-procedure-and-external-cause-codes/resource/4ba178f4-a8eb-4a47-a18a-27ba4d78a60a
# 2020: https://data.chhs.ca.gov/dataset/hospital-inpatient-diagnosis-procedure-and-external-cause-codes/resource/e15bdc87-520b-4e5c-a260-64cd1b89ffdf
# 2021: https://data.chhs.ca.gov/dataset/hospital-inpatient-diagnosis-procedure-and-external-cause-codes/resource/c73a6f6b-99e5-4809-8eb9-58aa8ec0f8b5

# OP
# 2019: https://data.chhs.ca.gov/dataset/ambulatory-surgery-diagnosis-procedure-and-external-cause-codes/resource/eb261dbb-700c-4586-8ce7-db8c5598d98b
# 2020: https://data.chhs.ca.gov/dataset/ambulatory-surgery-diagnosis-procedure-and-external-cause-codes/resource/f6e6bf74-a969-4ae4-8a88-bbea7cd990cf
# 2021: https://data.chhs.ca.gov/dataset/ambulatory-surgery-diagnosis-procedure-and-external-cause-codes/resource/9af34427-c55f-4059-b73e-96f890631571

# These should be downloaded as XLSX files and placed in the project's ./etc/icd10_freqs/ directory before continuing.

# We will separate these claims based on these cuts, in these orders. Then use the ICD10 frequency distribution data 
# from California within these cuts to assign completed, non-edited ICD10 codes to each individual using frequency percentages
# in each truncated ICD10 category as probability weights for code completion.

# In some cases this may give individuals a mix of conditions that don't make sense, but this might be a limited concern. TBD!

# ED
ca_icd10_freqs_ed_2021 <- read_xlsx("./etc/icd10_freqs/2021_diagnosiscodefrequencies_ed.xlsx",
                               sheet="Diagnosis Code Frequencies 2021") %>% 
  mutate(ICD10CM = str_remove(ICDCMCode, "[:punct:]")) %>% 
  mutate(ICD10CDX = str_sub(ICD10CM, 1, 3)) %>% 
  inner_join(icd10_ccsr3_map_2021, by=c("ICD10CM"="ICD10CM")) 

ca_icd10_freqs_ed_2020 <- read_xlsx("./etc/icd10_freqs/2020_diagnosiscodefrequencies_ed.xlsx",
                                    sheet="Diagnosis Code Frequencies 2020") %>% 
  mutate(ICD10CM = str_remove(ICDCMCode, "[:punct:]")) %>% 
  mutate(ICD10CDX = str_sub(ICD10CM, 1, 3)) %>% 
  inner_join(icd10_ccsr3_map_2020, by=c("ICD10CM"="ICD10CM")) 

ca_icd10_freqs_ed_2019 <- read_xlsx("./etc/icd10_freqs/2019_diagnosiscodefrequencies_ed.xlsx",
                                    sheet="Diagnosis Code Frequencies 2019") %>% 
  mutate(ICD10CM = str_remove(ICDCMCode, "[:punct:]")) %>% 
  mutate(ICD10CDX = str_sub(ICD10CM, 1, 3)) %>% 
  inner_join(icd10_ccsr3_map_2020, by=c("ICD10CM"="ICD10CM")) %>% 
  select(-PrimaryDiag, -SecondDiag)

# Now, for each person and each condition of that person, we'll join on the left 3 of the icd10 code PLUS match up 
# against all listed and specified CCSRs, so this join will exclude ICD10 possiblities where the CCSRs aren't aligned.

conds_completed_ed_2021 <- conds_filtered21 %>% 
  filter(ERCOND == 1) %>% 
  left_join(ca_icd10_freqs_ed_2021, relationship = "many-to-many") %>% 
  select(DUPERSID, CONDN, CCSR1X, CCSR2X, CCSR3X, ICD10CDX, ICDCMCode, ICD10_DSC, TotalDiag, ICD10CM) %>% 
  group_by(DUPERSID, CONDN, CCSR1X, CCSR2X, CCSR3X, ICD10CDX) %>% 
  mutate(TotalDiagPct = TotalDiag / sum(TotalDiag),
         rand = runif(1, min = 0, max = 1),
         cpd = cumsum(TotalDiagPct)) %>% 
  mutate(lag_cpd = lag(cpd),
         cpd_lo = if_else(is.na(lag_cpd), 0, lag_cpd),
         cpd_hi = cpd) %>% 
  ungroup() %>% 
  mutate(selected = between(rand, cpd_lo, cpd_hi)) %>% 
  filter(selected == T) %>% 
  select(DUPERSID, CONDN, ICDCMCode, ICD10_DSC, ICD10CM)

conds_completed_ed_2020 <- conds_filtered20 %>% 
  filter(ERNUM > 0) %>% 
  left_join(ca_icd10_freqs_ed_2020, relationship = "many-to-many") %>% 
  select(DUPERSID, CONDN, CCSR1X, CCSR2X, CCSR3X, ICD10CDX, ICDCMCode, ICD10_DSC, TotalDiag, ICD10CM) %>% 
  group_by(DUPERSID, CONDN, CCSR1X, CCSR2X, CCSR3X, ICD10CDX) %>% 
  mutate(TotalDiagPct = TotalDiag / sum(TotalDiag),
         rand = runif(1, min = 0, max = 1),
         cpd = cumsum(TotalDiagPct)) %>% 
  mutate(lag_cpd = lag(cpd),
         cpd_lo = if_else(is.na(lag_cpd), 0, lag_cpd),
         cpd_hi = cpd) %>% 
  ungroup() %>% 
  mutate(selected = between(rand, cpd_lo, cpd_hi)) %>% 
  filter(selected == T) %>% 
  select(DUPERSID, CONDN, ICDCMCode, ICD10_DSC, ICD10CM)

conds_completed_ed_2019 <- conds_filtered19 %>% 
  filter(ERNUM > 0) %>% 
  left_join(ca_icd10_freqs_ed_2019, relationship = "many-to-many") %>% 
  select(DUPERSID, CONDN, CCSR1X, CCSR2X, CCSR3X, ICD10CDX, ICDCMCode, ICD10_DSC, TotalDiag, ICD10CM) %>% 
  group_by(DUPERSID, CONDN, CCSR1X, CCSR2X, CCSR3X, ICD10CDX) %>% 
  mutate(TotalDiagPct = TotalDiag / sum(TotalDiag),
         rand = runif(1, min = 0, max = 1),
         cpd = cumsum(TotalDiagPct)) %>% 
  mutate(lag_cpd = lag(cpd),
         cpd_lo = if_else(is.na(lag_cpd), 0, lag_cpd),
         cpd_hi = cpd) %>% 
  ungroup() %>% 
  mutate(selected = between(rand, cpd_lo, cpd_hi)) %>% 
  filter(selected == T) %>% 
  select(DUPERSID, CONDN, ICDCMCode, ICD10_DSC, ICD10CM) 

# Repeat this process for hospital inpatient conditions that are non-ED
ca_icd10_freqs_ip_2021 <- read_xlsx("./etc/icd10_freqs/2021_diagnosiscodefrequencies_pdd.xlsx",
                               sheet="ICD-10-CM") %>% 
  mutate(ICD10CM = str_remove(ICDCMCode, "[:punct:]")) %>% 
  mutate(ICD10CDX = str_sub(ICD10CM, 1, 3)) %>% 
  inner_join(icd10_ccsr3_map_2021, by=c("ICD10CM"="ICD10CM")) 

ca_icd10_freqs_ip_2020 <- read_xlsx("./etc/icd10_freqs/2020_diagnosiscodefrequencies_pdd.xlsx",
                                    sheet="ICD-10-CM") %>% 
  mutate(ICD10CM = str_remove(ICDCMCode, "[:punct:]")) %>% 
  mutate(ICD10CDX = str_sub(ICD10CM, 1, 3)) %>% 
  inner_join(icd10_ccsr3_map_2020, by=c("ICD10CM"="ICD10CM")) 

ca_icd10_freqs_ip_2019 <- read_xlsx("./etc/icd10_freqs/2019_diagnosiscodefrequencies_pdd.xlsx",
                                    sheet="ICD-10-CM") %>% 
  mutate(ICD10CM = str_remove(ICDCMCode, "[:punct:]")) %>% 
  mutate(ICD10CDX = str_sub(ICD10CM, 1, 3)) %>% 
  inner_join(icd10_ccsr3_map_2020, by=c("ICD10CM"="ICD10CM")) %>% 
  select(-PrimaryDiag, -SecondDiag)

conds_completed_ip_2021 <- conds_filtered21 %>% 
  filter(ERCOND != 1, IPCOND == 1) %>% 
  left_join(ca_icd10_freqs_ip_2021, relationship="many-to-many") %>% 
  select(DUPERSID, CONDN, CCSR1X, CCSR2X, CCSR3X, ICD10CDX, ICDCMCode, ICD10_DSC, TotalDiag, ICD10CM) %>% 
  group_by(DUPERSID, CONDN, CCSR1X, CCSR2X, CCSR3X, ICD10CDX) %>% 
  mutate(TotalDiagPct = TotalDiag / sum(TotalDiag),
         rand = runif(1, min = 0, max = 1),
         cpd = cumsum(TotalDiagPct)) %>% 
  mutate(lag_cpd = lag(cpd),
         cpd_lo = if_else(is.na(lag_cpd), 0, lag_cpd),
         cpd_hi = cpd) %>% 
  ungroup() %>% 
  mutate(selected = between(rand, cpd_lo, cpd_hi)) %>% 
  filter(selected == T) %>% 
  select(DUPERSID, CONDN, ICDCMCode, ICD10_DSC, ICD10CM) 

conds_completed_ip_2020 <- conds_filtered20 %>% 
  filter(ERNUM == 0, IPNUM > 0) %>% 
  left_join(ca_icd10_freqs_ip_2020, relationship="many-to-many") %>% 
  select(DUPERSID, CONDN, CCSR1X, CCSR2X, CCSR3X, ICD10CDX, ICDCMCode, ICD10_DSC, TotalDiag, ICD10CM) %>% 
  group_by(DUPERSID, CONDN, CCSR1X, CCSR2X, CCSR3X, ICD10CDX) %>% 
  mutate(TotalDiagPct = TotalDiag / sum(TotalDiag),
         rand = runif(1, min = 0, max = 1),
         cpd = cumsum(TotalDiagPct)) %>% 
  mutate(lag_cpd = lag(cpd),
         cpd_lo = if_else(is.na(lag_cpd), 0, lag_cpd),
         cpd_hi = cpd) %>% 
  ungroup() %>% 
  mutate(selected = between(rand, cpd_lo, cpd_hi)) %>% 
  filter(selected == T) %>% 
  select(DUPERSID, CONDN, ICDCMCode, ICD10_DSC, ICD10CM) 

conds_completed_ip_2019 <- conds_filtered19 %>% 
  filter(ERNUM == 0, IPNUM > 0) %>% 
  left_join(ca_icd10_freqs_ip_2019, relationship="many-to-many") %>% 
  select(DUPERSID, CONDN, CCSR1X, CCSR2X, CCSR3X, ICD10CDX, ICDCMCode, ICD10_DSC, TotalDiag, ICD10CM) %>% 
  group_by(DUPERSID, CONDN, CCSR1X, CCSR2X, CCSR3X, ICD10CDX) %>% 
  mutate(TotalDiagPct = TotalDiag / sum(TotalDiag),
         rand = runif(1, min = 0, max = 1),
         cpd = cumsum(TotalDiagPct)) %>% 
  mutate(lag_cpd = lag(cpd),
         cpd_lo = if_else(is.na(lag_cpd), 0, lag_cpd),
         cpd_hi = cpd) %>% 
  ungroup() %>% 
  mutate(selected = between(rand, cpd_lo, cpd_hi)) %>% 
  filter(selected == T) %>% 
  select(DUPERSID, CONDN, ICDCMCode, ICD10_DSC, ICD10CM) 

# And now do ambulatory for conditions that are non-ED, non-IP
ca_icd10_freqs_op_2021 <- read_xlsx("./etc/icd10_freqs/2021_diagnosiscodefrequencies_as.xlsx",
                               sheet="Diagnosis Code 2021") %>% 
  mutate(ICD10CM = str_remove(ICDCMCode, "[:punct:]")) %>% 
  mutate(ICD10CDX = str_sub(ICD10CM, 1, 3)) %>% 
  inner_join(icd10_ccsr3_map_2021, by=c("ICD10CM"="ICD10CM")) 

ca_icd10_freqs_op_2020 <- read_xlsx("./etc/icd10_freqs/2020_diagnosiscodefrequencies_as.xlsx",
                                    sheet="Diagnosis Code 2020") %>% 
  mutate(ICD10CM = str_remove(ICDCMCode, "[:punct:]")) %>% 
  mutate(ICD10CDX = str_sub(ICD10CM, 1, 3)) %>% 
  inner_join(icd10_ccsr3_map_2020, by=c("ICD10CM"="ICD10CM")) 

ca_icd10_freqs_op_2019 <- read_xlsx("./etc/icd10_freqs/2019_diagnosiscodefrequencies_as.xlsx",
                                    sheet="Diagnosis Codes 2019") %>% 
  mutate(ICD10CM = str_remove(ICDCMCode, "[:punct:]")) %>% 
  mutate(ICD10CDX = str_sub(ICD10CM, 1, 3)) %>% 
  inner_join(icd10_ccsr3_map_2020, by=c("ICD10CM"="ICD10CM")) %>% 
  select(-PrimaryDiag, -SecondDiag)

conds_completed_op_2021 <- conds_filtered21 %>% 
  filter(ERCOND != 1, IPCOND != 1, OPCOND == 1) %>% 
  left_join(ca_icd10_freqs_op_2021, relationship = "many-to-many") %>% 
  select(DUPERSID, CONDN, CCSR1X, CCSR2X, CCSR3X, ICD10CDX, ICDCMCode, ICD10_DSC, TotalDiag, ICD10CM) %>% 
  group_by(DUPERSID, CONDN, CCSR1X, CCSR2X, CCSR3X, ICD10CDX) %>% 
  mutate(TotalDiagPct = TotalDiag / sum(TotalDiag),
         rand = runif(1, min = 0, max = 1),
         cpd = cumsum(TotalDiagPct)) %>% 
  mutate(lag_cpd = lag(cpd),
         cpd_lo = if_else(is.na(lag_cpd), 0, lag_cpd),
         cpd_hi = cpd) %>% 
  ungroup() %>% 
  mutate(selected = between(rand, cpd_lo, cpd_hi)) %>% 
  filter(selected == T) %>% 
  select(DUPERSID, CONDN, ICDCMCode, ICD10_DSC, ICD10CM) 

conds_completed_op_2020 <- conds_filtered20 %>% 
  filter(ERNUM == 0, IPNUM == 0, OPNUM > 0) %>% 
  left_join(ca_icd10_freqs_op_2020, relationship = "many-to-many") %>% 
  select(DUPERSID, CONDN, CCSR1X, CCSR2X, CCSR3X, ICD10CDX, ICDCMCode, ICD10_DSC, TotalDiag, ICD10CM) %>% 
  group_by(DUPERSID, CONDN, CCSR1X, CCSR2X, CCSR3X, ICD10CDX) %>% 
  mutate(TotalDiagPct = TotalDiag / sum(TotalDiag),
         rand = runif(1, min = 0, max = 1),
         cpd = cumsum(TotalDiagPct)) %>% 
  mutate(lag_cpd = lag(cpd),
         cpd_lo = if_else(is.na(lag_cpd), 0, lag_cpd),
         cpd_hi = cpd) %>% 
  ungroup() %>% 
  mutate(selected = between(rand, cpd_lo, cpd_hi)) %>% 
  filter(selected == T) %>% 
  select(DUPERSID, CONDN, ICDCMCode, ICD10_DSC, ICD10CM) 

conds_completed_op_2019 <- conds_filtered19 %>% 
  filter(ERNUM == 0, IPNUM == 0, OPNUM > 0) %>% 
  left_join(ca_icd10_freqs_op_2019, relationship = "many-to-many") %>% 
  select(DUPERSID, CONDN, CCSR1X, CCSR2X, CCSR3X, ICD10CDX, ICDCMCode, ICD10_DSC, TotalDiag, ICD10CM) %>% 
  group_by(DUPERSID, CONDN, CCSR1X, CCSR2X, CCSR3X, ICD10CDX) %>% 
  mutate(TotalDiagPct = TotalDiag / sum(TotalDiag),
         rand = runif(1, min = 0, max = 1),
         cpd = cumsum(TotalDiagPct)) %>% 
  mutate(lag_cpd = lag(cpd),
         cpd_lo = if_else(is.na(lag_cpd), 0, lag_cpd),
         cpd_hi = cpd) %>% 
  ungroup() %>% 
  mutate(selected = between(rand, cpd_lo, cpd_hi)) %>% 
  filter(selected == T) %>% 
  select(DUPERSID, CONDN, ICDCMCode, ICD10_DSC, ICD10CM) 

# We have some stragglers, so we will now identify those and complete ICD-10 codes with less scrutiny - all of this is probalby overkill anyways
conds_completed_so_far_2021 <- conds_completed_ed_2021 %>% 
  union_all(conds_completed_ip_2021) %>% 
  union_all(conds_completed_op_2021) 

conds_completed_so_far_2020 <- conds_completed_ed_2020 %>% 
  union_all(conds_completed_ip_2020) %>% 
  union_all(conds_completed_op_2020) 

conds_completed_so_far_2019 <- conds_completed_ed_2019 %>% 
  union_all(conds_completed_ip_2019) %>% 
  union_all(conds_completed_op_2019) 

conds_filtered_incomplete_round1_2021 <- conds_filtered21 %>% 
  anti_join(conds_completed_so_far_2021)

conds_filtered_incomplete_round1_2020 <- conds_filtered20 %>% 
  anti_join(conds_completed_so_far_2020)

conds_filtered_incomplete_round1_2019 <- conds_filtered19 %>% 
  anti_join(conds_completed_so_far_2019)

# We'll complete these using a complete union of all the ICD10 code distribution files
ca_icd10_freqs_combined_2021 <- ca_icd10_freqs_ed_2021 %>% 
  union_all(ca_icd10_freqs_ip_2021) %>% 
  union_all(ca_icd10_freqs_op_2021) %>% 
  group_by(ICDCMCode, ICD10_DSC, ICD10CM, ICD10CDX, CCSR1X, CCSR2X, CCSR3X) %>% 
  summarize(TotalDiag = sum(TotalDiag)) %>% 
  ungroup() %>% 
  relocate(TotalDiag, .after=ICD10_DSC)

ca_icd10_freqs_combined_2020 <- ca_icd10_freqs_ed_2020 %>% 
  union_all(ca_icd10_freqs_ip_2020) %>% 
  union_all(ca_icd10_freqs_op_2020) %>% 
  group_by(ICDCMCode, ICD10_DSC, ICD10CM, ICD10CDX, CCSR1X, CCSR2X, CCSR3X) %>% 
  summarize(TotalDiag = sum(TotalDiag)) %>% 
  ungroup() %>% 
  relocate(TotalDiag, .after=ICD10_DSC)

ca_icd10_freqs_combined_2019 <- ca_icd10_freqs_ed_2019 %>% 
  union_all(ca_icd10_freqs_ip_2019) %>% 
  union_all(ca_icd10_freqs_op_2019) %>% 
  group_by(ICDCMCode, ICD10_DSC, ICD10CM, ICD10CDX, CCSR1X, CCSR2X, CCSR3X) %>% 
  summarize(TotalDiag = sum(TotalDiag)) %>% 
  ungroup() %>% 
  relocate(TotalDiag, .after=ICD10_DSC)

# And now we'll apply this to our straggler conditions revealed by the anti-join above, first using CCSRs to help differentiate
conds_completed_round2_2021 <- conds_filtered_incomplete_round1_2021 %>% 
  left_join(ca_icd10_freqs_combined_2021, relationship="many-to-many") %>% 
  select(DUPERSID, CONDN, CCSR1X, CCSR2X, CCSR3X, ICD10CDX, ICDCMCode, ICD10_DSC, TotalDiag, ICD10CM) %>% 
  group_by(DUPERSID, CONDN, CCSR1X, CCSR2X, CCSR3X, ICD10CDX) %>% 
  mutate(TotalDiagPct = TotalDiag / sum(TotalDiag),
         rand = runif(1, min = 0, max = 1),
         cpd = cumsum(TotalDiagPct)) %>% 
  mutate(lag_cpd = lag(cpd),
         cpd_lo = if_else(is.na(lag_cpd), 0, lag_cpd),
         cpd_hi = cpd) %>% 
  ungroup() %>% 
  mutate(selected = between(rand, cpd_lo, cpd_hi)) %>% 
  filter(selected == T) %>% 
  select(DUPERSID, CONDN, ICDCMCode, ICD10_DSC, ICD10CM) 

conds_completed_round2_2020 <- conds_filtered_incomplete_round1_2020 %>% 
  left_join(ca_icd10_freqs_combined_2020, relationship="many-to-many") %>% 
  select(DUPERSID, CONDN, CCSR1X, CCSR2X, CCSR3X, ICD10CDX, ICDCMCode, ICD10_DSC, TotalDiag, ICD10CM) %>% 
  group_by(DUPERSID, CONDN, CCSR1X, CCSR2X, CCSR3X, ICD10CDX) %>% 
  mutate(TotalDiagPct = TotalDiag / sum(TotalDiag),
         rand = runif(1, min = 0, max = 1),
         cpd = cumsum(TotalDiagPct)) %>% 
  mutate(lag_cpd = lag(cpd),
         cpd_lo = if_else(is.na(lag_cpd), 0, lag_cpd),
         cpd_hi = cpd) %>% 
  ungroup() %>% 
  mutate(selected = between(rand, cpd_lo, cpd_hi)) %>% 
  filter(selected == T) %>% 
  select(DUPERSID, CONDN, ICDCMCode, ICD10_DSC, ICD10CM) 

conds_completed_round2_2019 <- conds_filtered_incomplete_round1_2019 %>% 
  left_join(ca_icd10_freqs_combined_2019, relationship="many-to-many") %>% 
  select(DUPERSID, CONDN, CCSR1X, CCSR2X, CCSR3X, ICD10CDX, ICDCMCode, ICD10_DSC, TotalDiag, ICD10CM) %>% 
  group_by(DUPERSID, CONDN, CCSR1X, CCSR2X, CCSR3X, ICD10CDX) %>% 
  mutate(TotalDiagPct = TotalDiag / sum(TotalDiag),
         rand = runif(1, min = 0, max = 1),
         cpd = cumsum(TotalDiagPct)) %>% 
  mutate(lag_cpd = lag(cpd),
         cpd_lo = if_else(is.na(lag_cpd), 0, lag_cpd),
         cpd_hi = cpd) %>% 
  ungroup() %>% 
  mutate(selected = between(rand, cpd_lo, cpd_hi)) %>% 
  filter(selected == T) %>% 
  select(DUPERSID, CONDN, ICDCMCode, ICD10_DSC, ICD10CM) 

# Adding this back to the primary conditions files, we'll compute our final stragglers
conds_completed_so_far_2021 <- conds_completed_ed_2021 %>% 
  union_all(conds_completed_ip_2021) %>% 
  union_all(conds_completed_op_2021) %>% 
  union_all(conds_completed_round2_2021)

conds_completed_so_far_2020 <- conds_completed_ed_2020 %>% 
  union_all(conds_completed_ip_2020) %>% 
  union_all(conds_completed_op_2020) %>% 
  union_all(conds_completed_round2_2020)

conds_completed_so_far_2019 <- conds_completed_ed_2019 %>% 
  union_all(conds_completed_ip_2019) %>% 
  union_all(conds_completed_op_2019) %>% 
  union_all(conds_completed_round2_2019)

# Evaluate how many conditions we've left on the table for completion after all this joining hell.
conds_filtered_incomplete_round2_2021 <- conds_filtered21 %>% 
  anti_join(conds_completed_so_far_2021)

conds_filtered_incomplete_round2_2020 <- conds_filtered20 %>% 
  anti_join(conds_completed_so_far_2020)

conds_filtered_incomplete_round2_2019 <- conds_filtered19 %>% 
  anti_join(conds_completed_so_far_2019)

# This captures almost all conditions - fewer than 100 are associated with a complete CD10CM code. They're 
# All Z21 codes which I don't want to complete anyways, because they're heavily condition specific.

# Create display table for Medical Conditions tab
# List all diagnoses, along with CCSR description in comma separated list, how long ago diagnosed, and whether it's
# a chronic condition

# CCIR Crosswalk must be placed in the ./etc/ccir_crosswalks/ folder
# It can be obtained at AHRQ's website:
# https://hcup-us.ahrq.gov/toolssoftware/chronic_icd10/chronic_icd10_archive.jsp

# Load CCIR crosswalk
ccir_icd10_map <- read_csv(str_replace_all(read_file("./etc/ccir_crosswalks/CCI_ICD10CM_v2021-1.csv"), "\'", ""), guess_max = 60000,
                           skip = 2) %>% 
  select(ICD10CM = `ICD-10-CM CODE`,
         CHRONIC_IND = `CHRONIC INDICATOR`) %>% 
  mutate(CHRONIC_IND = case_when(
    CHRONIC_IND == "A" ~ "Acute",
    CHRONIC_IND == "C" ~ "Chronic",
    CHRONIC_IND == "B" ~ "Both",
    CHRONIC_IND == "N" ~ "N/A"
  ))

icd10_ccsr3_dsc_map_2021 <- icd10_ccsr_map_2021 %>% 
  select(ICD10CM = `ICD-10-CM CODE`,
         ICD10_DSC = `ICD-10-CM CODE DESCRIPTION`,
         CCSR1X = `CCSR CATEGORY 1`,
         CCSR2X = `CCSR CATEGORY 2`,
         CCSR3X = `CCSR CATEGORY 3`,
         CCSR1X_DSC = `CCSR CATEGORY 1 DESCRIPTION`,
         CCSR2X_DSC = `CCSR CATEGORY 2 DESCRIPTION`,
         CCSR3X_DSC = `CCSR CATEGORY 3 DESCRIPTION`)

icd10_ccsr3_dsc_map_2020 <- icd10_ccsr_map_2020 %>% 
  select(ICD10CM = `ICD-10-CM CODE`,
         ICD10_DSC = `ICD-10-CM CODE DESCRIPTION`,
         CCSR1X = `CCSR CATEGORY 1`,
         CCSR2X = `CCSR CATEGORY 2`,
         CCSR3X = `CCSR CATEGORY 3`,
         CCSR1X_DSC = `CCSR CATEGORY 1 DESCRIPTION`,
         CCSR2X_DSC = `CCSR CATEGORY 2 DESCRIPTION`,
         CCSR3X_DSC = `CCSR CATEGORY 3 DESCRIPTION`)

icd10_ccsr3_dsc_map_2019 <- icd10_ccsr3_dsc_map_2020 # Ask me about it, it's complicated.
  
dx_detail_2021 <- conds_filtered21 %>% 
  inner_join(conds_completed_so_far_2021) %>% 
  inner_join(icd10_ccsr3_dsc_map_2021) %>% 
  left_join(ccir_icd10_map) %>% 
  mutate(CHRONIC_IND = replace_na(CHRONIC_IND, "N/A")) %>% 
  mutate(CCSR1X_DSC = replace_na(CCSR1X_DSC, ""),
         CCSR2X_DSC = replace_na(CCSR2X_DSC, ""),
         CCSR3X_DSC = replace_na(CCSR3X_DSC, "")) %>% 
  mutate(CCSR_DSC = str_squish(paste(CCSR1X_DSC, CCSR2X_DSC, CCSR3X_DSC, sep="\n"))) %>% 
  mutate(Age_Diag = if_else(AGEDIAG < 0, "Unknown", as.character(AGEDIAG))) %>% 
  select(DUPERSID, ICD10CM_Pretty = ICDCMCode, ICD10CM, ICD10_DSC, CCSR_DSC, CHRONIC_IND, Age_Diag) %>% 
  mutate(DUPERSID = paste0(DUPERSID, 2021))

dx_detail_2020 <- conds_filtered20 %>% 
  inner_join(conds_completed_so_far_2020) %>% 
  inner_join(icd10_ccsr3_dsc_map_2020) %>% 
  left_join(ccir_icd10_map) %>% 
  mutate(CHRONIC_IND = replace_na(CHRONIC_IND, "N/A")) %>% 
  mutate(CCSR1X_DSC = replace_na(CCSR1X_DSC, ""),
         CCSR2X_DSC = replace_na(CCSR2X_DSC, ""),
         CCSR3X_DSC = replace_na(CCSR3X_DSC, "")) %>% 
  mutate(CCSR_DSC = str_squish(paste(CCSR1X_DSC, CCSR2X_DSC, CCSR3X_DSC, sep="\n"))) %>% 
  mutate(Age_Diag = if_else(AGEDIAG < 0, "Unknown", as.character(AGEDIAG))) %>% 
  select(DUPERSID, ICD10CM_Pretty = ICDCMCode, ICD10CM, ICD10_DSC, CCSR_DSC, CHRONIC_IND, Age_Diag) %>% 
  mutate(DUPERSID = paste0(DUPERSID, 2020))

dx_detail_2019 <- conds_filtered19 %>% 
  inner_join(conds_completed_so_far_2019) %>% 
  inner_join(icd10_ccsr3_dsc_map_2019) %>% 
  left_join(ccir_icd10_map) %>% 
  mutate(CHRONIC_IND = replace_na(CHRONIC_IND, "N/A")) %>% 
  mutate(CCSR1X_DSC = replace_na(CCSR1X_DSC, ""),
         CCSR2X_DSC = replace_na(CCSR2X_DSC, ""),
         CCSR3X_DSC = replace_na(CCSR3X_DSC, "")) %>% 
  mutate(CCSR_DSC = str_squish(paste(CCSR1X_DSC, CCSR2X_DSC, CCSR3X_DSC, sep="\n"))) %>% 
  mutate(Age_Diag = if_else(AGEDIAG < 0, "Unknown", as.character(AGEDIAG))) %>% 
  select(DUPERSID, ICD10CM_Pretty = ICDCMCode, ICD10CM, ICD10_DSC, CCSR_DSC, CHRONIC_IND, Age_Diag) %>% 
  mutate(DUPERSID = paste0(DUPERSID, 2019))

dx_detail <- dx_detail_2021 %>% 
  union_all(dx_detail_2020) %>% 
  union_all(dx_detail_2019) %>% 
  write_csv("./etc/summary_tables/dx_detail.csv") 
