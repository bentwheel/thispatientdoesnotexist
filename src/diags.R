# Basic staging of medical conditions

library(tidyverse) 
library(MEPS)
library(survey)
library(srvyr)
library(readxl)

# Download drug file
conditions = MEPS::read_MEPS(year = 2021, type = "COND")

# Filter on ICD-10 codes that are suppressed
conds_filtered <- conditions %>% 
  select(DUPERSID, AGEDIAG, CONDN, ICD10CDX, CCSR1X, CCSR2X, CCSR3X, ERCOND, IPCOND, OPCOND) %>% 
  filter(ICD10CDX != -15) %>% 
  mutate(CCSR1X = if_else(CCSR1X == "-1", NA_character_, CCSR1X),
         CCSR2X = if_else(CCSR2X == "-1", NA_character_, CCSR2X),
         CCSR3X = if_else(CCSR3X == "-1", NA_character_, CCSR3X))
         
# Retrieve base file created by "match_simulated_ids.R"
final_mapping <- read_csv("./summary_tables/final_mapping.csv")

# Retrieve ICD10CM to CCSR mapping file - get archived v2021.2 version here:
# https://hcup-us.ahrq.gov/toolssoftware/ccsr/ccsr_archive.jsp#ccsr
input <- str_replace_all(read_file("DXCCSR_v2021-2.csv"), "\'", "")
icd10_ccsr_map_2021 <- read_csv(input, guess_max = 60000) 

icd10_ccsr3_map <- icd10_ccsr_map_2021 %>% 
  select(ICD10CM = `ICD-10-CM CODE`,
         ICD10_DSC = `ICD-10-CM CODE DESCRIPTION`,
         CCSR1X = `CCSR CATEGORY 1`,
         CCSR2X = `CCSR CATEGORY 2`,
         CCSR3X = `CCSR CATEGORY 3`)

# Read in state of california ICD 10 diagnosis code frequency data for:
# ED
# Inpatient not ED
# Outpatient
# All else

# We will separate these claims based on these cuts, in these orders. Then use the ICD10 frequency distribution data 
# from California within these cuts to assign completed, non-edited ICD10 codes to each individual using frequency percentages
# in each truncated ICD10 category as probability weights for code completion.

# In some cases this may give individuals a mix of conditions that don't make sense, but this might be a limited concern. TBD!

ca_icd10_freqs_ed <- read_xlsx("2021_diagnosiscodefrequencies_ed.xlsx",
                               sheet="Diagnosis Code Frequencies 2021") %>% 
  mutate(ICD10CM = str_remove(ICDCMCode, "[:punct:]")) %>% 
  mutate(ICD10CDX = str_sub(ICD10CM, 1, 3)) %>% 
  inner_join(icd10_ccsr3_map, by=c("ICD10CM"="ICD10CM")) 

# Now, for each person and each condition of that person, we'll join on the left 3 of the icd10 code PLUS match up 
# against all listed and specified CCSRs, so this join will exclude ICD10 possiblities where the CCSRs aren't aligned.

conds_completed_ed <- conds_filtered %>% 
  filter(ERCOND == 1) %>% 
  left_join(ca_icd10_freqs_ed) %>% 
  select(DUPERSID, CONDN, CCSR1X, CCSR2X, CCSR3X, ICD10CDX, ICDCMCode, DiagnosisDesc, TotalDiag, ICD10CM) %>% 
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
  select(DUPERSID, CONDN, ICDCMCode, DiagnosisDesc, ICD10CM)

# Repeat this process for hospital inpatient conditions that are non-ED
ca_icd10_freqs_ip <- read_xlsx("2021_diagnosiscodefrequencies_pdd.xlsx",
                               sheet="ICD-10-CM") %>% 
  mutate(ICD10CM = str_remove(ICDCMCode, "[:punct:]")) %>% 
  mutate(ICD10CDX = str_sub(ICD10CM, 1, 3)) %>% 
  left_join(icd10_ccsr3_map, by=c("ICD10CM"="ICD10CM")) 

conds_completed_ip <- conds_filtered %>% 
  filter(ERCOND != 1, IPCOND == 1) %>% 
  left_join(ca_icd10_freqs_ip) %>% 
  select(DUPERSID, CONDN, CCSR1X, CCSR2X, CCSR3X, ICD10CDX, ICDCMCode, DiagnosisDesc, TotalDiag, ICD10CM) %>% 
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
  select(DUPERSID, CONDN, ICDCMCode, DiagnosisDesc, ICD10CM) 

# And now do ambulatory for conditions that are non-ED, non-IP
ca_icd10_freqs_op <- read_xlsx("2021_diagnosiscodefrequencies_as.xlsx",
                               sheet="Diagnosis Code 2021") %>% 
  mutate(ICD10CM = str_remove(ICDCMCode, "[:punct:]")) %>% 
  mutate(ICD10CDX = str_sub(ICD10CM, 1, 3)) %>% 
  left_join(icd10_ccsr3_map, by=c("ICD10CM"="ICD10CM")) 

conds_completed_op <- conds_filtered %>% 
  filter(ERCOND != 1, IPCOND != 1, OPCOND == 1) %>% 
  left_join(ca_icd10_freqs_op) %>% 
  select(DUPERSID, CONDN, CCSR1X, CCSR2X, CCSR3X, ICD10CDX, ICDCMCode, DiagnosisDesc, TotalDiag, ICD10CM) %>% 
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
  select(DUPERSID, CONDN, ICDCMCode, DiagnosisDesc, ICD10CM) 

# We have some stragglers, so we will now identify those and complete ICD-10 codes with less scrutiny - all of this is probalby overkill anyways
conds_completed_so_far <- conds_completed_ed %>% 
  union_all(conds_completed_ip) %>% 
  union_all(conds_completed_op) 

conds_filtered_incomplete_round1 <- conds_filtered %>% 
  anti_join(conds_completed_so_far)

# We'll complete these using a complete union of all the ICD10 code distribution files
ca_icd10_freqs_combined <- ca_icd10_freqs_ed %>% 
  union_all(ca_icd10_freqs_ip) %>% 
  union_all(ca_icd10_freqs_op) %>% 
  group_by(ICDCMCode, DiagnosisDesc, ICD10CM, ICD10CDX, CCSR1X, CCSR2X, CCSR3X) %>% 
  summarize(TotalDiag = sum(TotalDiag)) %>% 
  ungroup() %>% 
  relocate(TotalDiag, .after=DiagnosisDesc)

# And now we'll apply this to our straggler conditions revealed by the anti-join above, first using CCSRs to help differentiate
conds_completed_round2 <- conds_filtered_incomplete_round1 %>% 
  left_join(ca_icd10_freqs_combined) %>% 
  select(DUPERSID, CONDN, CCSR1X, CCSR2X, CCSR3X, ICD10CDX, ICDCMCode, DiagnosisDesc, TotalDiag, ICD10CM) %>% 
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
  select(DUPERSID, CONDN, ICDCMCode, DiagnosisDesc, ICD10CM)

# Adding this back to the primary conditions files, we'll compute our final stragglers
conds_completed_so_far <- conds_completed_ed %>% 
  union_all(conds_completed_ip) %>% 
  union_all(conds_completed_op) %>% 
  union_all(conds_completed_round2)

conds_filtered_incomplete_round2 <- conds_filtered %>% 
  anti_join(conds_completed_so_far)

# This captures almost all conditions - fewer than 100 are associated with a complete CD10CM code. They're 
# All Z21 codes which I don't want to complete anyways, because they're heavily condition sspecific.

# Create display table for Medical Conditions tab
# List all diagnoses, along with CCSR description in comma separated list, how long ago diagnosed, and whether it's
# a chronic condition

# Load CCIR crosswalk
ccir_icd10_map <- read_csv(str_replace_all(read_file("CCI_ICD10CM_v2021-1.csv"), "\'", ""), guess_max = 60000,
                           skip = 2) %>% 
  select(ICD10CM = `ICD-10-CM CODE`,
         CHRONIC_IND = `CHRONIC INDICATOR`) %>% 
  mutate(CHRONIC_IND = case_when(
    CHRONIC_IND == "A" ~ "Acute",
    CHRONIC_IND == "C" ~ "Chronic",
    CHRONIC_IND == "B" ~ "Both",
    CHRONIC_IND == "N" ~ "N/A"
  ))

icd10_ccsr3_dsc_map <- icd10_ccsr_map_2021 %>% 
  select(ICD10CM = `ICD-10-CM CODE`,
         ICD10_DSC = `ICD-10-CM CODE DESCRIPTION`,
         CCSR1X = `CCSR CATEGORY 1`,
         CCSR2X = `CCSR CATEGORY 2`,
         CCSR3X = `CCSR CATEGORY 3`,
         CCSR1X_DSC = `CCSR CATEGORY 1 DESCRIPTION`,
         CCSR2X_DSC = `CCSR CATEGORY 2 DESCRIPTION`,
         CCSR3X_DSC = `CCSR CATEGORY 3 DESCRIPTION`)
  
dx_detail <- conds_filtered %>% 
  inner_join(conds_completed_so_far) %>% 
  inner_join(icd10_ccsr3_dsc_map) %>% 
  left_join(ccir_icd10_map) %>% 
  mutate(CHRONIC_IND = replace_na(CHRONIC_IND, "N/A")) %>% 
  mutate(CCSR1X_DSC = replace_na(CCSR1X_DSC, ""),
         CCSR2X_DSC = replace_na(CCSR2X_DSC, ""),
         CCSR3X_DSC = replace_na(CCSR3X_DSC, "")) %>% 
  mutate(CCSR_DSC = str_squish(paste(CCSR1X_DSC, CCSR2X_DSC, CCSR3X_DSC, sep="\n"))) %>% 
  mutate(Age_Diag = if_else(AGEDIAG < 0, "Unknown", as.character(AGEDIAG))) %>% 
  select(DUPERSID, ICD10CM_Pretty = ICDCMCode, ICD10CM, DiagnosisDesc, CCSR_DSC, CHRONIC_IND, Age_Diag) %>% 
  write_csv("./summary_tables/dx_detail.csv") 
