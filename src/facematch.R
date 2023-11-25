# faces.R

# This script matches a scanned face directory and face_attributes file corresponding to the face
# images in that directory against the Simulated patients so that each patient can have a face.

# It's an optional part of the underlying dataset, but part of the website, so it's done separately.

# Match to faces - prepare face data inventory.
face_attributes <- read_csv("./summary_tables/face_attributes.csv") %>% 
  filter(age >= 21) #no kids!

# Reinsert actual age from Synthea
matched_synthea <- matched_synthea %>% 
  left_join(synthea_people_matchdata_payerdeets) %>% 
  mutate(AGE_ACTUAL = (ymd(20211231) - ymd(BIRTHDATE)) / dyears(1))

# Determine normalization parameters for age as FaceScan tends to peg folks as younger
qqnorm(matched_synthea$AGE_ACTUAL)
qqnorm(face_attributes$age)

matched_meps_mean_age <- mean(matched_synthea$AGE_ACTUAL)
matched_meps_sd_age <- sd(matched_synthea$AGE_ACTUAL)
faces_mean_age <- mean(face_attributes$age)
faces_sd_age <- sd(face_attributes$age)

# Filter on images where race_ component is overwhelmingly certain (42% or higher) and gender_ is 99.5% or higher
face_attribs_filtered <- face_attributes %>%
  filter_at(vars(starts_with("gender_")), any_vars(. > 96)) %>% 
  mutate(tossout_race = case_when(
    dominant_race == "white" & race_white < 85 ~ 1,
    dominant_race == "black" & race_black < 25 ~ 1,
    dominant_race == "latino hispanic" & `race_latino hispanic` < 35 ~ 1,
    dominant_race == "asian" & race_asian < 60 ~ 1,
    .default = 0)) %>% 
  filter(tossout_race != 1) %>% 
  mutate(RACETHX = case_when(
    dominant_race == "latino hispanic" ~ "HISPANIC",
    dominant_race == "asian" ~ "NON-HISPANIC ASIAN ONLY",
    dominant_race == "black" ~ "NON-HISPANIC BLACK ONLY",
    dominant_race == "white" ~ "NON-HISPANIC WHITE ONLY",
    .default = "NON-HISPANIC OTHER RACE OR MULTIPLE RACE")) %>% 
  mutate(SEX = case_when(
    dominant_gender == "Man" ~ "M",
    dominant_gender == "Woman" ~ "F")) %>% 
  mutate(AGELAST = ((age - faces_mean_age)/faces_sd_age) * matched_meps_sd_age + faces_mean_age) %>%  #stretch but don't recenter - avoids very young looking old people :( 
  mutate(AGE_GRP_9 = case_when(AGELAST < 5 ~ "Under 5",
                               AGELAST >= 5 & AGELAST < 18 ~ "5 - 17",
                               AGELAST >= 18 & AGELAST < 30 ~ "18 - 29",
                               AGELAST >= 30 & AGELAST < 40 ~ "30 - 39",
                               AGELAST >= 40 & AGELAST < 50 ~ "40 - 49",
                               AGELAST >= 50 & AGELAST < 60 ~ "50 - 59",
                               AGELAST >= 60 & AGELAST < 70 ~ "60 - 69",
                               AGELAST >= 70 & AGELAST < 80 ~ "70 - 79",
                               AGELAST >= 80 ~ "80 and over",
                               T ~ as.character(AGELAST))) %>%
  select(Synthea_ID = filename, AGE_GRP_9, SEX, RACETHX, AGELAST) %>% 
  mutate(final_database = 0)

table(face_attribs_filtered$RACETHX) / count(face_attribs_filtered)$n

# Normal age dist test - make sure it looks normal-ish
qqnorm(face_attribs_filtered$AGELAST)

face_attribs_filtered <- face_attribs_filtered %>% 
  select(-AGELAST)

final_matchup_for_faces <- final_matchup %>% 
  select(Synthea_ID, AGE_GRP_9, SEX, RACETHX) %>% 
  mutate(final_database = 1) %>% 
  union_all(face_attribs_filtered)

# Again, perform the matching on what matters using an exact match with 1-1
matched_data_faces <-
  matchit(final_database ~  SEX + RACETHX + AGE_GRP_9,
          data=final_matchup_for_faces,
          method = "nearest",
          distance = "glm",
          exact = ~  SEX + RACETHX + AGE_GRP_9,
          replace = F,
          ratio = 20)

# Evaluate post-match distribution - this can take a while!
match_faces_summary <- summary(matched_data_faces)
match_faces_summary


plot(matched_data_faces, type = "density", interactive = FALSE,
     which.xs = ~SEX + RACETHX + AGE_GRP_9)

# Put it all together with the other matched_datset
matched_datset_faces <- match.data(matched_data_faces)

matched_finalpeople_faces <- matched_datset_faces %>% 
  filter(final_database == 1) %>% 
  arrange(subclass)

matched_images_faces <- matched_datset_faces %>% 
  filter(final_database == 0) %>% 
  arrange(subclass) 

face_images_mapping <- matched_finalpeople_faces %>% 
  inner_join(matched_images_faces, by=c("subclass")) %>% 
  select(Synthea_ID = Synthea_ID.x, filename = Synthea_ID.y) %>% 
  group_by(Synthea_ID) %>% 
  summarize(filename = first(filename)) %>% 
  ungroup()

final_mapping <- face_images_mapping %>% 
  inner_join(final_matchup, by=c("Synthea_ID"="Synthea_ID")) %>% 
  inner_join(meps_matchdata, by=c("MEPS_ID"="Id")) %>%
  inner_join(synthea_people, by=c("Synthea_ID"="Id")) %>% 
  inner_join(payer_transitions_filter, by=c("Synthea_ID"="PATIENT")) %>% 
  select(MEPS_ID, Synthea_ID, filename, 
         RACETHX = RACETHX.x, 
         AGE_GRP_9 = AGE_GRP_9.x, 
         GENDER = SEX.x,
         BIRTHDATE = BIRTHDATE.x,
         MARITAL = MARITAL.x,
         REGION21,
         INSURC21,
         AGELAST,
         PREFIX,
         FIRST,
         LAST,
         SUFFIX,
         MAIDEN,
         ADDRESS,
         CITY,
         STATE,
         COUNTY,
         ZIP,
         LAT,
         LON,
         PRIMARY_PAYER,
         PRIMARY_PAYER_ORG) %>% 
  mutate(MARITAL_STATUS = case_when(
    MARITAL == 'M' ~ "Married",
    MARITAL == 'W' ~ "Widowed",
    MARITAL == 'D' ~ "Divorced",
    MARITAL == 'S' ~ "Single",
    .default = "Single")) %>% 
  select(-MARITAL) %>% 
  mutate(RACETHX = str_to_title(RACETHX)) %>% 
  mutate(GENDER = if_else(GENDER == "M", "Male", "Female")) %>% 
  mutate(PRIMARY_PAYER = if_else(PRIMARY_PAYER == "NO_INSURANCE", "Uninsured", str_to_title(PRIMARY_PAYER)),
         PRIMARY_PAYER_ORG = if_else(PRIMARY_PAYER_ORG == "NO_INSURANCE", "", str_to_title(PRIMARY_PAYER_ORG)))

final_mapping %>% write_csv("./summary_tables/final_mapping.csv")