#site.R
# Builds the static pages required for thispatientdoesnotexist.com

library(rmarkdown)
library(dplyr)

# Load your dataset (replace 'your_dataset.csv' with your actual dataset file)
final_mapping <- read_csv("./etc/summary_tables/final_mapping.csv")

# Sample 200 random patients
sampled_patients <- final_mapping %>% 
  sample_n(1) %>% 
  distinct(Synthea_ID) %>% 
  mutate(n = row_number())

# Iterate through each patient and render the RMarkdown file
for (patient_id in sampled_patients$Synthea_ID) {
  # Extract patient details (assuming patient details are in a format that can be passed to RMarkdown)
  
  patient_num <- as.numeric((sampled_patients %>% 
    filter(patient_id == Synthea_ID))$n)
  
  # Render the RMarkdown file for each patient
  # 'output_file' specifies the name of the output HTML file
  # 'params' is a list of parameters passed to the RMarkdown file
  rmarkdown::render('./test_patient.Rmd',
                    output_file = paste0('./www/thispatientdoesnotexist.com/html/', patient_num, '.html'),
                    params = list(patient_id = patient_id))
}
