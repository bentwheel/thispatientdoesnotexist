# synthea.R

# This R script constructs a shell script that can be used to build a 
# Synthea-generated database of N individuals in the United States, where N is 
# set by the user in the variable 'simulated_individuals_count'.

# Synthea is capable of generating a rich, demographically-representative 
# dataset of patient demographics at the individual state level, but not at the
# entire US level. Therefore, a shell script must be created to generate
# synthetic patient datasets for each individual state (plus DC). 

# Proportion weights for each state (plus DC) are used to determine, alongside
# the user-provided value of 'simulated_individuals_count', the total number of
# individuals to run in each state. State population weights are drawn from the 
# US Census Bureau data (5-year American Community Survey) to determine 
# population sizes for each state such that the count of every state's simulated
# patients sums to the desired value in 'simulated_individuals_count'.

# Change this value to scale up or down the number of individuals simulated by 
# Synthea in the United States.  Simulated individuals will be generated
# in proportion to state population levels.
simulated_individuals_count <- 80000

# Required libraries
library(tidyverse)
library(tidycensus)

# First, we need to run Synthea to create a US portfolio of individuals from 
# which to select. 

# Obtain your own API Key for USCB Data Assets here:
# https://api.census.gov/data/key_signup.html

# Once the API key is granted for you, you will need to use it in the prompt
# generated by this line.
apikey <- rstudioapi::askForSecret(
  "USCB API Key", "Please enter your US Census Bureau API Key")

# This will load the API Key for future USCB Data queries.
# Change install=F to *not* save the apikey in the .Renviron file.
census_api_key(apikey, install=T)

# Remove previous shell script created by this process, if it exists.
unlink("./etc/synthea_run.sh")

# Get US Population estimates, by state, using 2021 5-yr ACS variables.
uspop_2021 <- get_acs(
  geography = "state", 
  variables = "B01001_001",
  year = 2021
) %>% 
  rename(state = NAME, pop = estimate) %>% 
  select(state, pop) %>% 
  ungroup() %>% 
  as_tibble() %>% 
  filter(state != "Puerto Rico") #Sorry, PR.

# Create shell script to call Synthea runs and generate simulated individuals 
# in the ./etc/output/ folder.  The default config options for this run
# are provided in ./etc/synthea.properties and should not be altered unless
# you are super sure of what you're doing!
uspop_2021_cmd <- uspop_2021 %>% 
  mutate(simulate = as.integer(simulated_individuals_count * pop / sum(pop)),
         cmd = "java",
         arg1 = " -jar ./etc/synthea-with-dependencies.jar",
         arg2 = str_c(" -p ", simulate, " \"", # Simulate this number of people
                      state, "\" ", 
                      if_else(state == "Alabama", # new files for first state
                              "--exporter.csv.append_mode=false ", 
                              "")), # All subsequent state runs are appended
         arg3 = str_c(" -c ", "./etc/synthea.properties"), 
         cmd_string = str_c(cmd, arg1, arg3, arg2, "\n")) %>% 
  mutate(written = map(cmd_string, 
                       function(x) write_file(I(x), "./etc/synthea_run.sh", 
                                              append=T)))

# This will have created a shell script file that executes 51 synthea runs for 
# each state, plus DC. Next, pop open an ice cold [drink] and run that script - 
# it'll take a moment to finish!

# Next chmod the file so that it will run.
system("chmod u+rwx ./etc/synthea_run.sh")

# This may or may not work depending on your JAVA setup. You can always run 
# the shell script in the terminal directly.
system("./etc/synthea_run.sh")

