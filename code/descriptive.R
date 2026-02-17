#-----------------------
# Descriptive statistics
#-----------------------

# Set up the environment
here::i_am("salt_results.qmd")

# Load cleaning functions
source(here::here("functions", "cleaning.R"))

# Importing data for descriptive statistics
print("Importing data for descriptive statistics")

data_prelim <- clean_long()