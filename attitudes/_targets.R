# This is a script running targets pipeline of the 'Attitudes towards Physical Activity' project.

# Load packages required to define the pipeline:
library(targets)

# Set target options:
tar_option_set( packages = c(
  
  "here", # for path listing
  "tidyverse", # for data wrangling
  "openxlsx", # for data reading
  "rstatix", # for stats
  "effectsize", # for effect sizes
  "ggpubr" # for visualisation
  
) )

# Load all in-house functions:
tar_source()

# List the targets:
list(
  
  # prepare data ----
  tar_target( datafile, read_data("_raw", "BNT_COSACTIW_DATA_Klara_final_pro_statistiku.xlsx"), format = "file"),
  tar_target( data, import_data(datafile) ),
  tar_target( variables, list_outcomes() ),
  # power analysis
  
  # fit models ----
  tar_target( models, fit_regressions(data, variables) ),
  tar_target( chisquares, compute_chisq(data, variables) ),
  # diagnostics
  # diagnostic tables?
  
  # extract results ----
  tar_target( table1, prepare_table(data, models, chisquares, variables) )
  # pairwise comparisons table
  # pairwise comparisons plot
  
)
