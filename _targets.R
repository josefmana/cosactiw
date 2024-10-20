# This is a script running targets pipeline of the project.

# Load packages required to define the pipeline:
library(targets)

# Set target options:
tar_option_set( packages = c(
  
  "here", # for path listing
  "tidyverse", # for data wrangling
  "ggdag", # for DAG drawing
  "ggraph", # for advance DAG/ggplot operations
  "openxlsx", # for data reading
  "performance", # for regression diagnostics 
  "emmeans", # for models' marginal means estimation
  "gridExtra" # for saving tables as images

) )

# Load all in-house functions:
tar_source()

# List the targets:
list(
  
  # causal assumptions ----
  tar_target( dag, make_dag(plot = F) ),
  tar_target( adjustment_sets, adjustment_table(dag) ),
  
  # read & format the data ----
  tar_target( data, import_data(file = "COSACTIW_NANOK_pro-jamovi.xlsx", sheet = "cosactiw+nanok") ),
  
  # regressing the outcomes on exposures ----
  tar_target( specs, model_specs(adjustment_sets) ), # model specifications table
  tar_target( models, model_fit(data, specs, log = T, contr = T) ), # fit the models
  tar_target( diagnostics, model_diagnose(models) ), # extract model diagnostics
  tar_target( test_results, stat_test(models, specs, adjustment_sets, save = T) ) # extract results of statistical models

)
