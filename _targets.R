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
  "MatchIt", # for propensity scores matching
  "cobalt", # for weightinh balance checking
  "patchwork", # for making grids of plots
  "marginaleffects", # for ATC estimation
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
  tar_target( raw_data, import_data(file = "COSACTIW_NANOK_pro-jamovi.xlsx", sheet = "cosactiw+nanok") ), # read data
  tar_target( processed_data, matching(raw_data, adjustment_sets, return = "data") ), # pre-process via propensity scores matching
  tar_target( graphical_balance_checks, matching(raw_data, adjustment_sets, return = "graphs") ), # balance plots
  tar_target( numerical_balance_checks, matching(raw_data, adjustment_sets, return = "tables") ), # balance tables
  
  # regressing the outcomes on exposures ----
  tar_target( specs, model_specs(adjustment_sets) ), # model specifications table
  tar_target( models, model_fit(processed_data, specs, log = T, contr = T) ), # fit the models
  tar_target( diagnostics, model_diagnose(models) ), # extract model diagnostics
  
  # extracting & saving the results ----
  tar_target( test_results, stat_test(models, specs, processed_data, adjustment_sets) ), # extract results of statistical models
  tar_target( tables_descriptive, save_tables(test_results, "descriptive") ),
  tar_target( tables_causal, save_tables(test_results, "causal") )

)
