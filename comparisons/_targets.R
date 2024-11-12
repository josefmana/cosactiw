# This is a script running targets pipeline of the project.

# Load packages required to define the pipeline:
library(targets)

# Set target options:
tar_option_set( packages = c(
  
  "here", # for path listing
  "tidyverse", # for data wrangling
  "ggdag", # for DAG drawing
  "ggraph", # for advance DAG/ggplot operations
  "ggtext", # for adding text to plots
  "patchwork", # for arranging plots
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
  tar_target( dag_plot, make_dag(plot = T) ),
  tar_target( adjustment_sets, adjustment_table(dag) ),
  
  # read & format the data ----
  tar_target( data, import_data(file = "COSACTIW_NANOK_pro-jamovi.xlsx", sheet = "cosactiw+nanok") ), # read data
  
  # regressing the outcomes on exposures ----
  tar_target( specs, model_specs(adjustment_sets, faq = "continuous") ), # model specifications table
  tar_target( models, fit_models(data, specs, log_1 = NULL, contr = T) ), # fit the models
  tar_target( diagnostics, diagnose_models(models) ), # extract model diagnostics
  
  # extracting & saving the results ----
  tar_target( results, stat_test(models, specs, adjustment_sets) ), # extract results of statistical models
  tar_target( results_plot, plot_results(data, results, specs, save = T) ) # save as a plot

)
