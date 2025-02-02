# This is a script running targets pipeline of the COSACTIW vs NANOK comparisons project.

# Load packages required to define the pipeline:
library(targets)

# Set target options:
tar_option_set( packages = c(
  
  "here",        # for path listing
  "tidyverse",   # for data wrangling
  "ggdag",       # for DAG drawing
  "ggraph",      # for advance DAG/ggplot operations
  "ggtext",      # for adding text to plots
  "patchwork",   # for arranging plots
  "openxlsx",    # for data reading
  "readxl",      # for .xls reading
  "performance", # for regression diagnostics 
  "emmeans",     # for models' marginal means estimation
  "gridExtra"    # for saving tables as images

) )

# Load all in-house functions:
tar_source()

# List the targets:
list(
  
  # causal assumptions ----
  tar_target( dag, make_dag(plot = F) ),
  tar_target( dag_plot, make_dag(plot = T) ),
  tar_target( adjustment_sets, adjustment_table(dag) ),
  
  # extract normative values for episodic memory ----
  tar_target( pvlt_data, data_file("_raw", "PVLT_2012_NANOK_export_fin.xls - PVLT Czech.csv"), format = "file" ), # PVLT data
  tar_target( pvlt_excl, data_file("_raw", "PVLT_2012_NANOK_export_fin.xls - vyřazení.csv"), format = "file" ), # PVLT excluded
  tar_target( nanok_demo, data_file("_raw", "data_NANOK_2012-2013-2014_pro-Ondreje-Rydla_opr-MoCA.xls"), format = "file" ), # NANOK demography
  tar_target( ravlt_norms, data_file("_raw", "ravlt_analyza.csv"), format = "file" ), # RAVLT data
  tar_target( memory_thresholds, extract_thresholds(pvlt_data, pvlt_excl, nanok_demo, ravlt_norms) ), # extract memory thresholds stratified by test and education level
  tar_target( memory_norms, extract_thresholds(pvlt_data, pvlt_excl, nanok_demo, ravlt_norms, output = "norms") ), # extract memory task norms
  
  # read & format the data ----
  tar_target( datafile, data_file("_raw", "COSACTIW_NANOK_pro-jamovi-oprav.xlsx"), format = "file" ), # path to the data file
  tar_target( data, import_data(datafile, "cosactiw+nanok", memory_norms, memory_thresholds, "mean") ), # read data
  
  # regressing the outcomes on exposures ----
  tar_target( specs, model_specs(adjustment_sets, faq = "continuous") ), # model specifications table
  tar_target( models, fit_models(data, specs, log_1 = NULL, contr = T) ), # fit the models
  tar_target( diagnostics, diagnose_models(models) ), # extract model diagnostics
  
  # extracting & saving the results ----
  tar_target( results, stat_test(models, specs, adjustment_sets) ), # extract results of statistical models
  tar_target( results_plot, plot_results(data, results, specs, type = 1, save = T) ), # save as a plot
  tar_target( cognition_plot, plot_results(data, results, specs, type = 2, save = T) ) # save a plot of cognition only
)
