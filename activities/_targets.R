# This is a script running targets pipeline of the COSACTIW vs NANOK comparisons project.

# Load packages required to define the pipeline:
library(targets)

# Set target options:
tar_option_set( packages = c(
  
  "here",        # for path listing
  "tidyverse",   # for data wrangling
  "openxlsx",    # for data reading
  "effsize",     # for effect sizes
  "rcompanion",  # for computation of some effect sizes
  "brms",        # for Bayesian regressions
  "bayesplot"    # for plotting
  
) )

# Use multiple cores for Bayesian models fitting:
options( mc.cores = parallel::detectCores() )

# Load all in-house functions:
tar_source()

# List the targets:
list(

  # READ & FORMAT DATA ----
  tar_target(
    name    = mapping_file, # path to the file with mapping
    command = here("_raw", "COSACTIW_DCERA-aktivity2.xlsx"),
    format  = "file"
  ),
  tar_target(
    name    = data_file, # path to the file with outcome data
    command = here("_raw", "COSACTIW_NANOK_pro-jamovi.xlsx"),
    format  = "file"
  ),
  tar_target(
    name    = data_long, # extract & format data in the long format
    command = get_data(mfile = mapping_file, dfile = data_file)
  ),
  tar_target(
    name    = data_wide, # extract a set of wide data
    command = pivot_data(.input = data_long)
  ),
  
  # DATA DESCRIPTION ----
  tar_target(
    name    = both_seasons_ids, # participants who reported the same activity as both seasonal and non-seasonal
    command = both_seasons(.input = data_wide)
  ),
  tar_target(
    name    = N, # count SA and non-SA subjects
    command = count_subjects(.data = data_wide, .subset = "all")
  ),
  tar_target(
    name    = activity_counts, # mean ± SD of activities per seasoness & category
    command = activity_counts(.input = data_long, .data = data_wide)
  ),
  tar_target(
    name    = cross_tables, # cross-tables of the number of at least one activity in SA vs non-SA
    command = contingency_tables(.input = data_long, .data = data_wide)
  ),
  tar_target(
    name    = chisquares, # chi-square test for each of the cross-tables
    command = chi_squares(.tabs = cross_tables)
  ),
  
  # INTENSITIES REGRESSIONS ----
  tar_target(
    name    = preprocessed_data, # data pre-processed for Bayesian regressions
    command = preprocess_data(.input = data_long)
  ),
  tar_target(
    name    = priors, # set-up priors for GLMMs
    command = set_priors() 
  ),
  tar_target(
    name    = formulas, # set-up linear model formulas for GLMMs
    command = set_formulas() 
  ),
  tar_target(
    name    = regressions, # fit a set of pre-specified Bayesian GLMMs
    command = fit_regressions(.formulas = formulas, .priors = priors, .data = preprocessed_data)
  ),
  tar_target(
    name    = loo_comparisons, # compare models via PSIS LOO
    command = psis_loo(.fits = regressions)
  )

  
)
