# This is a script running targets pipeline of the 'Attitudes towards Physical Activity' project.

# Load packages required to define the pipeline:
library(targets)

# Set target options:
tar_option_set( packages = c(
  
  "here",       # for path listing
  "tidyverse",  # for data wrangling
  "openxlsx",   # for data reading
  "rstatix",    # for stats
  "effectsize", # for effect sizes
  "gt",         # for tableing
  "ggpubr"      # for visualisation
  
) )

# Load all in-house functions:
tar_source()

# List the targets:
list(
  
  # prepare data ----
  tar_target(
    name = datafile,
    command = here("_raw", "BNT_COSACTIW_DATA_Klara_final_pro_statistiku_označené_sloupce_zbytek_statistiky.xlsx"),
    format = "file"
  ),
  tar_target(
    name = data,
    command = import_data(datafile)
  ),
  tar_target(
    name = variables,,
    command = list_outcomes()
  ),
  # power analysis
  
  # fit models ----
  tar_target(
    name = models,
    command = fit_regressions(data, variables)
  ),
  tar_target(
    name = chisquares,
    command = compute_chisq(data, variables)
  ),
  # diagnostics
  # diagnostic tables?
  
  # extract results ----
  tar_target(
    name = omnibus_table, # results of the omnibus tests
    command = prepare_table(data, models, chisquares, variables)
  ),
  tar_target(
    name = table2, # prepare a nicer form of Table 1
    command = format_table(omnibus_table)
  ),
  tar_target(
    name = pwc_table, # pairwise comparisons table
    command = table_pairwise(models, variables)
  ),
  tar_target(
    name = figure1, # pairwise comparisons plot
    command = plot_pairwise(data, variables, save = T)
  )
  
)
