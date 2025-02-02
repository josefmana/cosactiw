#
# This script is supposed to pre-process data for the analysis for the brief report.
#

#
# LIST DATA FILE ----
data_file <- function(folder, file) here(folder, file)

#
# EXTRACT DATA ----
import_data <- function(file, sheet, norms, thresholds, thres_type = "mean") read.xlsx(file, sheet = sheet) %>%
  
  # keep variables of interest
  select(
    1, Study, Age, `Education-2-cat`, Type_of_prevailing_occupation_during_life, Marital_status, # predictors
    `Regular-PA`, MMSE, GDS15, GAI, FAQ, `Total-mental-activities`, Health,                      # outcomes
    RAVLT_delayed_recall, TMT_B_time, Spon_sem, VF_animals,                                      # SuperAging variables raw scores
    `SA-TMT-B-new`, `SA-BNT-new`, `SA-VF`,                                                       # SuperAging indicators
    Z_RAVLT_PVLT_delayed_recall, Z_TMT_B_uds, Z_BNT_new, Z_VF_uds                                # SuperAging variables z scores
  ) %>%
  
  # re-format
  mutate(
    mPA = factor(
      x      = Study,
      labels = c("COSACTIW","NANOK")
    ),
    Cosactiw = factor(
      if_else(
        condition = Study == "COSACTIW",
        true      = 1,
        false     = 0
      )
    ),
    Education = factor(
      x       = `Education-2-cat`,
      levels  = 1:2,
      labels  = c("lower","higher"),
      ordered = T
    ),
    Age_bin = case_when(
      Age >= 60 & Age < 65 ~ "60-64",
      Age >= 65 & Age < 70 ~ "65-69",
      Age >= 70 & Age < 75 ~ "70-74",
      Age >= 75 & Age < 80 ~ "75-79",
      Age >= 80 & Age < 85 ~ "80-84",
      Age >= 85 ~ "85+"
    ),
    Z_RAVLT_PVLT_delayed_recall = unlist(
      # re-calculate z-score for memory tasks based on table norms
      # from data provided to us by original authors
      sapply(
        1:nrow(.),
        function(i)
          ( RAVLT_delayed_recall[i] - norms[ifelse(Study[i] == "NANOK", "pvlt", "ravlt"), "M", Education[i], Age_bin[i] ] ) /
            norms[ifelse(Study[i] == "NANOK", "pvlt", "ravlt"), "S", Education[i], Age_bin[i] ]
      ),
      use.names = F
    ),
    cutoff = sapply(
      # helper column showing thresholds for cognitive SA
      1:nrow(.),
      function(i) with(
        thresholds,
        get( paste0("thresh_",thres_type) )[task == case_when(Study[i] == "NANOK" ~ "pvlt", Study[i] == "COSACTIW" ~ "ravlt") & edu == as.character(Education[i])]
      )
    ),
    Delayed_recall_SA = if_else(
      # need to be at least at the level of threshold in delayed
      # memory task to qualify for SA
      condition = RAVLT_delayed_recall >= cutoff,
      true      = 1,
      false     = 0
    ),
    SA = factor(
      if_else(
        # needs at least the cut-off in a memory task
        # and at least average performance in all remaining tasks 
        # to be labelled SA = 1
        condition = (`SA-TMT-B-new` + `SA-BNT-new` + `SA-VF` + Delayed_recall_SA) == 4,
        true      = 1,
        false     = 0
      )
    ),
    Z_SA = rowMeans(
      x     = across( all_of( starts_with("Z_") ) ),
      na.rm = T
    ),
    cPA = factor(
      if_else(
        condition = `Regular-PA` == 1,
        true      = 1,
        false     = 0
      )
    ),
    Profession = factor(
      case_when(
        Type_of_prevailing_occupation_during_life == 1 ~ "manual",
        Type_of_prevailing_occupation_during_life == 2 ~ "mostly manual",
        Type_of_prevailing_occupation_during_life == 3 ~ "mostly mental",
        Type_of_prevailing_occupation_during_life == 4 ~ "mental"
      ),
    ),
    Status = factor(
      case_when(
        Marital_status == 1 ~ "Non-married", # "Single",
        Marital_status == 2 ~ "Married/partnership",
        Marital_status == 3 ~ "Non-married", # "Widowed",
        Marital_status == 4 ~ "Non-married"  #"Divorced"
      )
    ),
    Depr = factor(
      if_else(
        condition = GDS15 > 5,
        true      = 1,
        false     = 0
      )
    ),
    Anx = factor(
      if_else(
        condition = GAI > 10,
        true      = 1,
        false     = 0
      )
    )
  ) %>%
  
  # do some re-naming
  rename(
    "Total_MA"           = "Total-mental-activities",
    "Delayed_recall_raw" = "RAVLT_delayed_recall",
    "Delayed_recall_z"   = "Z_RAVLT_PVLT_delayed_recall",
    "TMT_B_raw"          = "TMT_B_time",
    "TMT_B_z"            = "Z_TMT_B_uds",
    "TMT_B_SA"           = "SA-TMT-B-new",
    "BNT_30_raw"         = "Spon_sem",
    "BNT_30_z"           = "Z_BNT_new",
    "BNT_30_SA"          = "SA-BNT-new",
    "VF_Animals_raw"     = "VF_animals",
    "VF_Animals_z"       = "Z_VF_uds",
    "VF_Animals_SA"      = "SA-VF"
  ) %>%
  
  # keep re-formatted variables
  select(
    ID, mPA, Cosactiw, Age, Age_bin, Education,
    SA, cPA, Z_SA, MMSE, GDS15, GAI, FAQ, Depr, Anx,
    Total_MA, Health, Profession, Status,
    cutoff, ends_with("_raw"), ends_with("_z"), ends_with("_SA")
  )

