# This script is supposed to pre-process data for the analysis for the brief report.


# extract data ----
import_data <- function(file, sheet) read.xlsx(here("_raw",file), sheet = sheet) %>%
  
  # keep variables of interest
  select(
    1, Study, Age, `Education-2-cat`, Type_of_prevailing_occupation_during_life, Marital_status, # predictors
    `SA_New-BNT`, `Regular-PA`, Z_SA, MMSE, GDS15, GAI, FAQ, `Total-mental-activities`, Health # outcomes
  ) %>%
  
  # re-format
  mutate(
    Study = factor(
      x = Study,
      levels = c("COSACTIW","NANOK")
    ),
    Cosactiw = factor(
      if_else(
        condition = Study == "COSACTIW",
        true = 1,
        false = 0
      )
    ),
    Education = factor(
      x = `Education-2-cat`,
      levels = 1:2,
      labels = c("lower","higher"),
      ordered = T
    ),
    SA = factor(
      case_when(
        `SA_New-BNT` == 1 ~ 1,
        `SA_New-BNT` == 0 ~ 0
      )
    ),
    PA = factor(
      if_else(
        condition = `Regular-PA` == 1,
        true = 1,
        false = 0
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
    Marital_status = factor(
      case_when(
        Marital_status == 1 ~ "Non-married",# "Single",
        Marital_status == 2 ~ "Married/partnership",
        Marital_status == 3 ~ "Widowed",
        Marital_status == 4 ~ "Non-married" #"Divorced"
      )
    ),
    Total_MA = `Total-mental-activities`
  ) %>%
  
  # keep re-formatted variables
  select(
    ID, Study, Cosactiw, Age, Education,
    SA, PA, Z_SA, MMSE, GDS15, GAI, FAQ, Total_MA, Health,
    Profession, Marital_status
  )

