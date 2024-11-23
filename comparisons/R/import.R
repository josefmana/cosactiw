# This script is supposed to pre-process data for the analysis for the brief report.


# LIST DATA FILE ----
data_file <- function(folder, file) here(folder, file)

# EXTRACT DATA ----
import_data <- function(file, sheet) read.xlsx(file, sheet = sheet) %>%
  
  # keep variables of interest
  select(
    1, Study, Age, `Education-2-cat`, Type_of_prevailing_occupation_during_life, Marital_status, # predictors
    `SA_New-BNT`, `Regular-PA`, MMSE, GDS15, GAI, FAQ, `Total-mental-activities`, Health, # outcomes
    Z_RAVLT_PVLT_delayed_recall, Z_TMT_B_uds, Z_BNT_new, Z_VF_uds, # SuperAging variables
    RAVLT_delayed_recall # for SA re-calculation
  ) %>%
  
  # re-format
  mutate(
    mPA = factor(
      x = Study,
      labels = c("COSACTIW","NANOK")
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
        # lower education COSACTIW SA
        `SA_New-BNT` == 1 & Study == "COSACTIW" &`Education-2-cat` == 1 & RAVLT_delayed_recall > 9 ~ 1,
        `SA_New-BNT` == 1 & Study == "COSACTIW" &`Education-2-cat` == 1 & RAVLT_delayed_recall < 10 ~ 0,
        # lower education COSACTIW SA
        `SA_New-BNT` == 1 & Study == "COSACTIW" &`Education-2-cat` == 2 & RAVLT_delayed_recall > 10 ~ 1,
        `SA_New-BNT` == 1 & Study == "COSACTIW" &`Education-2-cat` == 2 & RAVLT_delayed_recall < 11 ~ 0,
        # COSACTIW non-SA remain unchanged
        `SA_New-BNT` == 0 ~ 0,
        # NANOK (no change)
        Study == "NANOK" ~ `SA_New-BNT`,
      )
    ),
    Z_SA = rowMeans(
      x = across( all_of( starts_with("Z_") ) ),
      na.rm = T
    ),
    cPA = factor(
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
    Status = factor(
      case_when(
        Marital_status == 1 ~ "Non-married",# "Single",
        Marital_status == 2 ~ "Married/partnership",
        Marital_status == 3 ~ "Non-married", # "Widowed",
        Marital_status == 4 ~ "Non-married" #"Divorced"
      )
    ),
    Depr = factor(
      if_else(
        condition = GDS15 > 5,
        true = 1,
        false = 0
      )
    ),
    Anx = factor(
      if_else(
        condition = GAI > 10,
        true = 1,
        false = 0
      )
    ),
    Total_MA = `Total-mental-activities`
  ) %>%
  
  # keep re-formatted variables
  select(
    ID, mPA, Cosactiw, Age, Education,
    SA, cPA, Z_SA, MMSE, GDS15, GAI, FAQ, Depr, Anx,
    Total_MA, Health, Profession, Status
  )

