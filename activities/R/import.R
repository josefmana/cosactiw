#
# This script is supposed to pre-process data for the RTROS analysis.
#

#
# IMPORT THE DATA ----
get_data <- function(mfile, dfile, sfile) {
  
  # included women
  included <- readRDS(sfile)$id
  
  # activities mapping
  map <-
    read.xlsx(mfile, sheet = "Sheet 1") %>%
    select(type, category, activity) %>%
    add_row(type = "physical", category = "flexibility_health_exercise", activity = "chi_kung") %>%
    arrange(type, category, activity)
  
  # demography
  dem <-
    read.xlsx(dfile, sheet = "1ID_DATE_AGE_SCREENING_Demogr") %>%
    #filter( `Was_the_examination_valid?` == 1 ) %>%
    rename(
      "Age_years" = "Age",
      "Education_years" = "Number_of_years_of_study",
      "Education_level" = "Highest_education_level",
      "Subjective_age_years" = "Subjective_age_number",
      "Valid" = "Was_the_examination_valid?"
    ) %>%
    mutate(
      ID = gsub(" ", "", ID), # no spaces in ID allowed
      PA = factor(
        PA,
        levels = 1:6,
        labels = c(
          "I'm an athletic person",
          "I enjoy movement/exercise",
          "I exercised at least 3 times a week",
          "I don't avoid movement/exercise",
          "I'm not an athletic person",
          "I had to stop doing sports (Injury)"
        )
      ),
      Education_level = factor(
        Education_level,
        levels = 1:4,
        labels = c(
          "Primary school",
          "Vocational school (OU)",
          "Secondary school",
          "College/university"
        ),
        ordered = T
      )
    ) %>%
    filter(ID %in% included) %>% # keep only the same women as in COBRA analysis
    select(ID, Valid, Age_years, Subjective_age_years, Education_years, Education_level, PA, FAQ, MMSE)
  
  # cognition
  cog <-
    read.xlsx( dfile, sheet = "3COGNITIVE_TESTS" ) %>%
    left_join(
      readRDS(sfile) %>%
        mutate(ID = gsub(" ", "", id) ) %>% # no spaces in ID allowed)
        select(ID, sa)
    ) %>%
    filter(ID %in% included) %>% # keep only the same women as in COBRA analysis
    mutate(
      ID = gsub(" ", "", ID), # no spaces in ID allowed
      SA = factor(
        if_else(sa == T, 1, 2),
        levels = 1:2,
        labels = c("SA", "nonSA")
      ),
      WHO_PA = factor(
        `WHO-PA`,
        levels = 0:1,
        labels = c("nonPA", "PA")
      )
    ) %>%
    select(ID, SA, WHO_PA, `RAVLT_1-5`, RAVLT_delayed_recall, TMT_A_time, TMT_B_time, Spon_sem, VF_animals)
  
  # activities
  act <-
    read.xlsx( dfile, sheet = "5RETROS-LEISURE_ACTIVITIES" ) %>%
    mutate(
      ID = gsub(".0", "", gsub(" ", "", ID), fixed = T), # no spaces or decimals in ID allowed
      Activity = if_else(Activity == "theathre", "theatre", Activity),
      Activity_type = case_when(
        Activity == "reading" ~ "mental",
        Activity == "theatre" ~ "mental",
        Activity == "self_education" ~ "mental",
        .default = Activity_type
      ),
      Seasonal = if_else(Intensity >= 10, T, F),
      Intensity = factor(
        Intensity %% 10, # modulo 10 to pool seasonal
        levels = c(0:5),
        labels = c(
          "do not know",
          "occasionally or not at all",
          "several times a month",
          "once a week",
          "several times a week",
          "every day or nearly every day"
        )
      ),
      Category = unlist( sapply( 1:nrow(.), function(i) map[map$activity == Activity[i], "category"] ), use.names = F )
    ) %>%
    filter(ID %in% included) # keep only the same women as in COBRA analysis
  
  # Cobra-A
  cobra <-
    read.xlsx(dfile, sheet = "COSACTIW-pro-JAMOVI") %>%
    select(1, `Total-mental-activities`, `total-MA-frequency`, `Total-MA-time`) %>%
    filter(ID %in% included) %>% # keep only the same women as in COBRA analysis
    rename(
      "cobra_a_total" = "Total-mental-activities",
      "cobra_a_frequency" = "total-MA-frequency",
      "cobra_a_time" = "Total-MA-time"
    )
  
  # VLS-AQL
  vls <-
    read.xlsx(dfile, sheet = "COSACTIW-pro-JAMOVI") %>%
    select( 1, starts_with("VLS_I") ) %>%
    filter(ID %in% included) %>% # keep only the same women as in COBRA analysis
    pivot_longer(
      cols = -ID,
      names_to = "item",
      values_to = "response"
    )
  
  # return the data
  return(
    list(
      dem = dem,
      cog = cog,
      act = act,
      cobra = cobra,
      vls = vls,
      map = map
    )
  )

}

#
# PIVOT DATA TO WIDE FORMAT ----
pivot_data <- function(.input) with(
  
  .input, sapply(
    
    map$activity,
    function(i) sapply(
      
      unique(act$ID),
      function(j) if_else(
        
        condition = i %in% act[ act$ID == j, "Activity"],
        true      = 1,
        false     = 0
          
      )
      
    )
  ) %>%
    
    as.data.frame() %>%
    mutate(
      activities = rowSums( across( everything() ) ), # sum all activities per participant
      !!!setNames(rep( NA, length( unique(map$type) ) )    , unique(map$type) ),
      !!!setNames(rep( NA, length( unique(map$category) ) ), unique(map$category) ),
      across( unique(map$type),     ~ rowSums( across( all_of( map[map$type     == cur_column(), "activity"] ) ) ) ),
      across( unique(map$category), ~ rowSums( across( all_of( map[map$category == cur_column(), "activity"] ) ) ) )
    ) %>%
    rownames_to_column("ID") %>%
    
    # join other data sets
    left_join(cog, by = "ID") %>%
    left_join(dem, by = "ID")
  
)
