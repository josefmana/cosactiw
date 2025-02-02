#
# This script is supposed to extract normative thresholds for episodic memory for superaging.
#

#
# SCALED SCORES FOR RAVLT ----
ravlt_ss <- function(raw) case_when(
  
  # based on Tab. 7 from Frydrychova et al. (2018, https://psycnet.apa.org/record/2018-63633-003)
  raw == 0          ~ 3,
  raw == 1          ~ 4,
  raw == 2          ~ 5,
  raw == 3          ~ 6,
  raw == 4          ~ 7,
  raw == 5          ~ 8,
  raw == 6          ~ 9,
  raw %in% c(7,8)   ~ 10,
  raw == 9          ~ 11,
  raw == 10         ~ 12,
  raw == 11         ~ 13,
  raw == 12         ~ 14,
  raw == 13         ~ 15,
  raw %in% c(14,15) ~ 16
  
)

#
# EXTRACT SUPERAGING MEMORY THRESHOLDS ----
extract_thresholds <- function(pvlt_data_file, pvlt_id_file, nanok_file, ravlt_file, output = "thresholds") {
  
  # extract patient demography data
  demo <-
    read_xls( path = nanok_file, sheet = "NANOK_demografie" ) %>%
    mutate( edu = case_when(`1_EDU_TYPE_2` == 0 ~ "lower", `1_EDU_TYPE_2` == 1 ~ "higher") ) %>% # re-code education
    filter( `1_GENDER` == 2 ) %>% # keep women only
    select( ID, edu )
  
  # extract RAVLT data
  ravlt <-
    read_csv(file = ravlt_file) %>%
    filter( gender == "zena" ) %>% # keep women only
    mutate(
      ID      = id,
      age     = vek,
      age_cat = vek_kategorie,
      edu     = case_when(vzdelani_2 == "nizsi" ~ "lower", vzdelani_2 == "vyssi" ~ "higher"), # re-code education
      raw     = RAVLT_t7,
      scaled  = ravlt_ss(RAVLT_t7)
    ) %>%
    select(ID, age, age_cat, edu, raw, scaled)
    
  # extract PVLT data
  pvlt <-
    read_csv(pvlt_data_file) %>%
    filter( (!(Jméno %in% read_csv(pvlt_id_file, col_names = F)$X1) ) ) %>%
    mutate(
      ID  = as.numeric(Jméno), # for compatibility with NANOK, participant 1226p and 1247p were dropped as a result
      age = Věk,
      raw = T9
    ) %>%
    select(ID, age, raw) %>%
    left_join(demo, by = "ID") %>% # add demographic variables (i.e., education level)
    filter( complete.cases(edu) ) %>% # keep women only (men have edu == NA because they are not part of 'demo')
    mutate(
      age_cat = case_when(
        age >= 60 & age < 65 ~ "60-64",
        age >= 65 & age < 70 ~ "65-69",
        age >= 70 & age < 75 ~ "70-74",
        age >= 75 & age < 80 ~ "75-79",
        age >= 80 & age < 85 ~ "80-84",
        age >= 85 ~ "85+"
      )
    ) %>%
    select(ID, age, age_cat, edu, raw) # reorder columns
  
  # define a function for computing M, S, N and df
  M <- function(x) mean(x, na.rm = T)
  S <- function(x) sd(x, na.rm = T)
  N <- function(x) length( na.omit(x) )
  
  # compute threshold values, i.e., expected value for a women in 60-64 years age category
  # and their 95% CIs
  thresholds <- lapply(
    
    # loop through tests
    c("ravlt", "pvlt"),
    function(task) sapply(
      
      # loop through statistics
      c("M", "S", "N"),
      function(fun) sapply(
        
        # loop through education levels
        c("lower", "higher"),
        function(l) do.call(
          
          what = fun,
          args = list(subset(get(task), age_cat == "60-64" & edu == l)$raw)
          
        )
        
      )
    ) %>%
      
      as.data.frame() %>%
      rownames_to_column("edu") %>%
      mutate(
        df          = N - 1,
        low_CI      = M + ( S / sqrt(N) ) * qt(.025, df),
        high_CI     = M - ( S / sqrt(N) ) * qt(.025, df),
        thresh_mean = ceiling(M),
        thresh_low  = ceiling(low_CI),
        thresh_high = ceiling(high_CI),
        task        = task
      )
      
  ) %>%
    
    reduce(full_join) %>%
    relocate(task, .before = 1)
  
  # prepare normative tables for RAVLT and PVLT
  norms <- array(
    
    data     = NA, # to be added via loops
    dim      = c( task = 2, stat = 3, educ = 2, age = length( unique(ravlt$age_cat) ) ),
    dimnames = list(
      task = c("ravlt", "pvlt"),
      stat = c("M", "S", "N"),
      educ = c("lower", "higher"),
      age  = unique(ravlt$age_cat)
    )
    
  )
  
  # looping action
  for ( t in dimnames(norms)$task ) {
    for ( s in dimnames(norms)$stat ) {
      for ( e in dimnames(norms)$educ ) {
        for ( a in dimnames(norms)$age ) {
          
          norms[t, s, e, a] <- do.call(
            
            what = s,
            args = list(subset(get(t), age_cat == a & edu == e)$raw)
            
          )
          
        }
      }
    }
  }
  
  # return what was asked for
  return( get(output) )
  
}
