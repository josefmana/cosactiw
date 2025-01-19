#
# Functions to describe retrospectively recalled leisure activities with respect to cognitive SA via Bayesian regressions.
#

#
# PRE-PROCESS DATA ----
preprocess_data <- function(.input) with(

  .input, act %>%

    left_join( dem[ , c("ID","Age_years")] ) %>%
    filter( complete.cases(Intensity) ) %>%
    left_join(cog) %>%
    mutate(

      # re-format categorical variables
      Seasonal      = factor(Seasonal),
      Activity_type = factor(Activity_type),
      Activity      = factor(Activity),
      ID            = factor(ID),

      # intensity/frequency (outcome) measures
      Intensity    = as.integer( if_else(Intensity == "do not know", NA, Intensity) ),
      logIntensity = log( as.numeric(Intensity)-1 ),

      # time (predictor) variables
      Time_past = Time_bin - Age_years,
      Time_bin  = factor(Time_bin, levels = unique(act$Time_bin), ordered = T),
      Time_num  = as.numeric(Time_bin),
      #Time_log = log(Time_num),

      # PPC categories
      Type_Season_Time = paste(Activity_type, Seasonal, Time_bin, sep = "_"),
      SA_Type_Time     = paste(SA, Activity_type, Time_bin, sep = "_"),
      SA_Season_Time   = paste(SA, Seasonal, Time_bin, sep = "_")

    ) %>%
    
    # set-up contrasts
    within( . , {

      contrasts(SA)            <- -contr.sum(2)/2 # nonSA = -0.5, SA = 0.5
      contrasts(Seasonal)      <- -contr.sum(2)/2 # non-seasonal = -0.5, seasonal = 0.5
      contrasts(Activity_type) <- -contr.sum(2)/2 # mental = -0.5, physical = 0.5

    } )
  
)

#
# SET-UP PRIORS ----
set_priors <- function() c(

  prior( normal(1, 3),   class = "Intercept" ),
  prior( normal(0, 3),   class = "b"         ),
  prior( exponential(2), class = "sd"        ),
  prior( exponential(2), class = "sigma"     ),
  prior( lkj(2),         class = "cor"       )
  #prior( dirichlet(1),   class = "simo"      ) # default Dirichlet(1) for monotonic terms in the ordered time model

)

#
# SET-UP LINEAR MODEL FORMULAS ----
set_formulas <- function() list(
  
  linear_time  = bf( logIntensity ~ 1 + Time_num * SA * Activity_type + (1 + Time_num | ID) + (1 + Time_num | Category) ),
  #log_time     = bf( logIntensity ~ 1 + Time_log * SA * Activity_type + (1 + Time_log | ID) + (1 + Time_log | Category) ),
  reverse_time = bf( logIntensity ~ 1 + Time_past * SA * Activity_type + (1 + Time_past | ID) + (1 + Time_past | Category) ),
  ordered_time = bf( logIntensity ~ 1 + mo(Time_bin) * SA * Activity_type + (1 + mo(Time_bin) | ID) + (1 + mo(Time_bin) | Category) )
  
)

#
# FIT A SET OF GLMMS ----
fit_regressions <- function(.data, .priors, .formulas) lapply(
  
  set_names( names(.formulas) ),
  function(i) brm(
    
    formula = .formulas[[i]],
    family  = gaussian(link = "identity"),
    prior   = .priors,
    data    = .data,
    seed    = 87542
    
  )
)

#
# COMPARE MODELS VIA PSIS-LOO ----
psis_loo <- function(.fits) with( .fits, loo(linear_time, reverse_time, ordered_time) )
