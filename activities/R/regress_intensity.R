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

#
# ADD CATEGORIES FOR POSTERIOR PREDICTIVE CHECKS TO DATA ----
add_ppc_categories <- function(.data) mutate(
  
  .data            = .data,
  Type_Season_Time = paste(Activity_type, Seasonal, Time_bin, sep = "_"),
  SA_Type_Time     = paste(SA, Activity_type, Time_bin, sep = "_"),
  SA_Season_Time   = paste(SA, Seasonal, Time_bin, sep = "_")
  
)

#
# DO POSTERIOR PREDICTIVE CHECKING ----
perform_posterior_checks <- function(.fits, .data) lapply(
  
  set_names( names(.fits) ),
  function(y) lapply(
    
    set_names( x = c("dens", "mean", "sd") ),
    function(f) if (f == "dens") ppc_plot(
      
      # density plots
      fit   = .fits[[y]],
      data  = subset( .data, complete.cases(logIntensity) ),
      y     = "logIntensity",
      x     = "SA_Type_Time",
      labs  = list(
        "Observed (thick lines) and predicted (thin lines) distributions of log(Intensity) of recalled leisure activities",
        "Cells represent different combinations of SA status, activity type, and time bin."
      ),
      meth  = "dens_overlay_grouped",
      draws = sample(1:4e3, 1e2),
      stat  = NULL
      
    ) else ppc_plot(
      
      # stat plots
      fit   = .fits[[y]],
      data  = subset( .data, complete.cases(logIntensity) ),
      y     = "logIntensity",
      x     = "SA_Type_Time",
      labs  = list(
        paste0("Observed (thick bars) and predicted (histograms) ",f," of log(Intensity) of recalled leisure activities"),
        "Cells represent different combinations of SA status, activity type, and time bin."
      ),
      meth  = "stat_grouped",
      draws = 1:4e3,
      stat  = f
      
    )
  )
)

#
# EXTRACT POSTERIOR EXPECTATIONS ----
compute_posterior_expectations <- function(.data, .fit) {
  
  
  ## ---- Posterior Distributions ----
  # prepare a data frame for posterior inference
  d_seq <- with(
    
    .data, expand.grid(
      
      ID            = NA,
      Category      = NA,
      Activity_type = c(levels(Activity_type), NA),
      #Seasonal      = c(levels(Seasonal)     , NA),
      SA            = c(levels(SA)           , NA),
      Time_bin      = levels(Time_bin)
      
    )
  ) %>%
    
    mutate( Time_bin = as.ordered(Time_bin) ) %>%
    within( . , {
      contrasts(SA)            <- -contr.sum(2)/2
      #contrasts(Seasonal)      <- -contr.sum(2)/2
      contrasts(Activity_type) <- -contr.sum(2)/2
    } )
  
  # extract posterior draws for each combination of variables in each model
  ppred <- posterior_epred(

    object     = .fit,
    newdata    = d_seq,
    re_formula = NA

  ) %>% `colnames<-`( with(
    
    d_seq,
    paste(SA, Activity_type, Time_bin, sep = "_")
    
  ) )
  
  ## ---- Posterior Expecations ----
  # add median and 95% PPIs for each prediction to the d_seq
  post_sum <- cbind(
    
    d_seq,
    apply(
      X      = ppred,
      MARGIN = 2,
      FUN    = function(x) c(
        
        logxEstimate    = median(x),
        logxETI_low     = quantile(x, prob = .025, names = F),
        logxETI_high    = quantile(x, prob = .975, names = F),
        explogxEstimate = median( exp(x) ),
        explogxETI_low  = quantile( exp(x), prob = .025, names = F),
        explogxETI_high = quantile( exp(x), prob = .975, names = F)
        
      )
    ) %>% t()
    
  ) %>%
    
    as.data.frame() %>%
    pivot_longer(
      cols          = starts_with("log") | starts_with("explog"),
      names_to      = c("scale","term"),
      names_pattern = "(.*)x(.*)",
      values_to     = "value"
    ) %>%
    pivot_wider(
      names_from  = "term",
      values_from = "value",
      id_cols     = c("Activity_type","SA","Time_bin","scale")
    )
  
  ## ---- Pairwise Comparisons ----
  
  
  
  ## ---- Return ----
  return(post_sum)
  
}

#
# POSTERIOR INTERACTION PLOTS ----
draw_interaction_plots <- function(.posterior_expect) {
  
  # prepare plots
  plts <- lapply(
    
    set_names( x = c("log","explog"), nm = c("log","back-transformed raw") ),
    function(i) {
      
      # prepare data
      data <-
        .posterior_expect %>%
        filter( !is.na(Activity_type) ) %>%
        filter( !is.na(SA) ) %>%
        filter( scale == i )
      
      # plot both ways
      list(
        
        data %>%
          ggplot() +
          aes(x = Time_bin, y = Estimate, ymin = ETI_low, ymax = ETI_high, colour = Activity_type, group = Activity_type) + 
          geom_point(position = position_dodge(width = .5), size = 4) +
          geom_linerange(position = position_dodge(width = .5), linewidth = 1.5) +
          geom_line(linetype = "dashed", linewidth = .8) +
          scale_colour_manual( values = c("#56B4E9","#E69F00") ) +
          facet_wrap(~ SA, nrow = 2) +
          theme_bw(base_size = 12) +
          theme(panel.grid = element_blank(), legend.position = "bottom"),
        
        data %>%
          ggplot() +
          aes(x = Time_bin, y = Estimate, ymin = ETI_low, ymax = ETI_high, colour = SA, group = SA) +
          geom_point(position = position_dodge(width = .5), size = 4) +
          geom_linerange(position = position_dodge(width = .5), linewidth = 1.5) +
          geom_line(linetype = "dashed", linewidth = .8) +
          scale_colour_manual( values = c("#CC79A7","#999999") ) +
          facet_wrap(~ Activity_type, nrow = 2) +
          theme_bw(base_size = 12) +
          theme(panel.grid = element_blank(), legend.position = "bottom")
        
      )
    }
  )
  
  # put the plot together
  figs <- lapply(
    
    set_names( x = names(plts) ),
    function(i)
      with( plts, get(i)[[1]] | get(i)[2] ) +
      plot_layout(axis_titles = "collect") +
      plot_annotation(
        title = "Three-way interaction between time bin, SA, and activity type",
        subtitle = paste0("Left and right sets of panels are representation of the same interaction on a ",i," scale"),
        theme = theme( plot.title = element_text(hjust = .5, face = "bold"), plot.subtitle = element_text(hjust = .5) )
      )
    
  )
  
  # return the results
  return(figs)
  
}
