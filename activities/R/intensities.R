#
# Functions to describe retrospectively recalled leisure activities with respect to cognitive SuperAging
# via Bayesian regressions.
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
      Intensity    = as.integer( if_else(Intensity == "do not know", NA, Intensity) ) - 1, # subtracting 1 so that the scale starts from 1 not 2
      logIntensity = log( as.numeric(Intensity) ),

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
perform_posterior_checks <- function(.fits, .data, save = T) lapply(
  
  set_names( names(.fits) ),
  function(y) lapply(
    
    set_names( x = c("dens", "mean", "sd") ),
    function(f) {
      
      # do the plotting
      if (f == "dens") plt <- ppc_plot(
        
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
        
      ) else plt <- ppc_plot(
        
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
      
      # save if asked for
      if (save == T) {
        
        # prepare a folder
        new_folder("_figures")
        
        # save it
        ggsave(
          
          filename = here( "_figures", paste0("ppc_", f,"_", y, ".jpg") ),
          plot = plt,
          dpi = 300,
          width = 12.6,
          height = 13.3
          
        )
      }
      
      # return the result
      return(plt)
      
    } 
  )
)

#
# EXTRACT POSTERIOR EXPECTATIONS ----
compute_posterior_expectations <- function(.data, .fit, output = "expectations") {
  
  
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
  expectations <- cbind(
    
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
  
  # prepare a summary data frame
  post_comp <- list(
      
      # activity type and SA comparisons
      expand_grid(
        diff    = c("physical - mental", "SA - nonSA", "physical_SA - mental_SA", "physical_nonSA - mental_nonSA"),
        diff2   = NA,
        timebin = levels(d_seq$Time_bin),
        actype  = NA,
        SA      = NA
      ),
      
      # two-way interactions of interest
      expand_grid(
        diff    = "physical - mental",
        diff2   = "SA - nonSA",
        timebin = levels(d_seq$Time_bin),
        actype  = NA,
        SA      = NA
      ),
      
      # time comparisons
      expand_grid(
        diff     =
          data.frame( v2 = seq(30,80,5) ) %>%
          mutate( v1 = v2 + 5 ) %>%
          mutate( diff = paste0(v1," - ",v2) ) %>%
          select(diff) %>%
          unlist(use.names = F) %>%
          c("85 - 30"),
        diff2   = NA,
        timebin = NA,
        actype  = c("mental","physical",NA),
        SA      = c("SA","nonSA",NA)
      ),
      
      # two-way interactions with time
      expand_grid(diff = "85 - 30", diff2 = "SA - nonSA", timebin = NA, actype = c("mental","physical",NA), SA = NA),
      expand_grid(diff = "85 - 30", diff2 = "physical - mental", timebin = NA, actype = NA, SA = c("SA","nonSA",NA) )
      
    ) %>%
      
      do.call( rbind.data.frame, . ) %>%
      mutate( # re-code
        SA   = case_when(
          diff == "physical_SA - mental_SA"       ~ "SA",
          diff == "physical_nonSA - mental_nonSA" ~ "nonSA",
          .default = SA
        ),
        diff = gsub("_SA|_nonSA", "", diff)
      )
  
  # summarise the contrasts (pairwise comparisons)
  contrasts <- lapply(
    
    set_names( x = c("log","logexp") ),
    function(s)
      
      with(
        
        post_comp, sapply(
          
          X = 1:nrow(post_comp),
          FUN = function(i) {
            
            # extract terms
            term1 <- strsplit(diff[i], " - ")[[1]][1]
            term2 <- strsplit(diff[i], " - ")[[1]][2]
            
            # find out which variable is being compared
            # and extract column name appropriately
            if ( term1 %in% na.omit( unique(actype) ) ) {
              
              col1 <- paste(SA[i], term1, timebin[i], sep = "_")
              col2 <- paste(SA[i], term2, timebin[i], sep = "_")
              
            } else if ( term1 %in% na.omit( unique(SA) ) ) {
              
              col1 <- paste(term1, actype[i], timebin[i], sep = "_")
              col2 <- paste(term2, actype[i], timebin[i], sep = "_")
              
            } else if ( term1 %in% na.omit( unique(timebin) ) ) {
              
              col1 <- paste(SA[i], actype[i], term1, sep = "_")
              col2 <- paste(SA[i], actype[i], term2, sep = "_")
              
            }
            
            # extract the resulting posterior
            if   (s == "logexp") dif <- exp( ppred[ , col1] ) - exp( ppred[ , col2] )
            else if (s == "log") dif <-      ppred[ , col1]   -      ppred[ , col2]
            
            # compute interaction if relevant
            if( !is.na(diff2[i]) ) {
              
              # extract second terms
              term21 <- strsplit(diff2[i], " - ")[[1]][1]
              term22 <- strsplit(diff2[i], " - ")[[1]][2]
              
              # can be only SA or activity_type
              # find out columns to be compared
              if ( term1 %in% na.omit( unique(actype) ) ) {
                
                # term2 is SA
                col11 <- paste(term21, term1, timebin[i], sep = "_")
                col12 <- paste(term21, term2, timebin[i], sep = "_")
                col21 <- paste(term22, term1, timebin[i], sep = "_")
                col22 <- paste(term22, term2, timebin[i], sep = "_")
                
              }  else if ( term1 %in% na.omit( unique(timebin) ) ) {
                
                # term2 can be SA or activity_type
                if ( term21 %in% na.omit( unique(actype) ) ) {
                  
                  # term2 is activity_type
                  col11 <- paste(SA[i], term21, term1, sep = "_")
                  col12 <- paste(SA[i], term21, term2, sep = "_")
                  col21 <- paste(SA[i], term22, term1, sep = "_")
                  col22 <- paste(SA[i], term22, term2, sep = "_")
                  
                } else if ( term21 %in% na.omit( unique(SA) ) ) {
                  
                  # term2 is SA
                  col11 <- paste(term21, actype[i], term1, sep = "_")
                  col12 <- paste(term21, actype[i], term2, sep = "_")
                  col21 <- paste(term22, actype[i], term1, sep = "_")
                  col22 <- paste(term22, actype[i], term2, sep = "_")
                  
                }
                
              }
              
              # extract the resulting posterior
              if   (s == "logexp") dif <- ( exp( ppred[ , col11] ) - exp( ppred[ , col12] ) ) - ( exp( ppred[ , col21] ) - exp( ppred[ , col22] ) )
              else if (s == "log") dif <- (      ppred[ , col11] -        ppred[ , col12] )   - (      ppred[ , col21]   -      ppred[ , col22] )
              
            }
            
            # extract final results
            est <- paste0( rprint(median(dif), 2)," ", ciprint( unlist( eti(dif) )[-1] ) )
            pd  <- paste0( rprint( 100 * c( p_direction(dif) )$pd, 2 ), "%" )
            return( c(Estimate = est, pd = pd) )
            
          }
        )
      ) %>%
      
      t() %>%
      cbind.data.frame( post_comp, . ) %>%
      mutate(scale = s, .before = Estimate)
    
  ) %>%
    
    do.call( rbind.data.frame, . ) %>%
    pivot_wider( names_from = scale, values_from = c(Estimate, pd) ) %>%
    relocate(pd_log, .after = Estimate_log)
  
  
  
  ## ---- Return ----
  return( get(output) )
  
}

#
# POSTERIOR INTERACTION PLOTS ----
draw_interaction_plots <- function(.posterior_expect, text = F, save = T) {
  
  # prepare plots
  plts <- lapply(
    
    set_names( x = c("log","explog"), nm = c("log","back-transformed raw") ),
    function(i) {
      
      # prepare data
      data <-
        .posterior_expect %>%
        filter( !is.na(Activity_type) ) %>%
        filter( !is.na(SA) ) %>%
        filter( scale == i ) %>%
        mutate( # re-format some for easier time plotting
          SuperAging = factor(
            x = if_else(SA == "SA", 1, 0),
            levels = 0:1,
            labels = c("Yes", "No")
          ),
          `Activity type` = factor(
            x = case_when(
              Activity_type == "physical" ~ 1,
              Activity_type == "mental"   ~ 2
            ),
            levels = 1:2,
            labels = c("Physical", "Mental")
          )
        )
      
      # plot both ways
      list(
        
        data %>%
          ggplot() +
          aes(x = Time_bin, y = Estimate, ymin = ETI_low, ymax = ETI_high, colour = `Activity type`, group = `Activity type`) + 
          geom_point(position = position_dodge(width = .5), size = 4) +
          geom_linerange(position = position_dodge(width = .5), linewidth = 1.5) +
          geom_line(linetype = "dashed", linewidth = .8) +
          scale_colour_manual( values = c("#56B4E9","#E69F00") ) +
          labs(y = ifelse(i == "log", "log(Intensity)", "Intensity (1-5 points)"), x = "Time bin (years)") +
          facet_wrap( ~ SA, nrow = 2, labeller = as_labeller( c(SA = "SuperAging", nonSA = "non-SuperAging") ) ) +
          theme_bw(base_size = 12) +
          theme(legend.position = "bottom"),
        
        data %>%
          ggplot() +
          aes(x = Time_bin, y = Estimate, ymin = ETI_low, ymax = ETI_high, colour = SuperAging, group = SuperAging) +
          geom_point(position = position_dodge(width = .5), size = 4) +
          geom_linerange(position = position_dodge(width = .5), linewidth = 1.5) +
          geom_line(linetype = "dashed", linewidth = .8) +
          scale_colour_manual( values = c("#CC79A7","#999999") ) +
          labs(y = ifelse(i == "log", "log(Intensity)", "Intensity (1-5 points)"), x = "Time bin (years)") +
          facet_wrap(~ `Activity type`, nrow = 2) +
          theme_bw(base_size = 12) +
          theme(legend.position = "bottom")
        
      )
    }
  )
  
  # put the plot together
  figs <- lapply(
    
    set_names( x = names(plts) ),
    function(i) {
      
      # prepare a plot
      output <-
        with( plts, get(i)[[1]] | get(i)[2] ) +
        plot_layout(axis_titles = "collect")
      
      # add text if asked for
      if (text == T) output <-
          
          output +
          plot_annotation(

            title    = ifelse(text == F, NULL, "Three-way interaction between time bin, SA, and activity type"),
            subtitle = ifelse(text == F, NULL, paste0("Left and right sets of panels are representation of the same interaction on a ",i," scale") ),
            theme    = theme( plot.title = element_text(hjust = .5, face = "bold"), plot.subtitle = element_text(hjust = .5) )

          )
      
      # saving stuff
      new_folder("_figures") # prepare a folder
      fn <- paste0("conditional_means_", gsub("-| ","_", i),"_scale.jpg") # prepare filename
      
      # save if asked for
      if (save == T ) ggsave(
        
        plot     = last_plot(),
        filename = here("_figures",fn),
        dpi      = 300,
        width    = 12.6,
        height   = 8.31
  
      )
      
    }
  )
  
  # return the results
  return(figs)
  
}
