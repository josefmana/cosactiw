# This script serves for summarising results of regression analyses done previously.


# MODEL DIAGNOSTICS ----
diagnose_models <- function(model_list) lapply(
  
  set_names( names(model_list) ),
  function(type) lapply(
    
    model_list[[type]],
    function(fit) tryCatch(
      
      check_model(fit),
      error=function(e) e
      
    )
  )
)


# TABLE OF RESULTS ----
stat_test <- function(fits, specs, sets) {
  
  # extract & compare marginal means
  emm <- lapply( set_names( names(fits) ), function(i) compute_means( fits[[i]], subset(specs, estimate == i) ) )
  est <- lapply( set_names( names(emm) ), function(i) extract_means( emm[[i]], subset(specs, estimate == i) ) )
  comp <- lapply( set_names( names(emm) ), function(i) compare_means( emm[[i]], subset(specs, estimate == i) ) )
  
  # prepare a table for main/simple effects
  tabs <- lapply(
    
    set_names( names(fits) ),
    function(t) lapply(
      
      X = set_names( unique(est[[t]]$x) ),
      FUN = function(i) est[[t]] %>%
        
        filter(x == i) %>% # keep only predictor of interest
        pivot_wider( values_from = Est, names_from = group, names_prefix = paste0(i," = ") ) %>%
        left_join( comp[[t]], by = c("x","y","m","mod") ) %>% # add statistical comparisons
        
        # format variables
        mutate(
          
          Variable = factor(
            
            sapply(1:nrow(.), function(i) unique( sets[grepl(y[i], sets$Y), "outcome"] ) ),
            levels = c("Cognition", "Affect", "cPA"),
            ordered = T
            
          ),
          
          sig. = if_else(`p value` < .05, "*", ""),
          m = if_else(mod == "overall", "", paste0(m," = ",mod) ),
          Comparison = paste0( rprint(Comparison, 2),"\n(", rprint(SE, 2),")" ),
          #across( all_of( c("Comparison", "SE") ), ~ rprint(.x, 2) ),
          `test. stat.` = rprint(`test. stat.`, 3),
          `p value` = zerolead(`p value`)
          
        ) %>%
        
        # final formatting touches
        select(-mod, -SE) %>%
        relocate(Variable, .before = 1) %>%
        rename("Moderator" = "m", "Contrast" = "contrast")
      
    )
  )
  
  # print the result
  return(tabs)
  
}


# RESULTS IN A PLOT ----
plot_results <- function(data, stats, specs, save = T) {
  
  # prepare statistic text to the figures
  text <- lapply(
    
    paste0(c("un",""), "adjusted"),
    function(i) stats[[i]]$mPA %>%
      
      filter(Moderator == "") %>%
      mutate(
        #est = i,
        comp = sub("\n", " ", Comparison),
        ES = if_else(
          df != Inf,
          paste0("d<sub>",sub("adjusted", "adj.", i),"</sub> = ",comp),
          paste0("OR<sub>",sub("adjusted", "adj.", i),"</sub> = ",comp)
        ),
        stat = if_else(
          df != Inf,
          paste0("t(",df,") = ",`test. stat.`),
          paste0("z = ",`test. stat.`)
        ),
        p = if_else(
          `p value` != "< .001",
          paste0("p = ",`p value`),
          paste0("p ",`p value`)
        ),
        label = paste(ES, p, sep = ", ")
      ) %>%
      select(y, label)
    
  ) %>%
    
    reduce(full_join) %>%
    rename("Outcome" = "y") %>%
    mutate(
      x = "COSACTIW",
      y = c(25.35, 4.6, 1, -1.1, 12.2, 15.3, 1, 1, .24, 24.3, 3.7, .86, -1.75, 9.7, 12.3, .86, .86, .1)
    )
  
  # prepare data for plotting
  input <- data %>%
    
    mutate( across( c("SA","cPA","Depr","Anx"), ~ if_else(.x == 1, 1, 0) ) ) %>%
    pivot_longer(
      cols = unique(specs$outcome),
      names_to = "Outcome",
      values_to = "Score"
    ) %>%
    mutate(
      type = unlist(
        sapply(
          1:nrow(.),
          function(i) with(
            specs, ifelse(
              test = unique( likelihood[outcome == Outcome[i]] ) == "gaussian",
              yes = "continuous",
              no = "binary"
            )
          )
        ),
        use.names = F
      )
    )
  
  # extract continuous and binary variable names
  vars <- lapply(
    
    set_names( unique(input$type) ),
    function(i) unlist(
      
      unique( input[input$type == i, "Outcome"] ),
      use.names = F
      
    )
  )
  
  # plot continuous variables
  conplot <- 
    
    input %>%
    filter(type == "continuous") %>%
    
    ggplot() +
    aes(x = mPA, y = Score) +
    geom_violin(width = 1) +
    geom_boxplot(width = 0.2, alpha = 0.8) +
    stat_summary(fun = mean, geom = "point", shape = 20, size = 5, colour = "red3", fill = "red3") +
    facet_wrap( ~ Outcome, ncol = 1, scales = "free_y") +
    theme_bw() +
    theme( panel.grid = element_blank() ) +
    geom_richtext(
      data = text %>% filter(Outcome %in% vars$continuous),
      mapping = aes(y = y, x = x, label = label),
      nudge_x = .5,
      label.size = .1,
      size = 3
    )
  
  # plot binary variables
  binplot <-
    
    input %>%
    filter(type == "binary") %>%
    
    ggplot() +
    aes(x = mPA, y = Score, group = Outcome) +
    geom_point(position = position_jitter(width = .15, height = .033), alpha = .33, size = 3.3) +
    geom_smooth(method = "glm", se = F, method.args = list(family = "binomial"), colour = "red3", linewidth = 1.33) +
    scale_x_discrete( expand = expansion(add = .25 ) ) +
    scale_y_continuous(breaks = c(0,1), labels = c(0,1), name = NULL) + 
    facet_wrap(~ Outcome, ncol = 1) +
    theme_bw() +
    theme( panel.grid = element_blank() ) +
    geom_richtext(
      data = text %>% filter(Outcome %in% vars$binary),
      mapping = aes(y = y, x = x, label = label),
      nudge_x = .5,
      label.size = .1,
      size = 3
    )
  
  # put them next to each other
  plt <- conplot | binplot
  
  # save it if asked for
  if (save == T) {
    
    if ( !dir.exists("_figures") ) dir.create("_figures")
    ggsave(
      
      plot = plt,
      filename = here("_figures","data-and-stats.jpg"),
      dpi = 300,
      height = 9.9,
      width = 9
      
    )
    
  }
  
  # return the thing
  return(plt)
  
}

