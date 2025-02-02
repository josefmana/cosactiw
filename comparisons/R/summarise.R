#
# This script serves for summarising results of regression analyses done previously.
#

#
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

#
# COMPUTE MARGINAL MEANS  ----
compute_means <- function(fits, specs) lapply(
  
  X = set_names(x = 1:nrow(specs), nm = names(fits) ),
  FUN = function(i) with(
    
    specs, return(
      
      emmeans(
        
        object = fits[[ paste0(outcome[i]," ~ ",exposure[i]," | ",moderator[i]) ]],
        specs = formula( paste0("pairwise ~ ",exposure[i]) ),
        by = moderator[i],
        type = "response"
        
      )
    )
  )
  
)

#
# EXTRACT MEANS ----
extract_means <- function(means, specs, digits = 2) lapply(
  
  1:nrow(specs), function(i) with(
    
    specs, return(
      
      means[[i]]$emmeans %>%
        as_tibble() %>%
        `colnames<-`( c("group", "mod", "Est", "SE", "df", "low.CL", "upp.CL") ) %>%
        mutate(y = outcome[i], x = exposure[i], m = moderator[i], .before = 1) %>%
        mutate( Est = paste0( rprint(Est,digits),"\n(", rprint(SE,digits),")" ) ) %>%
        select(y, x, m, mod, group, Est)
      
    )
  )
) %>% reduce(full_join)

#
# COMPARE MEANS ----
compare_means <- function(means, specs) lapply(
  
  X = 1:nrow(specs),
  FUN = function(i) with(
    
    specs, return(
      
      # use the following to get confidence intervals:
      # means[[i]] %>% confint()
      means[[i]]$contrasts %>%
        as_tibble() %>% select( 1:5, (ncol(.)-1):ncol(.) ) %>%
        `colnames<-`( c("contrast", "mod", "Comparison", "SE", "df", "test. stat.", "p value") ) %>%
        mutate(y = outcome[i], x = exposure[i], m = moderator[i], .before = 1)
      
    )
    
  )
  
) %>% reduce(full_join)

#
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

#
# RESULTS IN A PLOT ----
plot_results <- function(data, stats, specs, type, save = T) {
  
  # keep only variables of interest to plotting
  specs <- subset(specs, analysis == type)
  
  # prepare statistic text to the figures
  text <- lapply(
    
    paste0(c("un",""), "adjusted"),
    function(i) stats[[i]]$mPA %>%
      
      filter(Moderator == "") %>%
      filter(y %in% specs$outcome) %>%
      mutate(
        #est = i,
        comp = sub("\n", " ", Comparison),
        ES = if_else(
          df != Inf,
          paste0("d<sub>", sub("adjusted", "adj.", i),"</sub> = ", comp),
          paste0("OR<sub>",sub("adjusted", "adj.", i),"</sub> = ", comp)
        ),
        stat = if_else(
          df != Inf,
          paste0("t(",df,") = ",`test. stat.`),
          paste0("z = ",`test. stat.`)
        ),
        p = if_else(
          `p value` != "< .001",
          paste0("p = ",`p value`),
          paste0("p ",  `p value`)
        ),
        label = paste(ES, p, sep = ", ")
      ) %>%
      select(y, label)
    
  ) %>%
    
    reduce(full_join) %>%
    rename("Outcome" = "y") %>%
    mutate(
      x = "COSACTIW",
      y = unlist(
        case_when(
          type == 1 ~ list( c(4.77, 1, -1.4, 13.33, 16.3, 1, 1, .24, 4.12, .86, -1.9, 11.5, 14.12, .86, .86, .1) ),
          type == 2 ~ list( c(-1.99, -3.85, -3.53, -2.02, -2.4, -4.4, -4, -2.5) )
        )
      )
    )
  
  # prepare data for plotting
  input <- data %>%
    
    mutate( across( c("SA","cPA","Depr","Anx"), ~ if_else(.x == 1, 1, 0) ) ) %>%
    pivot_longer(
      cols     = unique(specs$outcome),
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
              yes  = "continuous",
              no   = "binary"
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
  
  # labels for continuous plots
  labs <- unlist(
    
    if_else(
      condition = type == 1,
      true      = list( c(FAQ = "FAQ", GAI = "GAI", GDS15 = "GDS-15",Z_SA = "Cognition CS") ),
      false     = list( c(Delayed_recall_z = "Memory (delayed recall)", TMT_B_z = "TMT-B", BNT_30_z = "BNT-30", VF_Animals_z = "Verbal Fluency (animals)") )
    )
    
  )
  
  # levels of factor for continuous plots
  levs <- unlist(
    
    if_else(
      condition = type == 1,
      true      = list( c("GAI", "GDS15", "FAQ", "Z_SA") ),
      false     = list( c("Delayed_recall_z", "TMT_B_z", "BNT_30_z", "VF_Animals_z") )
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
    labs(y = case_when(type == 1 ~ "Score", type == 2 ~ "Z-score"), x = NULL) +
    facet_wrap(
      facets = ~ factor(Outcome, levels = levs), # factor to force order
      ncol = type,
      scales = "free_y",
      labeller = as_labeller(labs)
    ) +
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
    geom_point(position = position_jitter(width = .15, height = .033), alpha = .25, size = 3.3) +
    geom_smooth(method = "glm", se = F, method.args = list(family = "binomial"), colour = "red3", linewidth = 1.33) +
    scale_x_discrete( expand = expansion(add = .25 ) ) +
    scale_y_continuous(breaks = c(0,1), labels = c(0,1), name = NULL) + 
    labs(x = NULL) +
    facet_wrap(
      facets = ~ factor( Outcome, levels = c("Anx", "Depr", "cPA", "SA") ),
      ncol = 1,
      scales = "free_y",
      labeller = as_labeller( c(Anx = "Anxiety risk", cPA = "cPA", Depr = "Depression risk", SA = "SA") )
    ) +
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
  if (type == 1) plt <- conplot | binplot else plt <- conplot
  
  # prepare file name
  if (type == 1) fname <- "data-and-stats.jpg" else fname <- "cognitive-tests.jpg"
  
  # save it if asked for
  if (save == T) {
    
    if ( !dir.exists("_figures") ) dir.create("_figures")
    ggsave(
      
      plot     = plt,
      filename = here("_figures", fname),
      dpi      = 300,
      height   = case_when(type == 1 ~ 9.9, type == 2 ~ 7.5),
      width    = case_when(type == 1 ~ 9.0, type == 2 ~ 7.5)
      
    )
    
  }
  
  # return the thing
  return(plt)
  
}

