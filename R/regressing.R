# This script is supposed to fit and summarise regression models that can help with answering following questions:
#
# RQ1.1) How strong is the total causal effect of midlife-PA on cognitive variables in our sample?
# RQ1.2) How strong is the total causal effect of midlife-PA on cognitive variables conditional on Education in our sample?
# RQ1.3) How strong is the total causal effect of Education on cognitive variables in our sample?
# RQ1.4) How strong is the total causal effect of Marital Status on cognitive variables in our sample?
#
# RQ2.1) How strong is the total causal effect of midlife-PA on mental health variables in our sample?
# RQ2.2) How strong is the total causal effect of midlife-PA on mental health variables conditional on Education in our sample?
# RQ2.3) How strong is the total causal effect of Education on mental health variables in our sample?
# RQ2.4) How strong is the total causal effect of Marital Status on mental health variables in our sample?
#
# RQ3.1) How strong is the total causal effect of midlife-PA on current-PA in our sample?
# RQ3.2) How strong is the total causal effect of Marital Status on current-PA in our sample?


# utility functions ----
rprint <- function(x, d=2) sprintf( paste0("%.",d,"f"), round(x, d) )
zerolead <- function(x, d = 3) ifelse( x < .001, "< .001", sub("0","", rprint(x, d) ) )

# model specifications ----
model_specs <- function(table) lapply(
  
  1:nrow(table),
  function(i) lapply(
    
    strsplit(unlist(table[i, "outcome_variables"], use.names = F), ", ", fixed = T)[[1]],
    function(y) data.frame(
      
      outcome = y,
      exposure = table[i, "exposure"],
      moderator = ifelse( is.na( table[i, "moderator"] ), "none", table[i, "moderator"] ),
      formula = paste0( y, " ~ ", table[i, "X"] ),
      likelihood = case_when(
        y %in% c("MMSE","FAQ","Z_SA","GDS15","GAI") ~ "gaussian",
        y %in% c("SA","PA") ~ "binomial"
      )

    ) %>% mutate(
      
      # add variable column for better work
      exposure_column = case_when(
        exposure == "m-PA" ~ "Cosactiw",
        exposure == "Marital Status" ~ "Marital_status",
        .default = exposure
      ),
      
      term = if_else(
        condition = moderator == "none",
        true = exposure_column,
        false = paste0(exposure_column,":",moderator)
      ),
      
      .after = moderator
      
    )

  ) %>% do.call( rbind.data.frame, . )

) %>% do.call( rbind.data.frame, . )


# fit regressions ----
model_fit <- function(data, specs, log = T, contr = T) {
  
  if(log == T) data <- data %>% mutate( across( all_of( c("FAQ","GDS15","GAI") ), ~ log(.x+1) ) ) # log transform if called for
  if(contr == T) for( i in names(data) ) if ( is.factor(data[ ,i]) ) contrasts(data[ ,i]) <- contr.sum( length( levels(data[ ,i]) ) ) # do orthogonal contrasts if called for
  data$Age <- as.numeric( scale(data$Age, center = T, scale = F) ) # centre age
  
  # compute the regressions
  lapply(
    
    set_names( x = 1:nrow(specs), nm = with( specs, paste(outcome, exposure_column, moderator, sep = "-") ) ),
    function(i) with(
      
      specs, return( glm( formula = as.formula(formula[i]), family = likelihood[i], data = data ) )
      
    )
  ) %>% return()
  
}


# model diagnostics ----
model_diagnose <- function(model_list) lapply( model_list, function(fit) check_model(fit) )


# extract results of statistical tests ----
stat_test <- function(fits, specs, sets, save = T) {
  
  # prepare the table
  tab <- left_join(
    
    # extract ANOVA-like tables
    specs, lapply(
      
      1:nrow(specs),
      function(i) with(
        
        specs,
        joint_tests( fits[[ paste(outcome[i], exposure_column[i], moderator[i], sep = "-") ]] ) %>%
          filter(`model term` == term[i] ) %>% # keep only the causal estimate
          mutate(outcome = outcome[i], .before = 1) # add outcome variable for later glueing
        
      )
      
    ) %>%
      
      # tidy it up
      reduce(full_join) %>%
      rename("term" = "model term"),
    
    # pull specifications and results to a single file
    by = c("outcome", "term")
    
  ) %>%
    
    # finish it
    select(outcome, exposure, moderator, Chisq, F.ratio, df1, df2, p.value) %>% # keep variables of interest
    mutate(
      variable = factor(
        sapply(1:nrow(.), function(i) unique( sets[grepl(outcome[i], sets$outcome_variables), "outcome"] ) ),
        levels = c("Cognition", "Affect", "c-PA"),
        ordered = T
      ),
      outcome = if_else(outcome == "PA", "c-PA", outcome),
      moderator = if_else(moderator == "none", "-", moderator),
      Chisq = if_else(is.na(Chisq), "-", rprint(Chisq, 3) ),
      F.ratio = rprint(F.ratio, 3),
      df1 = as.character(df1),
      df2 = if_else(df2 == Inf, "-", as.character(df2) ),
      sig = if_else(p.value < .05, "*", ""),
      p.value = zerolead(p.value)
    ) %>% # format variables of interest
    relocate(variable, .before = 1) %>%
    arrange(variable)
  
  # save if called for & return the table
  if(save == T) {
    
    jpeg("results_table.jpg", units = "in", width = 7.5, height = 7, res = 300)
    grid.table(tab, theme = ttheme_default(), rows = NULL)
    dev.off()
    
  }
  return(tab)
  
}

