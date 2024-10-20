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
remove_brackets <- function(x) sub("{", "", sub("}", "", x, fixed = T), fixed = T)

# model specifications ----
model_specs <- function(table) lapply(
  
  1:nrow(table),
  function(i) lapply(
    
    strsplit(unlist( remove_brackets(table[i, "Y"]), use.names = F), ", ", fixed = T)[[1]],
    function(y) data.frame(
      
      outcome = y,
      exposure = table[i, "exposure"],
      moderator = ifelse( is.na( table[i, "moderator"] ), "1", table[i, "moderator"] ),
      term = table[i , "term"],
      formula = paste0( y, " ~ ", table[i, "X"] ),
      likelihood = case_when(
        y %in% c("MMSE","FAQ","Z_SA","GDS15","GAI") ~ "gaussian",
        y %in% c("SA","cPA") ~ "binomial"
      )

    )

  ) %>% do.call( rbind.data.frame, . )

) %>% do.call( rbind.data.frame, . )


# fit regressions ----
model_fit <- function(data, specs, log = T, contr = T) {
  
  # log transform if called for
  if(log == T) data <- data %>%
      mutate(
        across(
          all_of( c("FAQ","GDS15","GAI") ),
          ~ log(.x+1)
        )
      )
  
  # do orthogonal contrasts if called for
  if(contr == T) for( i in names(data) ) {
    
    if ( is.factor(data[ ,i]) ) contrasts(data[ ,i]) <- contr.sum( length( levels(data[ ,i]) ) )
    
  }
  
  # centre age
  data$Age <- as.numeric( scale(data$Age, center = T, scale = F) )
  
  # compute the regressions
  lapply(
    
    set_names( x = 1:nrow(specs), nm = with( specs, paste0(outcome," ~ ",exposure," | ",moderator) ) ),
    function(i) with(
      
      specs,
      return( glm( formula = as.formula(formula[i]), family = likelihood[i], data = data ) )
      
    )
  ) %>% return()
  
}


# model diagnostics ----
model_diagnose <- function(model_list) lapply( model_list, function(fit) check_model(fit) )


# extract results of statistical tests ----
stat_test <- function(fits, specs, sets, save = T) {
  
  # extract the emmeans
  emm <- lapply(
    
    X = set_names(x = 1:nrow(specs), nm = names(fits) ),
    FUN = function(i) with(
      
      specs, return(
        
        emmeans(
          object = fits[[ paste0(outcome[i]," ~ ",exposure[i]," | ",moderator[i]) ]],
          specs = formula( paste0("pairwise ~ ",exposure[i]) ),
          by = moderator[i],
          type = "response"
        ) %>%
          
          as.data.frame() #%>%
          
        
      )
    )
  )
  
  # prepare the table
  tab <- left_join(
    
    # extract ANOVA-like tables
    specs, lapply(
      
      1:nrow(specs),
      function(i) with(
        
        specs,
        joint_tests( fits[[ paste0(outcome[i]," ~ ",exposure[i]," | ",moderator[i]) ]] ) %>%
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
      
      # format variables of interest
      variable = factor(
  
        sapply(1:nrow(.), function(i) unique( sets[grepl(outcome[i], sets$Y), "outcome"] ) ),
        levels = c("Cognition", "Affect", "cPA"),
        ordered = T

      ),
      Chisq = if_else(is.na(Chisq), "-", rprint(Chisq, 3) ),
      F.ratio = rprint(F.ratio, 3),
      df1 = as.character(df1),
      df2 = if_else(df2 == Inf, "-", as.character(df2) ),
      sig = if_else(p.value < .05, "*", ""),
      p.value = zerolead(p.value)

    ) %>%

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

