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

    )

  ) %>% do.call( rbind.data.frame, . )

) %>% do.call( rbind.data.frame, . )


# fit regressions ----
model_fit <- function(data, specs, log = T, contr = T) {
  
  # log transform if called for
  if(log == T) data <- data %>% mutate( across( all_of( c("FAQ","GDS15","GAI") ), ~ log(.x+1) ) )
  #if(contr == T) data <- data %>% mutate_if( is.factor, contr.poly() )
  
  # compute the regressions
  lapply(
    
    set_names( x = 1:nrow(specs), nm = with( specs, paste(outcome, exposure, moderator, sep = "_") ) ),
    function(i) with(
      
      specs, return( glm( formula = as.formula(formula[i]), family = likelihood[i], data = data ) )
      
    )
  ) %>% return()
  
}
