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
  
  
  # compute the means
  emm <- lapply(
    
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
  
  # extract conditional means
  est <- lapply(
    
    1:nrow(specs), function(i) with(
      
      specs, return(
        
        emm[[i]]$emmeans %>%
          as_tibble() %>%
          `colnames<-`( c("group", "mod", "Est", "SE", "df", "low.CL", "upp.CL") ) %>%
          mutate(y = outcome[i], x = exposure[i], m = moderator[i], .before = 1) %>%
          mutate( Est = paste0( rprint(Est,3),"\n[", rprint(low.CL,3),", ",rprint(upp.CL,3),"]" ) ) %>%
          select(y, x, m, mod, group, Est)
        
      )
    )
  ) %>% reduce(full_join)
  
  # extract contrasts
  comp <- lapply(
    
    X = 1:nrow(specs),
    FUN = function(i) with(
      
      specs, return(
        
        emm[[i]]$contrasts %>%
          as_tibble() %>% select( 1:5, (ncol(.)-1):ncol(.) ) %>%
          `colnames<-`( c("contrast", "mod", "Comparison", "SE", "df", "test. stat.", "p value") ) %>%
          mutate(y = outcome[i], x = exposure[i], m = moderator[i], .before = 1)
        
      )
      
    )
    
  ) %>% reduce(full_join)
  
  # prepare tables for with main/simple effects
  tab <- lapply(
    
    X = set_names( unique(est$x) ),
    FUN = function(i) est %>%
      
      filter(x == i) %>% # keep only predictor of interest
      pivot_wider( values_from = Est, names_from = group, names_prefix = paste0(i," = ") ) %>%
      left_join( comp, by = c("x","y","m","mod") ) %>% # add statistical comparisons
      
      # format variables
      mutate(
        Variable = factor(
          
          sapply(1:nrow(.), function(i) unique( sets[grepl(y[i], sets$Y), "outcome"] ) ),
          levels = c("Cognition", "Affect", "cPA"),
          ordered = T
          
        ),
        sig = if_else(`p value` < .05, "*", ""),
        m = if_else(mod == "overall", "", paste0(m," = ",mod) ),
        across( all_of(c("Comparison", "SE", "test. stat.")), ~ rprint(.x, 3) ),
        `p value` = zerolead(`p value`)
      ) %>%
      
      # final formatting touches
      select(-mod) %>%
      relocate(Variable, .before = 1) %>%
      rename("Moderator" = "m", "Contrast" = "contrast")
    
  )
  
  # prepare height and width values
  dims <- matrix(
    
    data = c(12.5, 11.5, 12.5, 10.5, 3.5, 2.5),
    ncol = 2,
    dimnames = list(x = names(tab), y = c("width","height") )
    
  )
  
  # save if called for & return the table
  if(save == T) for( x in names(tab) ) {
    
    jpeg( paste0("results_table_",x,".jpg"), units = "in", width = dims[x, "width"], height = dims[x, "height"], res = 300)
    grid.table(tab[[x]], theme = ttheme_default(), rows = NULL)
    dev.off()
    
  }
  
  # print the result
  return(tab)
  
}

