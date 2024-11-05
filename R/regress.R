# This script is supposed to fit and summarise regression models that can help with answering following questions:
#
# RQ1.1) How strong is the total causal effect of midlife-PA on cognitive variables in our sample?
# RQ1.2) How strong is the total causal effect of midlife-PA on cognitive variables conditional on Education in our sample?
# RQ1.3) How strong is the total causal effect of Education on cognitive variables in our sample?
# RQ1.4) How strong is the total causal effect of current-PA on cognitive variables in our sample?
#
# RQ2.1) How strong is the total causal effect of midlife-PA on mental health variables in our sample?
# RQ2.2) How strong is the total causal effect of midlife-PA on mental health variables conditional on Education in our sample?
# RQ2.4) How strong is the total causal effect of current-PA on mental health variables in our sample?
#
# RQ3.1) How strong is the total causal effect of midlife-PA on current-PA in our sample?


# UTILITY FUNCTIONS ----

rprint <- function(x, d=2) sprintf( paste0("%.",d,"f"), round(x, d) )
zerolead <- function(x, d = 3) ifelse( x < .001, "< .001", sub("0","", rprint(x, d) ) )
remove_brackets <- function(x) sub("{", "", sub("}", "", x, fixed = T), fixed = T)


# MODEL SPECIFICATIONS ----
model_specs <- function(table, faq = "count") lapply(
  
  1:nrow(table),
  function(i) lapply(
    
    strsplit(unlist( remove_brackets(table[i, "Y"]), use.names = F), ", ", fixed = T)[[1]],
    function(y) data.frame(
      
      outcome = y,
      exposure = table[i, "exposure"],
      moderator = ifelse( is.na( table[i, "moderator"] ), "1", table[i, "moderator"] ),
      term = table[i , "term"],
      likelihood = case_when(
        y %in% c("MMSE","Z_SA","GDS15","GAI") ~ "gaussian",
        y %in% c("SA","cPA","Depr","Anx") ~ "binomial",
        y == "FAQ" & faq == "continuous" ~ "gaussian",
        y == "FAQ" & faq == "count" ~ "binomial"
      ),
      adjusted = paste0( y," ~ ",table[i, "X"] ), # formula for causal/adjusted models
      unadjusted = paste0( y," ~ ",table[i, "term"] ) # formula for descriptive/unadjusted models
      
    )
    
  ) %>% do.call( rbind.data.frame, . )
  
) %>%
  
  do.call( rbind.data.frame, . ) %>%
  pivot_longer( cols = c("adjusted", "unadjusted"), names_to = "estimate", values_to = "formula" )


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


# COMPARE MEANS ----
compare_means <- function(means, specs) lapply(
  
  X = 1:nrow(specs),
  FUN = function(i) with(
    
    specs, return(
      
      means[[i]]$contrasts %>%
        as_tibble() %>% select( 1:5, (ncol(.)-1):ncol(.) ) %>%
        `colnames<-`( c("contrast", "mod", "Comparison", "SE", "df", "test. stat.", "p value") ) %>%
        mutate(y = outcome[i], x = exposure[i], m = moderator[i], .before = 1)
      
    )
    
  )
  
) %>% reduce(full_join)


# FIT REGRESSIONS ----
fit_models <- function(data, specs, log_1 = c("GDS15","GAI"), contr = T) {
  
  # log transform variables if called for
  if ( !is.null(log_1) ) data <- data %>%
      mutate(
        across(
          all_of(log_1),
          ~ log(.x+1)
        )
      )
  
  # do orthogonal contrasts if called for
  if(contr == T) {
    for( i in names(data) ) {
      if ( is.factor(data[ , i]) ) contrasts(data[ , i]) <- contr.sum( length( levels(data[ , i]) ) )
    }
  }
  
  # centre age
  data$Age <- as.numeric( scale(data$Age, center = T, scale = F) )
  
  # mutate FAQ if it is binomial
  if (c( unique( specs[specs$outcome == "FAQ", "likelihood"] ) ) == "binomial") data$FAQ = data$FAQ / 10
  
  # compute regressions
  lapply(
    
    set_names( x = paste0(c("un", ""),"adjusted") ),
    function(t) {
      
      # prepare specific specification file
      model_specs <- subset(specs, estimate == t)
      
      # loop through all models to be fitted
      lapply(
        
        set_names(
          x = 1:nrow(model_specs),
          nm = with( model_specs, paste0(outcome," ~ ",exposure," | ",moderator) )
        ),
        
        function(i) with(
          
          model_specs, return(
            
            glm(
              formula = as.formula(formula[i]),
              family = likelihood[i],
              data = data,
              weights = unlist(
                ifelse(
                  test = outcome[i] == "FAQ" & likelihood[i] == "binomial",
                  yes = list( rep(10, nrow(data) ) ),
                  no = list(NULL)
                )
              )
            )
            
          )
        )
        
      ) %>% return()
      
    }

  ) %>% return()
  
}

