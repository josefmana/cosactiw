# This script serves to pre-process data for ATC estimation via propensity scores matching.

matching <- function(data, sets, return = "data") {
  
  # extract predictor/matching pairs
  X <-
    data.frame( formula = unique(sets$matching) ) %>% # S as in 'to be Stratified by'
    mutate(x = sub( " ~.*", "", unique(formula) ),  .before = 1) %>% # add formula
    mutate( # add estimand
      estimand = sapply(

        1:nrow(.),
        function(i)
          unique( sets[sets$exposure == x[i] & sets$matching == formula[i], "estimand"] )

      )
    )
  
  # calculate propensity scores matching
  fit0 <- lapply(
    
    set_names(x = 1:nrow(X), nm = X$x),
    function(i) with(
      
      X, matchit(
        
        formula = as.formula(formula[i]),
        data = data,
        method = "full",
        distance = "glm",
        estimand = estimand[i]
        
      )
    )
  )
  
  # graphical checks
  graphs <- lapply(
    
    set_names( names(fit0) ),
    function(i)
      
      lapply(
        
        set_names( strsplit(as.character(fit0[[i]]$formula)[[3]], split = " + ", fixed = T)[[1]] ),
        function(x) bal.plot(
          
          fit0[[i]],
          var.name = x,
          which = "both",
          colors = c("navyblue","orange"),
          sample.names = c("Non-weighted","Propensity score-weighted")

        ) + theme( plot.title = element_text(hjust = .5, face = "bold") )
        
      )
  )
  
  # numerical checks
  tables <- lapply( set_names( names(fit0) ), function(i) summary(fit0[[i]]) )
  
  # data
  data <- lapply( set_names( names(fit0) ), function(i) match.data(fit0[[i]]) )
  
  # return what was asked for
  return( get(return) )

}
