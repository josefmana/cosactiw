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


# utility functions ----

rprint <- function(x, d=2) sprintf( paste0("%.",d,"f"), round(x, d) )
zerolead <- function(x, d = 3) ifelse( x < .001, "< .001", sub("0","", rprint(x, d) ) )
remove_brackets <- function(x) sub("{", "", sub("}", "", x, fixed = T), fixed = T)

# compute predictions ----
model_predictions <- function(model, variables, trans = NULL) avg_predictions(
  
  model,
  variables = variables,
  wts = "weights", # will not affect descriptive models' predictions
  transform = trans
  
) %>%
  
  as.matrix() %>%
  as_tibble()

# compute marginal means for a set of models ----
compute_means <- function(models, specifications) lapply(
  
  set_names( names(models) ),
  function(t) {
    
    # prepare specifications of models to be used
    model_specs <- subset(specifications, model_type == t)
    
    # loop through models
    lapply(
      
      1:nrow(model_specs),
      function(i) with(
        
        model_specs, model_predictions(
          
          model = models[[t]][[paste0(outcome[i]," ~ ",exposure[[i]]," | ",moderator[i])]],
          if (moderator[i] == 1) variables = exposure[i] else variables = c(exposure[i], moderator[i]),
          if ( outcome[i] %in% c("FAQ","GDS15","GAI") ) trans = function(x) (exp(x)-1) else trans = NULL
          
        ) %>% mutate(
          
          y = outcome[i], x = exposure[i], m = moderator[i],
          .before = 1
          
        )
        
      )
    ) %>%
      
      reduce(full_join) %>%
      mutate(
        across( all_of( c("estimate","p.value","s.value","conf.low","conf.high") ), as.numeric ),
        estimate = rprint(estimate, 2),
        Est = paste0(estimate,"\n[", rprint(conf.low, 2),", ",rprint(conf.high, 2),"]"),
        mod = case_when(m == "cPA" ~ cPA, m == "Education" ~ Education)
      )
  }
)


# compute contrast results ----
g_computation <- function(model, nd, y, mod, comp, trans = NULL, int = F) avg_comparisons(

  model = model,
  newdata = nd,
  variables = y,
  by = mod,
  vcov = ~ subclass,
  wts = "weights", # does not affect descriptive models
  comparison = comp,
  transform = unlist( ifelse( is.null(trans), list(NULL), list(trans) ) ),
  hypothesis = unlist( ifelse( int == T, list("revpairwise"), list(NULL) ) )

) %>%

  as.matrix() %>%
  as_tibble()


# compare means from g-computation ----
compare_means <- function(models, specifications, data) lapply(
  
  set_names( names(models) ),
  function(t) {
    
    # prepare specifications of models to be used
    model_specs <- subset(specifications, model_type == t)
    
    # loop through models
    lapply(
      
      1:nrow(model_specs),
      function(i) with(
        
        model_specs, g_computation(
          
          # model
          model = models[[t]][[paste0(outcome[i]," ~ ",exposure[[i]]," | ",moderator[i])]],
          
          # data
          if (t == "descriptive") nd = data[[exposure[i]]]
          else if (exposure[i] == "mPA") nd = subset(data[[exposure[i]]], mPA == "NANOK")
          else if (exposure[i] == "cPA") nd = subset(data[[exposure[i]]], cPA == 0)
          else if (exposure[i] == "Education") nd = subset(data[[exposure[i]]], Education == "lower"),
          
          # hypothesis
          if (exposure[i] == "mPA") y = list(mPA = "revpairwise")
          else if (exposure[i] == "cPA") y = list(cPA = "pairwise")
          else if (exposure[i] == "Education") y = list(Education = "pairwise"),
          
          # other specification
          if (moderator[i] == 1) mod = F else mod = moderator[i], # moderator if any
          if (likelihood[i] == "binomial") comp = "lnratioavg" else comp = "differenceavg", # type of comparison
          if (likelihood[i] == "binomial") trans = "exp" else trans = NULL, # transformation
          int = F # calculate interaction contrasts (int = T) or simple effect contrasts (int = F)
          
        ) %>% mutate(
          
          y = outcome[i], x = exposure[i], m = moderator[i],
          .before = 1
          
        )
        
      )
    ) %>%
      
      reduce(full_join) %>%
      select( -starts_with("predicted") ) %>%
      mutate(
        across(all_of( c("estimate","std.error","statistic","p.value","s.value","conf.low","conf.high") ), as.numeric),
        across(starts_with("conf"), rprint),
        across(all_of( c("std.error","statistic","s.value") ), rprint),
        across( where(is.factor), as.character ),
        #p.value = zerolead(p.value),
        estimate = if_else(estimate < 100, rprint(estimate, 2), "≥ 100"),
        mod = case_when(m == "cPA" ~ cPA)
      )
  }
)


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
      estimand = table[i, "estimand"],
      likelihood = case_when(
        y %in% c("MMSE","FAQ","Z_SA","GDS15","GAI") ~ "gaussian",
        y %in% c("SA","cPA","Depr","Anx") ~ "binomial"
      ),
      causal = paste0( y," ~ ",table[i, "X"] ), # formula for causal models
      descriptive = paste0( y," ~ ",table[i, "term"] ) # formula for descriptive models

    )

  ) %>% do.call( rbind.data.frame, . )

) %>%
  
  do.call( rbind.data.frame, . ) %>%
  pivot_longer( cols = c("causal", "descriptive"), names_to = "model_type", values_to = "formula" ) %>%
  mutate( estimand = if_else(model_type == "causal", estimand, "-") )


# fit regressions ----
model_fit <- function(data_list, specs, log = T, contr = T) {
  
  # pre-process
  for ( i in names(data_list) ) {
    
    # log transform if called for
    if(log == T) data_list[[i]] <- data_list[[i]] %>%
        mutate(
          across(
            all_of( c("FAQ","GDS15","GAI") ),
            ~ log(.x+1)
          )
        )
    
    # do orthogonal contrasts if called for
    if(contr == T) for( j in names(data_list[[i]]) ) if ( is.factor(data_list[[i]][ ,j]) ) {

      contrasts(data_list[[i]][ ,j]) <- contr.sum( length( levels(data_list[[i]][ ,j]) ) )

    }
    
    # centre age
    data_list[[i]]$Age <- as.numeric( scale(data_list[[i]]$Age, center = T, scale = F) )
    
  }
  
  # compute regressions
  lapply(
    
    set_names( x = c("descriptive", "causal") ),
    function(t) {
      
      # prepare specific specification file
      model_specs <- subset(specs, model_type == t)
      
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
              data = data_list[[exposure[i]]],
              if (t == "causal") weights = weights
            )
            
          )
        )
        
      ) %>% return()
      
    }

  ) %>% return()
  
}


# model diagnostics ----
model_diagnose <- function(model_list) lapply(
  
  set_names( names(model_list) ),
  function(type) lapply(
    
    model_list[[type]],
    function(fit) tryCatch(
      
      check_model(fit),
      error=function(e) e
      
    )
  )
)


# extract results of statistical tests ----
stat_test <- function(fits, specs, data, sets) {
  
  # extract & compare marginal means
  marg_means <- compute_means(fits, specs)
  contrasts <- compare_means(fits, specs, data)
  
  # prepare a table for main/simple effects
  tabs <- lapply(
    
    set_names( names(fits) ),
    function(t)
      
      lapply(
        
        X = set_names( unique(specs$exposure) ),
        FUN = function(i) marg_means[[t]] %>%
          
          filter(x == i) %>% # keep only predictor of interest
          select( all_of( c("y", "x", "m", i, "mod", "Est") ) ) %>%
          pivot_wider( values_from = "Est", names_from = i, names_prefix = paste0(i," = ") ) %>%
          
          # add statistical comparisons
          left_join(
            contrasts[[t]] %>% select(y, x, m, contrast, estimate, std.error, statistic, p.value, s.value, mod),
            by = c("x","y","m","mod")
          ) %>%
          
          # format variables
          mutate(
            Variable = factor(
              
              sapply(1:nrow(.), function(i) unique( sets[grepl(y[i], sets$Y), "outcome"] ) ),
              levels = c("Cognition", "Affect", "cPA"),
              ordered = T
              
            ),
            sig. = if_else(p.value < .05, "*", ""),
            p.value = zerolead(p.value),
            m = if_else( is.na(mod), "", paste0(m," = ",mod) )
          ) %>%
          
          # final formatting touches
          select(-mod) %>%
          relocate(Variable, .before = 1) %>%
          relocate(s.value, .before = p.value) %>%
          rename(
            "Moderator" = "m",
            "Contrast" = "contrast",
            "Est." = "estimate",
            "SE" = "std.error",
            "Stat." = "statistic",
            "p value" = "p.value",
            "s value" = "s.value"
          )
      
    )
  )

  # print the result
  return(tabs)
  
}

# save resulting tables as .jpeg ----
save_tables <- function(tabs, type) {
  
  # prepare height and width values
  dims <- matrix(
    
    data = c(13.5, 11.1, 13, 13.5, 4.5, 2.5),
    ncol = 2,
    dimnames = list(x = names(tabs[[type]]), y = c("width","height") )
    
  )
  
  # do the saving
  for( x in names(tabs[[type]]) ) {
    
    # prepare the file
    jpeg(
      paste0("tab_",x,"_",type,".jpg"),
      units = "in",
      width = dims[x, "width"],
      height = dims[x, "height"],
      res = 300
    )
    
    # plot the table
    grid.table(
      
      tabs[[type]][[x]],
      theme = ttheme_default(),
      rows = NULL
      
    )
    
    # save it
    dev.off()
    
  }
  
}

