# This script is supposed to give such a data summaries that it can help with answering following questions:
#
# RQ1.1) How strong is the total causal effect of midlife PA on SA and other psychological variables?
# RQ1.2) How strong is the direct causal effect of midlife PA on SA and other psychological variables?
# RQ2) How strong is the total causal effect of SA on psychological variables?
#

rm( list = ls() ) # clean environment

# load packages
library(here)
library(tidyverse)
#library(patchwork)
#library(performance)
library(marginaleffects)

theme_set( theme_bw(base_size = 12) ) # set theme for plotting
estimand <- "ATC" # set-up estimand (ATC, ATT, or ATE)
d <- readRDS( here("_data.rds") ) # read data

# set-up folder for results
sapply( c("figures","tables"), function(i) if( !dir.exists(i) ) dir.create(i) )


# UTILS ----

rprint <- function(x, d=2) sprintf( paste0("%.",d,"f"), round(x, d) )
zerolead <- function(x, d = 3) ifelse( x < .001, "< .001", sub("0","", rprint(x, d) ) )

# compute predictions
preds <- function(mod, vars, cont = F) avg_predictions(
  mod,
  variables = vars,
  wts = "weights"
) %>%
  as.data.frame() %>%
  mutate(
    across( starts_with("conf"), ~ rprint( ifelse(cont == F ,100, 1) * .x) ),
    across(where(is.factor), as.character),
    across(ends_with("value"), ~ NA),
    estimate = paste0( rprint( ifelse(cont == F, 100, 1) * estimate), ifelse(cont == F, "%", "") )
  )

# compute comparisons
comps <- function(mod, nd, y, m, comp, trans = NULL, int = F) avg_comparisons(
  model = mod,
  newdata = nd,
  variables = y,
  by = m,
  vcov = ~subclass,
  wts = "weights",
  comparison = comp,
  transform = unlist( ifelse( is.null(trans), list(NULL), list(trans) ) ),
  hypothesis = unlist( ifelse( int == T, list("revpairwise"), list(NULL) ) )
) %>%
  as.data.frame() %>%
  mutate(
    across(starts_with("conf"), rprint),
    across(all_of( c("estimate","s.value") ), rprint),
    across( where(is.factor), as.character ),
    p.value = zerolead(p.value),
  )

# pull results from a list to single neat data frame
tidyit <- function(x, y) reduce(x, full_join) %>%
  mutate( confint = paste0("[",conf.low,", ",conf.high,"]") ) %>%
  select(Study, Education, term, contrast, estimate, confint, p.value, s.value) %>%
  mutate(y = y, .before = 1)


# RQ1.1) TOTAL EFFECT OF MIDLIFE PHYSICAL ACTIVITY ----

# prepare a data frame with model specifications
specs1 <- data.frame(
  
  lab = c("SA","WHO-PA","Cognition CS", "MMSE", "FAQ", "GDS-15", "GAI", "log(FAQ)", "log(GDS-15)", "log(GAI)"),
  y = c("SA", "PA", "Z_SA", "MMSE", "FAQ", "GDS15", "GAI", "logFAQ", "logGDS15", "logGAI"),
  X1 = c( "Study * Education", rep("Study * (Education + Age_centr)", 9) ), # predictors for a total effect model
  X2 = c( NA, rep("Study * (SA + Education + Age_centr)", 9) ), # predictors for a direct effect model
  likelihood = c( rep("quasibinomial", 2), rep("gaussian", 8) ),
  dataset = c( "PA2SA_teff", rep("PA2Y_teff", 9) )
  
)


## ---- STATISTICAL MODELLING ----

# fit a set of weighted regressions, one for each outcome of interest
fit1.1 <- with(
  
  specs1, lapply(
      
      set_names(x = y, nm = lab),
      function(i) glm(

        formula = as.formula( paste0(i," ~ ",X1[y == i]) ),
        data = d[[dataset[y == i]]],
        weights = weights,
        family = likelihood[y == i]

      )
  )
)


## ---- EXTRACT RESULTS ----
list(
  
  # SA & PA
  lapply(
    
    names(fit1.1)[1:2],
    function(y)
      
      list(
        
        preds( fit1[[y]], "Study", cont = F ), # proportion of SAs
        comps( fit1[[y]], subset(d1, Study == "NANOK"), list(Study = "revpairwise"), F, "lnratioavg", "exp"), # ATC
        #preds( fit1[[y]], c("Study", "Education"), cont = F ), # proportion of SAs given education level
        #comps( fit1[[y]], subset(d1, Study == "NANOK"), list(Study = "revpairwise"), "Education", "lnratioavg", "exp"), # CATC
        #comps( fit1[[y]], subset(d1, Study == "NANOK"), list(Study = "revpairwise"), "Education", "lnratioavg", "exp", int = T) # Study/Education interaction
        
      ) %>% tidyit(y = y)
    
  ) %>% do.call( rbind.data.frame, . ),
  
  # the rest
  lapply(
    
    names(fit1)[-1:-2],
    function(y)
      
      list(
        
        preds( fit1[[y]], "Study", cont = T ), # mean score
        comps( fit1[[y]], subset(d1, Study == "NANOK"), list(Study = "revpairwise"), F, "differenceavg", NULL), # ATC
        preds( fit1[[y]], c("Study", "Education"), cont = T ), # mean score given education level
        comps( fit1[[y]], subset(d1, Study == "NANOK"), list(Study = "revpairwise"), "Education", "differenceavg", NULL), # CATC
        comps( fit1[[y]], subset(d1, Study == "NANOK"), list(Study = "revpairwise"), "Education", "differenceavg", NULL, int = T) # Study/Education interaction

      ) %>%  tidyit(y = y)
    
  ) %>% do.call( rbind.data.frame, . )
  
) %>%
  
  do.call( rbind.data.frame, . ) %>%
  write.table(file = here("tables","gcomputation_results.csv"), sep =";", row.names = F, quote = F)


## ---- VISUALISE RESULTS ----

lapply(
  
  names(fit1),
  function(y) {
    
    # plot it
    fig1 <- lapply(
      
      c("Education","Age_centr"),
      function(m) plot_predictions(
        
        model = fit1[[y]],
        condition = c(m, "Study"),
        points = .8
        
      ) +
        scale_colour_manual( values = c("navyblue","orange") ) +
        scale_fill_manual( values = c("navyblue","orange") ) +
        theme(
          legend.position = if_else(m == "Education", "none", "bottom"),
          panel.grid.minor = element_blank()
        )
    )
    
    # prepare a figure and save it
    ( fig1[[1]] / fig1[[2]] )
    ggsave( here( "figures", paste0("interactions_",y,".jpg") ), dpi = 300, width = 7, height = 9)
    
  }
)


# TABLE 2 (RE-)ANALYSIS ----
