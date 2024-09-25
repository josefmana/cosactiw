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
tidyit <- function(x, y) reduce(x, full_join) %>% mutate(y = y, .before = 1)


# RQ1) CAUSAL EFFECT OF MIDLIFE-PA ON SELECTED OUTCOMES ----

# prepare a data frame with model specifications
specs1 <- data.frame(
  
  lab = c("SA","WHO-PA","Cognition CS", "MMSE", "FAQ", "GDS-15", "GAI", "log(FAQ)", "log(GDS-15)", "log(GAI)"),
  y = c("SA", "PA", "Z_SA", "MMSE", "FAQ", "GDS15", "GAI", "logFAQ", "logGDS15", "logGAI"),
  X1 = c( "Study * Education", rep("Study * (Education + Age_centr)", 9) ), # predictors for a total effect model
  X2 = c( NA, rep("Study * SA * (Education + Age_centr)", 9) ), # predictors for a direct effect model & moderation
  likelihood = c( rep("quasibinomial", 2), rep("gaussian", 8) ),
  dataset1 = c( "PA2SA_teff", rep("PA2Y_teff", 9) ),
  dataset2 = c( NA, rep("PA2Y_deff", 9) )
  
)


## ---- RQ1.1) TOTAL EFFECT OF MIDLIFE PHYSICAL ACTIVITY ----

# fit a set of weighted regressions, one for each outcome of interest
fit1.1 <- with(
  
  specs1, lapply(
      
      set_names(x = y, nm = lab),
      function(i) glm(

        formula = as.formula( paste0(i," ~ ",X1[y == i]) ),
        data = d[[dataset1[y == i]]],
        weights = weights,
        family = likelihood[y == i]

      )
  )
)

# extract estimates of the total effect of midlife-PA on different outcomes
list(
  
  # SA & current PA
  lapply(
    
    names(fit1.1)[1:2],
    function(y) list(
      
      preds(fit1.1[[y]], "Study", cont = F), # proportion of SAs
      comps(fit1.1[[y]], subset(d[[specs1$dataset1[specs1$lab == y]]], Study == "NANOK"), list(Study = "revpairwise"), F, "lnratioavg", "exp") # ATC
      
    ) %>% tidyit(y = y)
    
  ) %>% do.call( rbind.data.frame, . ),
  
  # the rest
  lapply(
    
    names(fit1.1)[-1:-2],
    function(y) list(

      preds(fit1.1[[y]], "Study", cont = T), # mean score
      comps(fit1.1[[y]], subset(d[[specs1$dataset1[specs1$lab == y]]], Study == "NANOK"), list(Study = "revpairwise"), F, "differenceavg", NULL) # ATC

    ) %>%  tidyit(y = y)
    
  ) %>% do.call( rbind.data.frame, . )
  
) %>%
  
  reduce( ., full_join ) %>%
  select(y, Study, term, contrast, estimate, conf.low, conf.high, p.value, s.value) %>%
  write.table(file = here("tables","gcomputation_midlifePA_teffs.csv"), sep =",", row.names = F, quote = F)


## ---- RQ1.2) DIRECT EFFECT OF MIDLIFE PHYSICAL ACTIVITY ----

# fit a set of weighted regressions, one for each outcome of interest
fit1.2 <- with(
  
  specs1, lapply(
    
    set_names(x = y[!is.na(X2)], nm = lab[!is.na(X2)]), # SA is a predictor in these analyses, not outcome
    function(i) glm(
      
      formula = as.formula( paste0(i," ~ ",X2[y == i]) ),
      data = d[[dataset2[y == i]]],
      weights = weights,
      family = likelihood[y == i]
      
    )
  )
)

# direct effect of midlife-PA on different outcomes together with SA moderation
list(
  
  # current PA
  list(
    
    preds( fit1.2$`WHO-PA`, c("Study", "SA"), cont = F ), # proportion of SAs given education level
    comps( fit1.2$`WHO-PA`, subset(d$PA2Y_deff, Study == "NANOK"), list(Study = "revpairwise"), "SA", "lnratioavg", "exp"), # CATC
    comps( fit1.2$`WHO-PA`, subset(d$PA2Y_deff, Study == "NANOK"), list(Study = "revpairwise"), "SA", "lnratioavg", "exp", int = T) # Study/SA interaction
    
  ) %>% tidyit(y = "WHO-PA"),
  
  # the rest
  lapply(
    
    names(fit1.2)[-1],
    function(y) list(
      
      preds( fit1.2[[y]], c("Study", "SA"), cont = T ), # mean score given education level
      comps( fit1.2[[y]], subset(d$PA2Y_deff, Study == "NANOK"), list(Study = "revpairwise"), "SA", "differenceavg", NULL), # CATC
      comps( fit1.2[[y]], subset(d$PA2Y_deff, Study == "NANOK"), list(Study = "revpairwise"), "SA", "differenceavg", NULL, int = T) # Study/SA interaction
      
    ) %>%  tidyit(y = y)
    
  ) %>% do.call( rbind.data.frame, . )
  
) %>%
  
  reduce( ., full_join ) %>%
  select(y, Study, term, contrast, estimate, conf.low, conf.high, p.value, s.value) %>%
  write.table(file = here("tables","gcomputation_midlifePA_deffs.csv"), sep =",", row.names = F, quote = F)


# RQ2) CAUSAL EFFECT OF SA ON SELECTED OUTCOMES ----

# prepare a data frame with model specifications
specs2 <- data.frame(
  
  lab = c("Cognition CS", "MMSE", "FAQ", "GDS-15", "GAI", "log(FAQ)", "log(GDS-15)", "log(GAI)","SF1-Health","WHO-PA","Total MA"),
  y = c("Z_SA", "MMSE", "FAQ", "GDS15", "GAI", "logFAQ", "logGDS15", "logGAI","Health","PA","Total_MA"),
  X = c(rep("SA * (Study + Education + Age_centr)", 8), "SA * (Education + Age_centr)", "SA * (Study + Education + Age_centr)", "SA * (Education + Age_centr)"),
  likelihood = c(rep("gaussian", 9),  "quasibinomial", "gaussian"),
  dataset = c(rep("SA2Y_both", 8), "SA2Y_cosa", "SA2Y_both", "SA2Y_cosa")
  
)

# fit a set of weighted regressions, one for each outcome of interest
fit2 <- with(
  
  specs2, lapply(
    
    set_names(x = y, nm = lab),
    function(i) glm(
      
      formula = as.formula( paste0(i," ~ ",X[y == i]) ),
      data = d[[dataset[y == i]]],
      weights = weights,
      family = likelihood[y == i]
      
    )
  )
)

