# This script is supposed to give such a data summaries that it can help with answering following questions:
#
# RQ1) Is there a bigger probability that a woman is SA given she was physically active during middle age?
# RQ2) How does the probability of SA differ between education levels conditional on physical activity?
#

rm( list = ls() ) # clean environment

# load packages
library(here)
library(tidyverse)
library(openxlsx)
library(MatchIt)
library(cobalt)
library(patchwork)
library(marginaleffects)
library(performance)

theme_set( theme_bw(base_size = 12) ) # set theme for plotting
estimand <- "ATC" # set-up estimand (ATC, ATT, or ATE)

# set-up folder for results
sapply( c("figures","tables"), function(i) if( !dir.exists(i) ) dir.create(i) )


# UTILS ----

rprint <- function(x, d=2) sprintf( paste0("%.",d,"f"), round(x, d) )
zerolead <- function(x, d = 3) ifelse( x < .001, "< .001", sub("0","", rprint(x, d) ) )


# PREPARE DATA ----
d0 <-

  read.xlsx(here("_raw","COSACTIW_NANOK_for-Eef.xlsx"), sheet = "data") %>%
  select(
    1, Study, Age, `Education-2-cat`, `Regular-PA`, Type_of_prevailing_occupation_during_life, # predictors
    `SA_New-BNT`, GDS15, GAI, FAQ # outcomes
  ) %>%
  mutate(
    Age_centr = as.numeric(
      scale(Age, center = T, scale = F)
    ),
    Study = factor(
      x = Study,
      levels = c("COSACTIW","NANOK")
    ),
    Cosactiw = factor(
      if_else(
        condition = Study == "COSACTIW",
        true = 1,
        false = 0
      )
    ),
    Education = factor(
      x = `Education-2-cat`,
      levels = 1:2,
      labels = c("lower","higher"),
      ordered = T
    ),
    SA = factor(
      case_when(
        `SA_New-BNT` == 1 ~ 1,
        `SA_New-BNT` == 2 ~ 0
      )
    ),
    PA = factor(
      if_else(
        condition = `Regular-PA` == 1,
        true = 1,
        false = 0
      )
    ),
    Profession = factor(
      case_when(
        Type_of_prevailing_occupation_during_life == 1 ~ "manual",
        Type_of_prevailing_occupation_during_life == 2 ~ "mostly manual",
        Type_of_prevailing_occupation_during_life == 3 ~ "mostly mental",
        Type_of_prevailing_occupation_during_life == 4 ~ "mental"
      ),
    ),
    across(
      .cols = c("FAQ","GDS15","GAI"),
      .fns = ~ log(.x+1),
      .names = "log{col}"
    )
  ) %>%
  select(ID, Study, Cosactiw, Age, Age_centr, Education, SA, PA, Profession, GDS15, GAI, FAQ, logGDS15, logGAI, logFAQ)


# PROPENSITY SCORES MATCHING ----

# fit a logistic regression for Cosactiw variable with full matching via logistic regression
fit0 <- matchit(
  formula = Cosactiw ~ Age + Education,
  data = d0,
  method = "full",
  distance = "glm",
  estimand = estimand
)

# graphical check
fig0 <- lapply(
  
  set_names( x = c("Age","Education") ),
  function(x) bal.plot(
    
    fit0,
    var.name = x,
    which = "both",
    colors = c("navyblue","orange"),
    position = if_else(x == "Age", "none", "bottom"),
    sample.names = c("Non-weighted","Propensity score-weighted")
    
  ) +
    
    theme( plot.title = element_text(hjust = .5, face = "bold") ) +
    labs( fill = "COSACTIW: " )
  
)

# prepare a figure and save it
(fig0$Age / fig0$Education)
ggsave(plot = last_plot(), filename = here("figures","propensity_scores_check.jpg"), dpi = 300, width = 9, height = 9)

# extract propensity score-weighted data set
d1 <- match.data(fit0)


# STATISTICAL MODELLING ----

# fit a set of weighted regressions, one for each outcome of interest
fit1 <- lapply(
  
  set_names( c("SA","GDS15","GAI","FAQ","logGDS15","logGAI","logFAQ") ),
  function(y) glm(
    
    formula = as.formula( paste0(y," ~ Study * (Age_centr + Education)") ),
    data = d1,
    weights = weights,
    family = if_else(y == "SA", "quasibinomial", "gaussian")
    
  )
)

# save model check plots
lapply(
  
  names(fit1),
  function(y) ggsave(
    
    plot = plot( check_model(fit1[[y]]) ),
    filename = here( "figures", paste0("model_checks_",y,".jpg") ),
    dpi = 300,
    width = 10,
    height = 12
    
  )
)


# EXTRACT RESULTS ----

list(
  
  # SA
  list(
    
    # proportion of SAs
    avg_predictions(
      fit1$SA,
      variables = "Study",
      wts = "weights"
    ) %>%
      as.data.frame() %>%
      mutate(
        across( starts_with("conf"), ~ rprint(100 * .x) ),
        across( where(is.factor), as.character ),
        estimate = paste0( rprint(100 * estimate),"%" ),
        p.value = zerolead(p.value),
        s.value = rprint(s.value)
      ),
    
    # ATC
    avg_comparisons(
      model = fit1$SA,
      newdata = subset(d1, Study == "NANOK"),
      variables = list(Study = "revpairwise"),
      vcov = ~subclass,
      wts = "weights",
      comparison = "lnratioavg",
      transform = "exp"
    ) %>%
      as.data.frame() %>%
      select( -contains("predicted") ) %>%
      mutate(
        across(starts_with("conf"), rprint),
        across(all_of( c("estimate","s.value") ), rprint),
        across( where(is.factor), as.character ),
        p.value = zerolead(p.value),
      ),
    
    # proportion of SAs given education level
    avg_predictions(
      fit1$SA,
      variables = c("Study", "Education"),
      wts = "weights"
    ) %>%
      as.data.frame() %>%
      mutate(
        across( starts_with("conf"), ~ rprint(100 * .x) ),
        across( where(is.factor), as.character ),
        estimate = paste0( rprint(100 * estimate),"%" ),
        p.value = zerolead(p.value),
        s.value = rprint(s.value)
      ),
    
    # CATC
    avg_comparisons(
      model = fit1$SA,
      newdata = subset(d1, Study == "NANOK"),
      variables = list(Study = "revpairwise"),
      by = "Education",
      vcov = ~subclass,
      wts = "weights",
      comparison = "lnratioavg",
      transform = "exp"
    ) %>%
      as.data.frame() %>%
      select( -contains("predicted") ) %>%
      mutate(
        across(starts_with("conf"), rprint),
        across(all_of( c("estimate","s.value") ), rprint),
        across( where(is.factor), as.character ),
        p.value = zerolead(p.value),
      ),
    
    # interaction with education
    avg_comparisons(
      model = fit1$SA,
      newdata = subset(d1, Study == "NANOK"),
      variables = "Study",
      by = "Education",
      vcov = ~subclass,
      wts = "weights",
      hypothesis = "revpairwise",
      comparison = "lnratioavg",
      transform = "exp"
    ) %>%
      as.data.frame() %>%
      rename("contrast" = "term") %>%
      mutate(
        across(starts_with("conf"), rprint),
        across(all_of( c("estimate","s.value") ), rprint),
        across( where(is.factor), as.character ),
        p.value = zerolead(p.value),
      )
    
  ) %>%
    
    reduce(full_join) %>%
    mutate( confint = paste0("[",conf.low,", ",conf.high,"]") ) %>%
    select(Study, Education, contrast, estimate, confint, p.value, s.value) %>%
    mutate(y = "SA", .before = 1),
  
  # the rest
  lapply(
    
    names(fit1)[-1],
    function(y)
      
      list(
        
        # proportion of SAs
        avg_predictions(
          fit1[[y]],
          variables = "Study",
          wts = "weights"
        ) %>%
          as.data.frame() %>%
          mutate(
            across(starts_with("conf"), rprint),
            across(where(is.factor), as.character),
            estimate = rprint(estimate),
            p.value = zerolead(p.value),
            s.value = rprint(s.value)
          ),
        
        # ATC
        avg_comparisons(
          model = fit1[[y]],
          newdata = subset(d1, Study == "NANOK"),
          variables = list(Study = "revpairwise"),
          vcov = ~subclass,
          wts = "weights"
        ) %>%
          as.data.frame() %>%
          select( -contains("predicted") ) %>%
          mutate(
            across(starts_with("conf"), rprint),
            across(all_of( c("estimate","s.value") ), rprint),
            across( where(is.factor), as.character ),
            p.value = zerolead(p.value),
          ),
        
        # proportion of SAs given education level
        avg_predictions(
          fit1[[y]],
          variables = c("Study", "Education"),
          wts = "weights"
        ) %>%
          as.data.frame() %>%
          mutate(
            across(starts_with("conf"), rprint),
            across(where(is.factor), as.character),
            estimate = rprint(estimate),
            p.value = zerolead(p.value),
            s.value = rprint(s.value)
          ),
        
        # CATC
        avg_comparisons(
          model = fit1[[y]],
          newdata = subset(d1, Study == "NANOK"),
          variables = list(Study = "revpairwise"),
          by = "Education",
          vcov = ~subclass,
          wts = "weights"
        ) %>%
          as.data.frame() %>%
          select( -contains("predicted") ) %>%
          mutate(
            across(starts_with("conf"), rprint),
            across(all_of( c("estimate","s.value") ), rprint),
            across( where(is.factor), as.character ),
            p.value = zerolead(p.value),
          ),
        
        # interaction with education
        avg_comparisons(
          model = fit1[[y]],
          newdata = subset(d1, Study == "NANOK"),
          variables = list(Study = "revpairwise"),
          by = "Education",
          hypothesis = "revpairwise",
          vcov = ~subclass,
          wts = "weights"
        ) %>%
          as.data.frame() %>%
          rename("contrast" = "term") %>%
          mutate(
            across(starts_with("conf"), rprint),
            across(all_of( c("estimate","s.value") ), rprint),
            across( where(is.factor), as.character ),
            p.value = zerolead(p.value),
          )
        
      ) %>%
      
      reduce(full_join) %>%
      mutate( confint = paste0("[",conf.low,", ",conf.high,"]") ) %>%
      select(Study, Education, contrast, estimate, confint, p.value, s.value) %>%
      mutate(y = y, .before = 1)
    
  ) %>%
    
    do.call( rbind.data.frame, . )
  
) %>%
  
  do.call( rbind.data.frame, . ) %>%
  mutate( across( ends_with("value"), ~ if_else( is.na(contrast), NA, .x) ) )


