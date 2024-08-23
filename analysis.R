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

# compute predictions
preds <- function(mod, vars) avg_predictions(
  mod,
  variables = vars,
  wts = "weights"
) %>%
  as.data.frame() %>%
  mutate(
    across( starts_with("conf"), ~ rprint(100 * .x) ),
    across(where(is.factor), as.character),
    across(ends_with("value"), ~ NA),
    estimate = paste0( rprint(100 * estimate),"%" )
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


# PREPARE DATA ----
d0 <-

  read.xlsx(here("_raw","COSACTIW_NANOK_pro-jamovi.xlsx"), sheet = "cosactiw+nanok") %>%
  select(
    1, Study, Age, `Education-2-cat`, Type_of_prevailing_occupation_during_life, # predictors
    `SA_New-BNT`, `Regular-PA`, Z_SA, MMSE, GDS15, GAI, FAQ # outcomes
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
        `SA_New-BNT` == 0 ~ 0
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
  select(ID, Study, Cosactiw, Age, Age_centr, Education, SA, PA, Z_SA, MMSE, GDS15, GAI, FAQ, logGDS15, logGAI, logFAQ, Profession)


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

# save table evaluating matching
write.table(
  
  x = list(
    summary(fit0)$sum.all %>% as.data.frame() %>% rownames_to_column("var") %>% mutate(type = "original", .before = 1),
    summary(fit0)$sum.matched %>% as.data.frame() %>% rownames_to_column("var") %>% mutate(type = "matched", .before = 1)
  ) %>%
    do.call( rbind.data.frame, . ),
  
  file = here("tables","propensity_scores_summary.csv"),
  sep = ",",
  row.names = F,
  quote = F
  
)

# save table estimating ESSs
write.table(
  
  summary(fit0)$nn %>% as.data.frame() %>% rownames_to_column("type"),
  file = here("tables","propensity_scores_sample_sizes.csv"),
  sep = ",",
  row.names = F,
  quote = F
  
)

# extract propensity score-weighted data set
d1 <- match.data(fit0)

# save it
write.table(
  x = d1,
  file = here("_dataset.scv"),
  sep = ",",
  row.names = F,
  quote = F
)


# STATISTICAL MODELLING ----

# fit a set of weighted regressions, one for each outcome of interest
fit1 <- lapply(
  
  set_names( c("SA","PA","Z_SA","MMSE","GDS15","GAI","FAQ","logGDS15","logGAI","logFAQ") ),
  function(y) glm(
    
    formula = as.formula( paste0(y," ~ Study * (Age_centr + Education)") ),
    data = d1,
    weights = weights,
    family = if_else(y %in% c("SA","PA"), "quasibinomial", "gaussian")
    
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
  
  # SA & PA
  lapply(
    
    names(fit1)[1:2],
    function(y)
      
      list(
        
        preds( fit1[[y]], "Study" ), # proportion of SAs
        comps( fit1[[y]], subset(d1, Study == "NANOK"), list(Study = "revpairwise"), F, "lnratioavg", "exp"), # ATC
        preds( fit1[[y]], c("Study", "Education") ), # proportion of SAs given education level
        comps( fit1[[y]], subset(d1, Study == "NANOK"), list(Study = "revpairwise"), "Education", "lnratioavg", "exp"), # CATC
        comps( fit1[[y]], subset(d1, Study == "NANOK"), list(Study = "revpairwise"), "Education", "lnratioavg", "exp", int = T) # Study/Education interaction
        
      ) %>% tidyit(y = y)
    
  ) %>% do.call( rbind.data.frame, . ),
  
  # the rest
  lapply(
    
    names(fit1)[-1:-2],
    function(y)
      
      list(
        
        preds( fit1[[y]], "Study" ), # mean score
        comps( fit1[[y]], subset(d1, Study == "NANOK"), list(Study = "revpairwise"), F, "differenceavg", NULL), # ATC
        preds( fit1[[y]], c("Study", "Education") ), # mean score given education level
        comps( fit1[[y]], subset(d1, Study == "NANOK"), list(Study = "revpairwise"), "Education", "differenceavg", NULL), # CATC
        comps( fit1[[y]], subset(d1, Study == "NANOK"), list(Study = "revpairwise"), "Education", "differenceavg", NULL, int = T) # Study/Education interaction

      ) %>%  tidyit(y = y)
    
  ) %>% do.call( rbind.data.frame, . )
  
) %>%
  
  do.call( rbind.data.frame, . ) %>%
  write.table(file = here("tables","gcomputation_results.csv"), sep =";", row.names = F, quote = F)


# VISUALISE RESULTS ----

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


