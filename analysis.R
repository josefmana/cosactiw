# This script is supposed to give such a data summaries that it can help with answering following questions:
#
# RQ1) Is there a bigger probability that a woman is SA given she was physically active during middle age?
# RQ2) How does the probability of SA differ between education levels conditional on physical activity?
# RQ3) What's more important for being SA, education or midlife physical activity?
# RQ4) Is there a bigger probability of being physically active in old age if one was active in middle age?

rm( list = ls() ) # clean environment

# load packages
library(here)
library(tidyverse)
library(openxlsx)
library(MatchIt)
library(marginaleffects)
library(emmeans)
library(performance)
library(patchwork)


# IN-HOUSE FUNCTION ----

# propensity scores with g-computation for the "marginaleffects solution"
marginal_effects <- function(
    
  data = d,
  estimand = "ATE",
  y = "SA",
  x = "Cosactiw",
  control = 0,
  exposure = 1,
  save = F,
  likelihood = "quasibinomial",
  trans = function(x) exp(x)
  
    ) {
  
  ## propensity scores matching ----

  # model it
  m.out <- matchit(
    formula = as.formula( paste0(x," ~ Age + Education") ),
    data = data,
    method = "full",
    distance = "glm",
    estimand = estimand
  )
  
  # check-it
  plot(m.out, type = "density", interactive = F)
  
  dia <- summary(m.out)
  print("Matching diagnostics")
  print(dia)
  
  # prepare matched data
  m.data <- match.data(m.out)
  
  
  ## regression analysis ----
  fit <- glm(
    formula = formula( paste0(y," ~ 1 + ",x," * (Age + Education)") ),
    data = m.data,
    weights = weights,
    family = likelihood
  )
  
  ## g-computation ----
  
  # prepare dat for calculation of the effect
  if (estimand == "ATE") d_seq <- m.data
  else if (estimand == "ATT") d_seq <- subset(m.data, get(x) == exposure)
  else if (estimand == "ATC") d_seq <- subset(m.data, get(x) == control)
  
  # prepare comparison and transform functions
  comp <- case_when(likelihood == "quasibinomial" ~ "lnratioavg", likelihood == "gaussian" ~ "difference")
  
  # average effect
  avg_comp <- avg_comparisons(
    model = fit,
    newdata = d_seq,
    variables = x,
    vcov = ~subclass,
    wts = "weights",
    comparison = comp,
    transform = trans
  )
  
  # conditional average 
  cond_comp <- avg_comparisons(
    model = fit,
    newdata = d_seq,
    variables = x,
    by = "Education",
    vcov = ~subclass,
    wts = "weights",
    comparison = comp,
    transform = trans
  )
  
  # interaction with education
  inter <- avg_comparisons(
    model = fit,
    newdata = d_seq,
    variables = x,
    by = "Education",
    hypothesis = "revpairwise",
    vcov = ~subclass,
    wts = "weights",
    comparison = comp,
    transform = trans
  )
  
  # plot interaction with education
  plt_edu <- plot_predictions(
    model = fit,
    condition = c("Education", x),
    points = 1,
    newdata = d_seq,
    vcov = ~subclass
  )
  
  # plot interaction with age
  plt_age <- plot_predictions(
    model = fit,
    condition = c("Age", x),
    points = 1,
    newdata = d_seq,
    vcov = ~subclass
  )
  
  # print results
  Sys.sleep(3)
  print( paste0("Average Effect (",estimand,")") )
  print(avg_comp)
  print("Average Effect per Education Level")
  print(cond_comp)
  print("Interaction with Education")
  print(inter)
  print( (plt_edu / plt_age) + plot_layout(guides = "collect") )
  
  # save if called for
  if ( isTRUE(save) ) return(
    list(
      x_model = m.out, # model for exposure
      y_model = fit, # model for outcome
      prop_dia = dia, # diagnosis of propensity scores matching (== summary(x_model) )
      data = m.data, # data including weights
      estimand = estimand, # type of effect
      average_comp = avg_comp, # ATT/ATC/ATE
      stratified_comp = cond_comp, # effect stratified by education
      interaction = inter # exposure * education interaction
    )
  )
  
}

# simple regressions and estimated marginal means for the "emmeans solution"
marginal_means <- function(data = d, y = "SA", x = "Cosactiw", save = F, likelihood = "binomial", age = F) {

  # fit the model
  fit <- glm(
    formula = formula( paste0(y," ~ 1 + ",x," * (Age + Education)") ),
    data = data,
    family = likelihood
  )

  # extract the emmeans
  emm0 <- emmeans(object = fit, specs = formula( paste0("revpairwise ~ ",x) ), type = "response") # averaged over
  emm1 <- emmeans(object = fit, specs = formula( paste0("revpairwise ~ ",x) ), type = "response", by = "Education") # stratified by education levels
  inter <- contrast( emmeans(object = fit, specs = formula( paste0("revpairwise ~ ",x," * Education") ), type = "response"), interaction = "consec")
  ageint <- emtrends(fit, formula( paste0("revpairwise ~ ",x) ), type = "response", var = "Age")
  
  # print it
  print("Non-stratified estimated marginal means")
  print(emm0)
  print("Estimated marginal means stratified by education level")
  print(emm1)
  print( paste0(x," * education interaction") )
  print(inter)
  if (age == T) {
    print( paste0(x," * age interaction") )
    print(ageint)
  }
  
  # save if called for
  if ( isTRUE(save) ) return(
    list(
      model = fit, # model for outcome
      average_emmean = emm0, # non-stratified emmean
      stratified_emmean = cond_comp, # stratified emmean
      interaction = inter # exposure * education interaction
    )
  ) 
    
}


# PREPARE DATA ----

d <-
  read.xlsx(here("_raw","COSACTIW_NANOK_for-Eef.xlsx"), sheet = "data") %>%
  select(
    1, Study, Age, `Education-2-cat`, `Regular-PA`, Type_of_prevailing_occupation_during_life, # predictors
    `SA_New-BNT`, GDS15, GAI, FAQ # outcomes
  ) %>%
  mutate(
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
    )
  ) %>%
  select(ID, Study, Cosactiw, Age, Education, SA, PA, Profession, GDS15, GAI, FAQ)


# MARGINAL EFFECTS SOLUTION ----

# via the MatchIt and marginaleffects packages
marginal_effects(estimand = "ATC", y = "SA", x = "Cosactiw", save = F) # effect of midlife PA on SA
marginal_effects(estimand = "ATC", y = "FAQ", x = "Cosactiw", save = F, likelihood = "gaussian")
marginal_effects(estimand = "ATC", y = "GAI", x = "Cosactiw", save = F, likelihood = "gaussian")
marginal_effects(estimand = "ATC", y = "GDS15", x = "Cosactiw", save = F, likelihood = "gaussian")

#marginal_effects(estimand = "ATC", y = "SA", x = "PA", save = F) # effect of actual PA on SA
#marginal_effects(estimand = "ATC", y = "PA", x = "Cosactiw", save = F) # effect of midlife PA on actual PA


# MARGINAL MEANS SOLUTION ----

# via the emmeans package
marginal_means(y = "SA", x = "Cosactiw", save = F) # emmeans of P(SA | midlife PA)
#marginal_means(y = "SA", x = "PA", save = F) # emmeans of P(SA | actual PA)
#marginal_means(y = "PA", x = "Cosactiw", save = F) # emmeans of P(actual PA | midlife PA)

marginal_means(y = "FAQ", x = "Cosactiw", save = F, likelihood = "gaussian", age = T) # emmeans of E(FAQ | midlife PA)
marginal_means(y = "GDS15", x = "Cosactiw", save = F, likelihood = "gaussian", age = T) # emmeans of E(GDS15 | midlife PA)
marginal_means(y = "GAI", x = "Cosactiw", save = F, likelihood = "gaussian", age = T) # emmeans of E(GAI | midlife PA)


