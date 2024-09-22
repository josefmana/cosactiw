# This script is supposed to pre-process data for the analysis for the brief report.
# The bulk of the script serves to:
#
# (i) extract relevant variables
# (ii) do propensity matching


rm( list = ls() ) # clean environment

theme_set( theme_bw(base_size = 12) ) # set theme for plotting
estimand <- "ATC" # set-up estimand (ATC, ATT, or ATE)

# load packages
library(here)
library(tidyverse)
library(openxlsx)
library(MatchIt)
library(cobalt)
library(patchwork)

theme_set( theme_bw(base_size = 12) ) # set theme for plotting
estimand <- "ATC" # set-up estimand (ATC, ATT, or ATE)

# set-up folders for results and data
sapply( c("figures","tables","_data"), function(i) if( !dir.exists(i) ) dir.create(i) )


# EXTRACT DATA ----

# read the data
d0 <- read.xlsx(here("_raw","COSACTIW_NANOK_pro-jamovi.xlsx"), sheet = "cosactiw+nanok") %>%
  
  # keep variables of interest
  select(
    1, Study, Age, `Education-2-cat`, Type_of_prevailing_occupation_during_life, # predictors
    `SA_New-BNT`, `Regular-PA`, Z_SA, MMSE, GDS15, GAI, FAQ, `Total-mental-activities`, Health # outcomes
  ) %>%
  
  # re-format
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
    `Total MA` = `Total-mental-activities`,
    `SF1-Health` = Health,
    across(
      .cols = c("FAQ","GDS15","GAI"),
      .fns = ~ log(.x+1),
      .names = "log{col}"
    )
  ) %>%
  
  # keep re-formatted variables
  select(ID, Study, Cosactiw, Age, Age_centr, Education, SA, PA, Z_SA, MMSE, GDS15, GAI, FAQ, `Total MA`, `SF1-Health`, logGDS15, logGAI, logFAQ, Profession)


# RQ1: ATC OF MIDLIFE ACTIVITY ----

# full matching via logistic regression for COSACTIW total & direct effect
fit0 <- lapply(
  
  set_names( x = c("Cosactiw ~ Age + Education","Cosactiw ~ Age + Education + SA"), nm = c("total","direct") ),
  function(X)
    
    matchit(
      formula = as.formula(X),
      data = d0,
      method = "full",
      distance = "glm",
      estimand = estimand
    )
  
)

# graphical check
fig0 <- lapply(
  
  set_names( names(fit0) ),
  function(i)
    
    lapply(
      
      set_names( strsplit(as.character(fit0[[i]]$formula)[[3]], split = " + ", fixed = T)[[1]] ),
      function(x) bal.plot(
        
        fit0[[i]],
        var.name = x,
        which = "both",
        colors = c("navyblue","orange"),
        sample.names = c("Non-weighted","Propensity score-weighted"),
        position = case_when(
          i == "total" & x == "Education" ~ "bottom",
          i == "direct" & x == "SA" ~ "bottom",
          .default = "none"
        )
      ) +
   
        theme( plot.title = element_text(hjust = .5, face = "bold") ) +
        labs( fill = "COSACTIW: " )
      
    )
)

# prepare a figures and save them
ggsave(plot = with(fig0$total, Age / Education ), filename = here("figures","midlifePA_teff_propscores_balplot.jpg"), dpi = 300, width = 8, height = 9)
ggsave(plot = with(fig0$direct, Age / Education / SA ), filename = here("figures","midlifePA_deff_propscores_balplot.jpg"), dpi = 300, width = 8, height = 10)

# save tables evaluating matching
lapply(
  
  names(fit0),
  function(i) {
    
    # variables information
    write.table(
      
      x = list(
        summary(fit0[[i]])$sum.all %>% as.data.frame() %>% rownames_to_column("var") %>% mutate(type = "original", .before = 1),
        summary(fit0[[i]])$sum.matched %>% as.data.frame() %>% rownames_to_column("var") %>% mutate(type = "matched", .before = 1)
      ) %>%
        do.call( rbind.data.frame, . ),
      
      file = here( "tables", paste0("midlifePA_", substr(i,1,1),"eff_propscores_summary.csv") ),
      sep = ",",
      row.names = F,
      quote = F
      
    )
    
    # estimated ESSs
    write.table(
      
      summary(fit0[[i]])$nn %>% as.data.frame() %>% rownames_to_column("type"),
      file = here( "tables", paste0("midlifePA_", substr(i,1,1),"eff_propscores_ESS.csv") ),
      sep = ",",
      row.names = F,
      quote = F
      
    )
    
  }
)


# RQ2: ATC OF SA WITHIN ----

# full matching via logistic regression for COSACTIW total & direct effect
fit1 <- lapply(
  
  set_names( x = c("SA ~ Age + Education","SA ~ Study + Age + Education"), nm = c("COSACTIW","BOTH") ),
  function(X) {
    
    # prepare the correct data set
    if( grepl("Study",X) ) data <- d0
    else data <- subset(d0, Cosactiw == 1)
    
    # compute & returrn it
    return(
      matchit(
        formula = as.formula(X),
        data = data,
        method = "full",
        distance = "glm",
        estimand = estimand
      )
    )
  }
)

# graphical check
fig1 <- lapply(
  
  set_names( names(fit1) ),
  function(i)
    
    lapply(
      
      set_names( strsplit(as.character(fit1[[i]]$formula)[[3]], split = " + ", fixed = T)[[1]] ),
      function(x) bal.plot(
        
        fit1[[i]],
        var.name = x,
        which = "both",
        colors = c("lightblue","violetred"),
        sample.names = c("Non-weighted","Propensity score-weighted"),
        position = if_else(x == "Education", "bottom", "none")
      ) +
        
        theme( plot.title = element_text(hjust = .5, face = "bold") ) +
        labs( fill = "SA: " )
      
    )
)

# prepare a figures and save them
ggsave(plot = with(fig1$COSACTIW, Age / Education ), filename = here("figures","SA_teff_cosactiw_propscores_balplot.jpg"), dpi = 300, width = 8, height = 9)
ggsave(plot = with(fig1$BOTH, Age / Study / Education ), filename = here("figures","SA_teff_both_propscores_balplot.jpg"), dpi = 300, width = 8, height = 10)

# save tables evaluating matching
lapply(
  
  names(fit1),
  function(i) {
    
    # variables information
    write.table(
      
      x = list(
        summary(fit1[[i]])$sum.all %>% as.data.frame() %>% rownames_to_column("var") %>% mutate(type = "original", .before = 1),
        summary(fit1[[i]])$sum.matched %>% as.data.frame() %>% rownames_to_column("var") %>% mutate(type = "matched", .before = 1)
      ) %>%
        do.call( rbind.data.frame, . ),
      
      file = here( "tables", paste0("SA_teff_", tolower(i), "_propscores_summary.csv") ),
      sep = ",",
      row.names = F,
      quote = F
      
    )
    
    # estimated ESSs
    write.table(
      
      summary(fit1[[i]])$nn %>% as.data.frame() %>% rownames_to_column("type"),
      file = here( "tables", paste0("SA_teff_", tolower(i), "_propscores_ESS.csv") ),
      sep = ",",
      row.names = F,
      quote = F
      
    )
    
  }
)


# EXTRACT & SAVE DATA ----

saveRDS(
  
  object = list(
    PA_teff = match.data(fit0$total),
    PA_deff = match.data(fit0$direct),
    SA_both = match.data(fit1$BOTH),
    SA_cosa = match.data(fit1$COSACTIW)
  ),
  
  file = here("_data.rds")
  
)
