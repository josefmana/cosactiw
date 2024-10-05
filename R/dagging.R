# This is a script used to write functions to represent causal assumptions
# for deducing  correct adjustment sets conditional on these assumptions
# for a subsequent statistical analysis


# prepare & save the DAG ----
make_dag <- function(plot = T) {
  
  # set-up a data frame with node labels and coordinates
  nms <- data.frame(
    
    name = c("S1", "S2", "S3", "Age", "Edu", "mPA", "cPA", "Cog", "Aff", "Stat"),
    label = c( rep("S",3), "Age", "Educ.", "m-PA", "c-PA", "Cogn.", "Affect", "Mar.\nStat."),
    x = c(rep(1,2), 3, 3, 1, 1, 2, 2, 2, 3),
    y = c(0, 4, 0, 1, 3, 1, 1, 3, 2, 3)
    
  ) %>% mutate( colour = if_else(name %in% paste0("S",1:3), "black", "white") )
  
  # prepare the DAG
  dag <- dagify(
    
    Aff ~ mPA + cPA + Age + Stat + Edu,
    Cog ~ mPA + cPA + Edu + Aff + Age + Stat,
    Stat ~ Age,
    cPA ~ mPA + Age + Edu + Stat,
    mPA ~ Edu + S1,
    Edu ~ S2,
    Age ~ S3,
    
    latent = c("S1", "S2", "S3"),
    coords = nms[ , c("name","x","y")]
    
  ) %>%
    
    tidy_dagitty() %>%
    arrange(name) %>%
    mutate(
      selection = if_else(name %in% paste0("S",1:3), "1", "0"),
      curve = if_else( is.na(direction), NA, if_else(name == "cPA" & to == "Cog", 0.12, 0) )
    )
  
  # basic DAG
  dag0 <- dag %>%
    
    ggplot() +
    aes(x = x, y = y, xend = xend, yend = yend, shape = selection, colour = selection) +
    
    geom_dag_point(size = 20, fill = "white") +
    geom_dag_edges_arc( curvature = na.omit(dag$data$curve), arrow = grid::arrow(length = grid::unit(11, "pt"), type = "open") ) +
    scale_shape_manual( values = c(`1` = 22, `0` = 21) ) +
    scale_colour_manual( values = c("white", "black") ) +
    geom_dag_text(
      label = arrange(nms, name)$label,
      color = "black",
      size = 5
    ) +
    
    theme_dag() +
    theme(legend.position = "none")
  
  # save it
  ggsave(plot = dag0, filename = here("DAG.jpg"), dpi = 300, width = 9, height = 6)
  
  # return it
  if(plot == T) return(dag0) else return(dag)
  
}


# table outcomes, exposures and adjustment sets ----
adjustment_table <- function() data.frame(
  
  outcome = c(
    rep("Cognition",4),
    rep("Affect",3),
    rep("c-PA",2)
  ),
  
  exposure = c(
    rep("m-PA",2), "Education", "Marital Status", # Y = Cognition
    rep("m-PA",2), "Marital Status", # Y = Affect/Mental Health
    "m-PA", "Marital Status" # Y = c-PA
  ),
  
  mediator = c(
    rep(NA,4), # Y = Cognition
    rep(NA,3), # Y = Affect/Mental Health
    rep(NA,2) # Y = c-PA
  ),
  
  moderator = c(
    NA, "Education", rep(NA, 2), # Y = Cognition
    NA, "Education", NA, # Y = Affect/Mental Health
    rep(NA,2) # Y = c-PA
  ),
  
  effect = "total",
  
  adjustment_type = "canonical",
  
  adjustment_set = c(
    "Age, Education, Marital Status", "Age, Marital Status", # Cognition ~ f(m-PA)
    "Age, Marital Status", #"Age, Marital Status", # Cognition ~ f(Education)
    "Age, Education, m-PA", # Cognition ~ f(Marital Status)
    "Age, Education, Marital Status", # Affect ~ f(m-PA)
    "Age, Education, m-PA", "Age, Education", # Affect ~ f(Marital Status)
    "Age, Education, Marital Status", # c-PA ~ f(m-PA)
    "Age, Education, m-PA"  # c-PA ~ f(Marital Status)
  ),
  
  propensity_scores_matching = c(
    rep("Cosactiw ~ Age + Education + Marital_status", 2),
    "Education ~ Age + Marital_status", #"Education ~ Age + Marital_status + Cosactiw",
    "Marital_status ~ Age + Education + Cosactiw",
    rep("Cosactiw ~ Age + Education + Marital_status", 2),
    "Marital_status ~ Age + Education + Cosactiw",
    "Cosactiw ~ Age + Education + Marital_status",
    "Marital_status ~ Age + Education + Cosactiw"
  ),
  
  X = c(
    rep("Cosactiw * (Education + Age + Marital_status)", 2),
    "Education + Age + Marital_status", #NA,
    "Marital_status * (Education + Age + Cosactiw)",
    rep("Cosactiw * (Education + Age + Marital_status)", 2),
    "Marital_status * (Education + Age + Cosactiw)",
    "Cosactiw * (Education + Age + Marital_status)",
    "Marital_status * (Education + Age + Cosactiw)"
  )
  
) %>% mutate(
  
  outcome_variables = case_when(
    outcome == "Cognition" ~ "MMSE, FAQ, SA, Z_SA",
    outcome == "Affect" ~ "GDS15, GAI",
    outcome == "c-PA" ~ "PA"
  )
  
)
