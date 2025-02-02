
# This is a script used to write functions to represent causal assumptions
# for deducing  correct adjustment sets conditional on these assumptions
# for a subsequent statistical analysis

#
# CHANGE SETS TO EQUATIONS ----
set2equation <- function(x, bracket = F) sapply(
  
  X   = 1:length(x),
  FUN = function(i) if_else(
    
    condition = bracket == T,
    true      = sub("{", "(", sub("}", ")", gsub(", ", " + ", x[i]), fixed = T), fixed = T),
    false     = sub("{", "", sub("}", "", gsub(", ", " + ", x[i]), fixed = T), fixed = T)
    
  ),
  USE.NAMES = F
  
)

#
# PREPARE & SAVE A DAG ----
make_dag <- function(plot = T) {
  
  # set-up a data frame with node labels and coordinates
  nms <- data.frame(
    
    name  = c("S", "Age", "Education", "mPA", "cPA", "Cognition", "Affect"),
    label = c("S", "Age", "Educ.", "m-PA", "c-PA", "Cogn.", "Affect"),
    x     = c(2, 3, 2, 1, 1, 3, 2),
    y     = c(0, 1, 1, 1, 3, 3, 3)
    
  ) %>% mutate( colour = if_else(name %in% paste0("S",1:3), "black", "white") )
  
  # prepare the DAG
  dag <- dagify(
    
    Affect    ~ mPA + cPA + Age + Education,
    Cognition ~ mPA + cPA + Education + Affect + Age,
    cPA       ~ mPA + Age + Education,
    mPA       ~ Education,
    S         ~ mPA + Education + Age,
    
    latent = "S",
    coords = nms[ , c("name","x","y")]
    
  ) %>%
    
    tidy_dagitty() %>%
    arrange(name) %>%
    mutate(
      selection = if_else(name == "S", "1", "0"),
      curve     = if_else( is.na(direction), NA, if_else(name == "cPA" & to == "Cognition", 0.60, 0) )
    )
  
  # basic DAG
  dag0 <- dag %>%
    
    ggplot() +
    aes(x = x, y = y, xend = xend, yend = yend, shape = selection, colour = selection) +
    
    geom_dag_point(size = 20, fill = "white", stroke = 1) +
    geom_dag_edges_arc( curvature = na.omit(dag$data$curve), arrow = grid::arrow(length = grid::unit(11, "pt"), type = "open") ) +
    scale_shape_manual( values = c(`1` = 22, `0` = 21) ) +
    scale_colour_manual( values = c("white", "black") ) +
    geom_dag_text(
      label = arrange(nms, name)$label,
      color = "black",
      size  = 5.33
    ) +
    
    theme_dag() +
    theme(legend.position = "none")
  
  # return it
  if(plot == T) return(dag0) else return(dag)
  
}

#
# ADJUSTMENT SETS  ----
adjustment_table <- function(DAG) data.frame(
  
  outcome         = c( rep("Cognition",4), rep("Affect",3), "cPA" ),
  exposure        = c(
    rep("mPA",2), "cPA", "Education", # Y = Cognition
    rep("mPA",2), "cPA", # Y = Affect/Mental Health
    "mPA" # Y = c-PA
  ),
  moderator       = c( NA, "cPA", rep(NA,3), "cPA", rep(NA,2) ),
  effect          = "total",
  adjustment_type = "canonical"
  
) %>%
  
  mutate(
    
    adjustment_set = sapply(
      
      1:nrow(.),
      function(i) ( DAG %>% ggdag_adjustment_set(
        
        outcome  = outcome[i],
        exposure = ifelse( is.na(moderator[i]), exposure[i], c(exposure[i], moderator[i]) ),
        effect   = effect[i],
        type     = adjustment_type[i]
        
      ) )$data %>%
        
        select(set) %>%
        unique() %>%
        unlist(use.names = F)
      
    ),
    
    Y = case_when(
      
      outcome == "Cognition" ~ "{MMSE, FAQ, SA, Z_SA, Delayed_recall_z, TMT_B_z, BNT_30_z, VF_Animals_z}",
      outcome == "Affect"    ~ "{GDS15, GAI, Depr, Anx}",
      outcome == "cPA"       ~ "{cPA}"
      
    ),
    
    X = if_else(
      condition = is.na(moderator),
      true      = paste0( exposure," * ",set2equation(adjustment_set, bracket = T) ),
      false     = paste0( exposure," * ",moderator," * ",set2equation(adjustment_set, bracket = T) )
    ),
    term = if_else( is.na(moderator), exposure, paste0(exposure,":",moderator) ),
    matching =
      paste0( exposure," ~ ",set2equation(adjustment_set, bracket = F) ) %>%
      sub("mPA", "Cosactiw", . )
    
  )
