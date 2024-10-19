# This is a script used to write functions to represent causal assumptions
# for deducing  correct adjustment sets conditional on these assumptions
# for a subsequent statistical analysis


# function for changing set to equation syntax ----
set2equation <- function(x, bracket = F) sapply(
  
  X = 1:length(x),
  FUN = function(i) if_else(
    
    condition = bracket == T,
    true = sub("{", "(", sub("}", ")", gsub(", ", " + ", x[i]), fixed = T), fixed = T),
    false = sub("{", "", sub("}", "", gsub(", ", " + ", x[i]), fixed = T), fixed = T)
    
  ),
  USE.NAMES = F
  
)

# prepare & save the DAG ----
make_dag <- function(plot = T) {
  
  # set-up a data frame with node labels and coordinates
  nms <- data.frame(
    
    name = c("S1", "S2", "S3", "Age", "Education", "mPA", "cPA", "Cognition", "Affect", "Status"),
    label = c( rep("S",3), "Age", "Educ.", "m-PA", "c-PA", "Cogn.", "Affect", "Mar.\nStat."),
    x = c(rep(1,2), 3, 3, 1, 1, 2, 2, 2, 3),
    y = c(0, 4, 0, 1, 3, 1, 1, 3, 2, 3)
    
  ) %>% mutate( colour = if_else(name %in% paste0("S",1:3), "black", "white") )
  
  # prepare the DAG
  dag <- dagify(
    
    Affect ~ mPA + cPA + Age + Status + Education,
    Cognition ~ mPA + cPA + Education + Affect + Age + Status,
    Status ~ Age,
    cPA ~ mPA + Age + Education + Status,
    mPA ~ Education + S1,
    Education ~ S2,
    Age ~ S3,
    
    latent = c("S1", "S2", "S3"),
    coords = nms[ , c("name","x","y")]
    
  ) %>%
    
    tidy_dagitty() %>%
    arrange(name) %>%
    mutate(
      selection = if_else(name %in% paste0("S",1:3), "1", "0"),
      curve = if_else( is.na(direction), NA, if_else(name == "cPA" & to == "Cognition", 0.12, 0) )
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
adjustment_table <- function(DAG) data.frame(
  
  outcome = c( rep("Cognition",4), rep("Affect",3), rep("cPA",2) ),
  exposure = c(
    "mPA", "cPA", "Education", "Status", # Y = Cognition
    "mPA", "cPA", "Status", # Y = Affect/Mental Health
    "mPA", "Status" # Y = c-PA
  ),
  effect = "total",
  adjustment_type = "canonical"
  
) %>%
  
  mutate(
    
    adjustment_set = sapply(
      
      1:nrow(.),
      function(i) ( DAG %>% ggdag_adjustment_set(
        
        outcome = outcome[i],
        exposure = exposure[i],
        effect = effect[i],
        type = adjustment_type[i]
        
      ) )$data %>%
        
        select(set) %>%
        unique() %>%
        unlist(use.names = F)
      
    ),
    
    Y = case_when(
      
      outcome == "Cognition" ~ "{MMSE, FAQ, SA, Z_SA}",
      outcome == "Affect" ~ "{GDS15, GAI}",
      outcome == "cPA" ~ "{cPA}"
      
    ),
    
    X = paste0( exposure," * ",set2equation(adjustment_set, bracket = T) ),
    matching = paste0( exposure," ~ ",set2equation(adjustment_set, bracket = F) )
    
  )
