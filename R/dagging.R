# This is a script used to write functions to represent causal assumptions
# for deducing  correct adjustment sets conditional on these assumptions
# for a subsequent statistical analysis

# prepare and save the DAG
make_dag <- function() {
  
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
    cPA ~ mPA + Age,
    mPA ~ Edu + S1,
    Edu ~ S2,
    Age ~ S3,
    
    coords = nms[ , c("name","x","y")]
    
  ) %>%
    
    tidy_dagitty() %>%
    arrange(name) %>%
    mutate(
      selection = if_else(name %in% paste0("S",1:3), "1", "0"),
      curve = if_else( is.na(direction), NA, if_else(name == "cPA" & to == "Cog", .3, 0) )
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
  ggsave(plot = dag0, filename = here("DAG.jpg"), dpi = 300, width = 7, height = 7)
  
  # return it
  return(dag0)
  
}
