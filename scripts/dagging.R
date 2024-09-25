# This is a script used to represent causal assumptions & deduce correct adjustment sets
# conditional on the assumptions for a subsequent statistical analysis

rm( list = ls() ) # clean environment

# set-up libraries
library(here)
library(tidyverse)
library(ggdag)
library(ggraph)
library(patchwork)

# set-up theme
theme_set( theme_dag() )

# set-up a data frame with node labels and coordinates
nms <- data.frame(
  
  name = c("S1", "S2", "S3", "Age", "Edu", "mPA", "SA", "Y"),
  label = c( rep("S",3), "Age", "Educ.", "m-PA", "SA", "Y"),
  x = c( rep(1.5,3), rep(2,3), rep(3,2) ),
  y = c(1, 2, 3, 3, 2, 1, 2, 1)
  
) %>% mutate( colour = if_else(name %in% paste0("S",1:3), "black", "white") )

# prepare the DAG
dag <- dagify(
  
  Y ~ mPA + SA + Age,
  SA ~ mPA + Edu + Age,
  mPA ~ Edu + S1,
  Edu ~ S2,
  Age ~ S3,
  
  coords = nms[ , c("name","x","y")]
  
) %>%
  
  tidy_dagitty() %>%
  arrange(name) %>%
  mutate( selection = if_else(name %in% paste0("S",1:3), "1", "0") )

# basic DAG
dag0 <- dag %>%
  
  ggplot() +
  aes(x = x, y = y, xend = xend, yend = yend, shape = selection, colour = selection) +
  
  geom_dag_point(size = 20, fill = "white") +
  geom_dag_edges( arrow_directed = grid::arrow(length = grid::unit(11, "pt"), type = "open") ) +
  scale_shape_manual( values = c(`1` = 22, `0` = 21) ) +
  scale_colour_manual( values = c("white", "black") ) +
  geom_dag_text(
    label = arrange(nms, name)$label,
    color = "black",
    size = 5
  ) +
  
  theme_dag() +
  theme(legend.position = "none")

# total effect of midlife-PA on Y
dag %>%
  
  mutate(
    
    fill = case_when(
      selection == "1" ~ "covariates",
      name %in% c("Age", "Edu") ~ "covariates",
      name %in% c("SA", "Y") ~ "path",
      name == "mPA" ~ "exposure"
    ),
    
    colour = case_when(
      
    )
    
  )
