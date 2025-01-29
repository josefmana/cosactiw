#
# This script includes utility functions used throughout analyses for this project.
#

#
# ROUNDED NUMBER ----
rprint <- function(x, d = 2) sprintf( paste0("%.",d,"f"), round(x, d) )

#
# RETURN [CI_LOWER, CI_HIGHER] ----
ciprint <- function(x, d = 2) paste0( "[", paste( rprint(x, d), collapse = ", "), "]" )

#
# GET RID OF LEADING ZEROES ----
zerolead <- function(x, d = 3) ifelse( x < .001, "< .001", sub("0.", ".", rprint(x, 3), fixed = T) )

#
# PRINT P-VALUE ----
pprint <- function(.p, .dec = 3, text = F) ifelse(
  
  test = text == T,
  yes  = ifelse( .p < .001, "< .001", paste0( "= ", zerolead(.p, .dec) ) ),
  no   = ifelse( .p < .001, "< .001",               zerolead(.p, .dec)   )
  
)

#
# GIVE CENTRAL TENDENCY ± VARIABILITY ----
cenvar <- function(.y, .dec = 2, cen = "mean", var = "sd", sep = " ± ") sapply(
  
  c(cen, var),
  function(fun) do.call(
    
    fun,
    list(.y, na.rm = T)
    
  ) %>% rprint(d = .dec)
  
) %>% paste(collapse = sep)


#
# UPPER BOUND ----
ubound <- function(x, dec = 2) ifelse( round(x,dec) > x, rprint(x,dec), rprint( x+1/(10^dec), dec ) )

#
# CAPITALISE FIRST LETTER ----
capitalise <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

#
# PLOT POSTERIOR PREDICTIONS ----
ppc_plot <- function(
    
    fit,
    data,
    y, x,
    labs  = list(NULL, NULL),
    meth  = "ppc_dens_overlay_grouped",
    draws = sample(1:4000,100),
    stat  = "mean"
  
  ) {
  
  pp_check(
    
    object = data[ , y],
    yrep   = posterior_predict(fit, newdata = data)[draws, ],
    fun    = meth,
    stat   = stat,
    group  = data[ , x]
      
  ) + labs(
    
    title    = labs[[1]],
    subtitle = labs[[2]]
    
  ) + theme(
    
    legend.position = "none",
    plot.title      = element_text(hjust = .5, face = "bold"),
    plot.subtitle   = element_text(hjust = .5)
    
  )
    
}

#
# FORMAT TABLE TO APA STYLE ----
gt_apa <- function(x, grp = NULL, nms = NULL, title = " ") x %>%
  
  gt(groupname_col = grp, rowname_col = nms) %>%
  tab_options(
    table.border.top.color            = "white",
    heading.title.font.size           = px(16),
    column_labels.border.top.width    = 3,
    column_labels.border.top.color    = "black",
    column_labels.border.bottom.width = 3,
    column_labels.border.bottom.color = "black",
    table_body.border.bottom.color    = "black",
    table.border.bottom.color         = "white",
    table.width                       = pct(100),
    table.background.color            = "white"
  ) %>%
  cols_align(align = "center") %>%
  tab_style(
    style = list(
      cell_borders(
        sides  = c("top", "bottom"),
        color  = "white",
        weight = px(1)
      ),
      cell_text(
        align="center"
      ),
      cell_fill(color = "white", alpha = NULL)
    ),
    locations = cells_body(
      columns = everything(),
      rows    = everything()
    )
  ) %>%
  tab_header( # title setup
    title = html(title)
  ) %>%
  opt_align_table_header(align = "left")

