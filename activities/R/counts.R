#
# Functions to describe & predict counts of retrospectively remembered leisure activities
# with respect to cognitive SuperAging.
#

#
# NUMBER OF PARTICIPANTS ----
count_subjects <- function(.data) sapply(

  c("SA","nonSA"),
  function(i)
    nrow( subset(.data, SA == i) )

)

#
# DEMOGRAPHY COMPARISONS ----
compare_demography <- function(data) data %>%
  
  group_by(SA) %>%
  summarise( across( all_of( c("Age_years","Education_years") ), cenvar ) ) %>%
  column_to_rownames("SA") %>%
  t() %>%
  as.data.frame() %>%
  
  # add t-test results
  mutate(
    t  = unlist(sapply( rownames(.), function(y) rprint(t.test(as.formula( paste0(y," ~ SA") ), data = data)$statistic, d = 3) ), use.names = F ),
    df = unlist(sapply( rownames(.), function(y) rprint(t.test(as.formula( paste0(y," ~ SA") ), data = data)$parameter, d = 2) ), use.names = F ),
    p  = unlist(sapply( rownames(.), function(y) pprint(t.test(as.formula( paste0(y," ~ SA") ), data = data)$p.value, .dec = 3, text = T) ), use.names = F )
  )

#
# SPECIFY REGRESSION TYPES ----
specify_regression <- function(.input) data.frame(
  
  y = with( .input, c( "activities", unique(map$type), unique(map$category) ) )
  
) %>% mutate(
  
  type = case_when(
    y %in% c("activities") ~ "lm",
    #y %in% c("community_activities", "mixed_mental", "house_and_garden", "strengthening_resistance_exercise") ~ "glm",
    .default = "lm"
  )
  
)

#
# ACTIVITY COUNTS REGRESSIONS ----
count_regressions <- function(.data, .specs) {
  
  # prepare data
  # re-coding such that the data are compatible with the VLS and COBRA analyses
  d0 <- .data %>% mutate(
    
    saf = factor(
      if_else(SA == "SA", T, F),
      levels = c(T, F), 
      labels = c("Superager", "non-Superager")
    ),
    edu2 = factor(
      if_else(as.numeric(Education_level) > 2, 2, 1),
      levels = 1:2
    ),
    age = Age_years
  )
  
  # do the regressions
  fits <- with(
    
    .specs, lapply(
      
      set_names(y),
      function(i)
        
        # zero-inflate negative-binomial
        if (type[y == i] == "glm" ) glm(
          
          formula = as.formula( paste0(i," ~ saf + age + edu2") ),
          data    = d0,
          family  = poisson(link = "log")
          
          # linear regression or vanilla negative-binomial
        ) else do.call(
          
          what = type[y == i] ,
          args = list(
            formula = as.formula( paste0(i," ~ saf + age + edu2") ),
            data    = d0
          )
          
        )
    )
  )
  
  # return
  return(fits)
  
}

#
# DIAGNOSE MODELS ----
diagnose_regressions <- function(.fits) lapply(
  
  set_names( names(.fits) ),
  function(y) check_model(x = .fits[[y]])
  
)


#
# ACTIVITY COUNTS PER (SA) STATUS ----
count_activities <- function(.input, .data) with(
  
  # extract descriptive statistics of activity counts
  .input, lapply(
    
    c( "activities", unique(map$type), unique(map$category) ),
    function(y) sapply(
      
      c("nonSA","SA"),
      function(i) if_else(
        
        condition = sum( subset(.data, SA == i)[ , y] ) == 0,
        true      = "-",
        false     = cenvar(.y = subset(.data, SA == i)[ , y], .dec = 2, cen = "mean", var = "sd", sep = " ± ")
        
      )
      
    ) %>%
      
      t() %>%
      as.data.frame() %>%
      mutate(category = y, .before = 1)
    
  ) %>%
    
    do.call(rbind.data.frame, .) %>%
    mutate(
      type = sapply(
        1:nrow(.),
        function(i)
          case_when(
            
            category[i] == "activities" ~ "all",
            category[i] == "mental" |   category[i] %in% unique( subset(map, type == "mental")$category )   ~ "mental",
            category[i] == "physical" | category[i] %in% unique( subset(map, type == "physical")$category ) ~ "physical"
            
          )
      ),
      category = case_when(
        category %in% c("activities","mental","physical") ~ "all",
        .default = category
      ),
      .before = 1
    ) %>%
    arrange(type)
  
)

#
# ADD SIMPLE COMPARISONS ----
add_comparisons <- function(.table, .data) sapply(
  
  1:nrow(.table),
  function(i) {
    
    # extract variables to be used
    v <- with(
      
      .table, case_when(
        
        category[i] == "all" & type[i] == "all" ~ "activities",
        category[i] == "all"                    ~ type[i],
        .default                                = category[i]
        
      )
    )
    
    # calculate t-test
    t <- t.test( as.formula( paste0(v," ~ SA") ), data = .data)
    d <- cohen.d(as.formula( paste0(v," ~ SA") ), data = .data)
    
    # calculate Mann-Whitney test
    wilcox <- wilcox.test( as.formula( paste0(v," ~ SA") ), data = .data)
    vda    <- VD.A( as.formula( paste0(v," ~ SA") ), data = .data)
    
    # return it
    return( c(
      #cohens_d     = paste0( rprint(d$estimate), " ", ciprint(d$conf.int) ),
      cohens_d      = rprint(d$estimate),
      t_stat        = rprint(t$statistic),
      df            = rprint(t$parameter),
      p_ttest       = zerolead(t$p.value),
      VD.A          = rprint(vda$estimate),
      W             = rprint(wilcox$statistic),
      p_mannwhitney = pprint(wilcox$p.value, text = F)
    ) )
  }
  
) %>%
  
  t() %>%
  as.data.frame() %>%
  cbind.data.frame(.table, . ) %>%
  mutate(
    
    # get rid of NaNs
    across( everything(), ~ case_when( grepl("NaN",.x) ~ "-", is.na(.x) ~ "-", .default = .x ) ),
    
    # renaming
    category = if_else(
      condition = category == "all",
      true      = "-",
      false     = category
    ),
    type = case_when(
      type == "all"      ~ "Both",
      type == "mental"   ~ "Mental",
      type == "physical" ~ "Physical"
    )
  ) ## ---- add gt() ----

#
# EXTRACT REGRESSION PARAMETERS ----
extract_parameters <- function(fits, input) sapply(
  
  names(fits),
  function(y) cbind.data.frame(
    
    b  = # the coefficient
      coefficients(fits[[y]]) %>%
      rprint(),
    CI = # 95% confidence interval
      confint.lm(fits[[y]]) %>%
      as.data.frame() %>%
      mutate_all(rprint) %>%
      mutate( CI = paste0("[", `2.5 %`,", ",`97.5 %`,"]") ) %>%
      select(CI),
    t  = # t value
      summary(fits[[y]])$coefficients[ , 't value']  %>%
      rprint(),
    p  = # p value
      summary(fits[[y]])$coefficients[ , 'Pr(>|t|)'] %>%
      sapply( function(i) pprint(.p = i, text = F) )
    
  ) %>%
    
    rownames_to_column("x") %>%
    pivot_wider(names_from = x, values_from = -x, names_vary = "slowest")
  
) %>%
  
  t() %>%
  as.data.frame() %>%
  rownames_to_column("y") %>%
  mutate( # prepare variables for glueing to a table
    type     = sapply(
      1:nrow(.),
      function(i) case_when(
        
        y[i] == "activities" ~ "all",
        y[i] == "mental"   | y[i] %in% unique( with( input, subset(map, type == "mental"  )$category ) )   ~ "mental",
        y[i] == "physical" | y[i] %in% unique( with( input, subset(map, type == "physical")$category ) )  ~ "physical"
        
      )
    ),
    category = sapply(
      1:nrow(.),
      function(i) ifelse(
        
        test = y[i] %in% c("activities", "mental", "physical"),
        yes  = "all",
        no   = unique( input$map[input$map$category == y[i], "category"] )
        
      )
    ),
    .before = 1
  ) %>%
  select(-y)


#
# ADD LINEAR REGRESSIONS ----
add_regression_parameters <- function(.table, .fits, .input) left_join(

  .table,
  extract_parameters(fits = .fits, input = .input),
  by = c("type", "category")

) %>% mutate_all( ~ unlist(.x, use.names = F) )
  

#
# CONTINGENCY TABLES ----
contingency_tables <- function(.input, .data) with(
  
  .input, .data %>%
    
    select( SA, all_of( unique(map$category) ) ) %>%
    mutate(
      across(
        .cols = all_of(unique(map$category)),
        .fns  = ~ if_else(.x > 0, T, F)
      )
    ) %>%
    group_by(SA) %>%
    summarise_all( list(sum) ) %>%
    column_to_rownames("SA") %>%
    t() %>%
    as.data.frame() %>%
    mutate(
      Type = unlist(
        x = sapply(
          X = rownames(.),
          FUN = function(i) with( map, unique( type[category == i] ) )
        ),
        use.names = F
      ),
      .before = 1
    ) %>%
    rownames_to_column("Category") %>%
    group_by(Type) %>%
    mutate(
      across(
        .cols  = ends_with("SA"),
        .fns   = ~ paste0(.x, " (", rprint( 100 * .x/sum(.x), 0),"%)"),
        .names = "{col}_perc"
      )
    ) %>%
    ungroup()
    
)

#
# CHI-SQUARE TESTS ----
chi_squares <- function(.tabs) sapply(
  
  c("physical","mental"),
  function(x) {
    
    # contingency table to be analysed
    contab <-
      .tabs %>%
      mutate(sum = SA + nonSA) %>% # drop zero-sum rows
      filter(Type == x & sum > 0) %>%
      select( ends_with("SA") ) %>%
      as.matrix() 
    
    # testing
    chsq <- chisq.test(contab)
    v    <- cramerV(contab)
    
    # return table
    return(
      data.frame(
        type     = x,
        chisq    = rprint(chsq$statistic, 3),
        df       = rprint(chsq$parameter, 0),
        p        = pprint(chsq$p.value, 3, text = F),
        cramer_v = rprint(v, 3)
      )
    )

  }

) %>%
  
  t() %>%
  as.data.frame() %>%
  mutate_all(unlist, use.names = F)

