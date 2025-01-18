#
# Functions to describe retrospectively remembered leisure activities with respect to cognitive SA
#

#
# PARTICIPANTS REPORTING THE SAME ACTIVITY AS SEASONAL & NON-SEASONAL ----
both_seasons <- function(.input) with(
  
  .input,
  cbind.data.frame(all$ID, all$activities, seasonal$activities, `non-seasonal`$activities) %>%
    `colnames<-`( c("ID","all","seas","nseas") ) %>%
    mutate(ctrl = seas + nseas) %>%
    mutate(ctrl0 = all - ctrl) %>%
    filter(ctrl0 != 0) %>%
    select(ID) %>%
    unlist(use.names = F)
  
)

#
# NUMBER OF PARTICIPANTS ----
count_subjects <- function(.data, .subset) sapply(

  c("SA","nonSA"),
  function(i)
    nrow( subset(.data[[.subset]], SA == i) )

)

#
# ACTIVITY COUNTS PER (SA) STATUS ----
activity_counts <- function(.input, .data) {
  
  # extract descriptive statistics of activity counts
  output <- with(
    
    .input, lapply(
      
      c( "activities", unique(map$type), unique(map$category) ),
      function(y) sapply(
        
        c("nonSA","SA"),
        function(i)
          
          sapply(
            names(.data),
            function(j) if_else(
              condition = sum( subset(.data[[j]], SA == i)[ , y] ) == 0,
              true = "-",
              false = cenvar(.y = subset(.data[[j]], SA == i)[ , y], .dec = 2, cen = "mean", var = "sd", sep = " ± ")
            )
          )
        
      ) %>%
        
        as.data.frame() %>%
        rownames_to_column("season") %>%
        mutate(category = y, .before = 1)
      
    ) %>%
      
      do.call(rbind.data.frame, .) %>%
      filter( !( ( category %in% c( "mental", unique( subset(map, type == "mental")$category ) ) ) & ( grepl("season",season) ) ) ) %>%
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
  
  # add statistical tests and effect sizes
  output <- sapply(
    
    1:nrow(output),
    function(i) {
      
      # extract variables to be used
      s <- output$season[i]
      v <- with(
        output,
        case_when(
          category[i] == "all" & type[i] == "all" ~ "activities",
          category[i] == "all"                    ~ type[i],
          .default = category[i]
        )
      )
      
      # calculate t-test
      t <- t.test(  as.formula( paste0(v," ~ SA") ), data = .data[[s]] )
      d <- cohen.d( as.formula( paste0(v," ~ SA") ), data = .data[[s]] )
      
      # calculate Mann-Whitney test
      wilcox <- wilcox.test( as.formula( paste0(v," ~ SA") ), data = .data[[s]] )
      vda    <- VD.A( as.formula( paste0(v," ~ SA") ), data = .data[[s]] )
      
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
    cbind.data.frame(output, .) %>%
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
      ),
      season = case_when(
        season == "all" & type == "mental" ~ "Non-seasonal",
        season == "all" & type != "mental" ~ "Seasonal & Non-seasonal",
        .default = capitalise(season)
      )
    )
  
  # return the table
  return(output)
  
}

#
# CONTINGENCY TABLES ----
contingency_tables <- function(.input, .data) with(
  
  .input, lapply(
    
    set_names( names(.data) ),
    function(x)
      
      .data[[x]] %>%
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
)

#
# CHI-SQUARE TESTS ----
chi_squares <- function(.tabs) lapply(
  
  set_names( names(.tabs) ),
  function(i) sapply(
    
    c("physical","mental"),
    function(x) {
      
      # All mental activities were non-seasonal
      if (i != "non-seasonal" & x == "mental") return(
        data.frame(
          season   = i,
          type     = x,
          chisq    = NA,
          df       = NA,
          p        = NA,
          cramer_v = NA
        )
      )
      else {
        
        # contingency table to be analysed
        contab <-
          .tabs[[i]] %>%
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
            season   = i,
            type     = x,
            chisq    = rprint(chsq$statistic, 3),
            df       = rprint(chsq$parameter, 0),
            p        = pprint(chsq$p.value, 3, text = F),
            cramer_v = rprint(v, 3)
          )
        )
        
      }
    }

  ) %>% t()

) %>%
  
  do.call(rbind.data.frame, .) %>%
  mutate_all(unlist, use.names = F)

