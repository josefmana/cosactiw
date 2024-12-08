# This is a script containing functions covering all aspect of my (re-)analysis of attitudes-towards-physical-activity
# project from COSACTIW from data import to results visualisation.


# READ DATA FILE ----
read_data <- function(folder, file) here(folder, file)


# PRINT ROUNDED NUMBERS ----
rprint <- function(x, dec = 2) sprintf( paste0("%.",dec,"f"), round(x, dec) )


# CALCULATE MEAN & STANDARD DEVIATION ----
msd <- function(x, .dec = 2) paste0( rprint(mean(x, na.rm = T), .dec), " (", rprint(sd(x, na.rm = T), .dec), ")" )


# EXTRACT FREQUENCIES ----
frequencies <- function(x, .sep = "/") paste(table(x) , collapse = .sep)


# DROP LEADING ZERO ----
zerolead <- function(x, .dec = 3, fortext = F) ifelse(
  
  test = fortext == T,
  yes = ifelse(x >= .001, paste0( "= ", sub("0.", ".", rprint(x, .dec), fixed = T) ), "< .001"),
  no = ifelse(x >= .001, sub("0.", ".", rprint(x, .dec), fixed = T), "< .001")

)


# EXTRACT TEST STATISTICS ----
extract_stats <- function(.models, labs, type = "continuous") {
  
  # return F-test results for continuous outcomes
  if (type == "continuous") tab <- sapply(
    
    names(.models),
    function(i) {
      
      aov <- anova(.models[[i]]) # extract ANOVA table
      eta <- eta_squared(.models[[i]], partial = F, alternative = "two")
      
      # prepare and return outcome columns
      return(
        
        data.frame(
          
          label = with(labs, label[column == i]),
          Stat = with( aov, paste0("F(",paste(Df, collapse = ", ") ,") = ", rprint(na.omit(`F value`), 3) ) ),
          ES = with( eta, paste0("eta_2 = ", rprint(Eta2, 2)," [",rprint(CI_low),", ",rprint(CI_high),"]") ),
          p.value = zerolead( aov$`Pr(>F)`, fortext = F )

        )
      )
      
    }
    
  ) else if (type == "categorical") tab <- sapply(
    
    names(.models),
    function(i) data.frame(
      
      label = with(labs, label[column == i]),
      Stat = with( .models[[i]], paste0("X2(",parameter,") = ", rprint(statistic, 3) ) ),
      ES = with( cramers_v(.models[[i]], alternative = "two"), paste0("Cramer's V = ", rprint(Cramers_v_adjusted, 2)," [",rprint(CI_low),", ",rprint(CI_high),"]") ),
      p.value = zerolead( .models[[i]]$p.value, fortext = F )
      
    )
  )
  
  # reformat and return
  tab <- tab %>% t() %>% as.data.frame() %>% mutate_all( ~unlist(.x, use.names = F) )
  return(tab)
  
}


# LIST VARIABLES OF INTEREST ----
list_outcomes <- function() data.frame(
  
  column = c(
    "Age",
    "Education",
    "MMSE",
    "Waist_circumference",
    paste0( "Chair_stand", c("","_z", "_level") ),
    paste0( "Arm_curl", c("", "_z", "_level") ),
    paste0( "Up_and_go", c("", "_z", "_level") ),
    paste0( "Two_min_step_test", c("", "_z", "_level") ),
    "SFT_z_composite"
  ),
  
  label = c(
    "Age (years)",
    "Education (years)",
    "MMSE (range 0-30)",
    "Waist circumference (cm)",
    paste("Chair Stand", c("(# in 30 sec)", "(z-score)", "normative value"), sep = " "),
    paste("Arm Curl", c("(# in 30 sec)", "(z-score)", "normative value"), sep = " "),
    paste("8-Foot Up and Go", c("(sec)", "(z-score)", "normative value"), sep = " "),
    paste("2 min Step test", c("(# of steps)", "(z-score)", "normative value"), sep = " "),
    "SFT (composite z-score)"
  ),
  
  role = c(
    rep("descriptor", 4),
    rep("outcome", 13)
  ),
  
  type = c(
    rep("continuous", 4),
    rep(c(rep("continuous", 2), "categorical"), 4),
    "continuous"
  )
  
)

  
# READ AND PRE-PROCESS DATA
import_data <- function(file) read.xlsx(
  
  xlsxFile = file,
  sheet = "7IPAQ_GAI_GDS_DRUGS"
  
) %>%
  
  # rename variables for z-score calculations
  rename(
    
    "Chair_stand_mean" = "X37", "Chair_stand_sd" = "X38",
    "Arm_curl_mean" = "X42", "Arm_curl_sd" = "X43",
    "Up_and_go_mean" = "X47", "Up_and_go_sd" = "X48",
    "Two_min_step_test_mean" = "X52", "Two_min_step_test_sd" = "X53",
    
    "Up_and_go_level" = "Up_go_level",
    "Two_min_step_test" = "SFT_Two_minute_STEP_TEST",
    "PA_att" = "PA.attitude",
    "Education" = "Number_of_years_of_study"

  ) %>%
  
  # drop 'SFT_' from Senior Fitness Test variables names
  rename_with( .cols = starts_with("SFT_"), .fn = ~ sub("SFT_", "", .x) ) %>%
  
  # keep only variables of interest
  select(
    ID, PA_att, # identification and group (predictor) variables
    Age, MMSE, Education, Waist_circumference, # descriptive variables
    starts_with("Chair_stand"), starts_with("Arm_curl"), starts_with("Up_and_go"), starts_with("Two_min_step_test") # primary outcomes (SFT)
  ) %>%
  
  # drop cases with PA == 6
  # because they were dropped in the original analysis as well
  filter( PA_att != 6 ) %>%
  
  # mutate variables where needed
  mutate(
    
    PA_att = factor(
      
      case_when(
        PA_att == 1 ~ "I am an athletic person",
        PA_att == 2 ~ "I enjoy movement",
        PA_att == 3 ~ "I exercised at least thrice a wk",
        PA_att == 4 ~ "I am not a sporty person",
        PA_att == 5 ~ "I am not a sporty person"
      )
      
    ),
    
    across(
      
      .cols = all_of( paste(c("Chair_stand", "Arm_curl", "Up_and_go", "Two_min_step_test"), "level", sep = "_") ),
      .fns = ~ factor(.x, levels = 1:3, ordered = T)
      
    ),
    
    across(
      
      .cols = all_of( c("Chair_stand", "Arm_curl", "Up_and_go", "Two_min_step_test") ),
      .fns = ~ (.x - get( paste0(cur_column(),"_mean") ) ) / get( paste0(cur_column(),"_sd") ),
      .names = "{.col}_z"
      
    ),
    
    Up_and_go_z = -Up_and_go_z, # reverse scored
    SFT_z_composite = rowMeans( across( all_of( ends_with("_z") ) ) )
    
  )


# FIT LINEAR REGRESSIONS ----
fit_regressions <- function(.data, .variables) lapply(
  
  set_names( with( .variables, all_of(column[type == "continuous"]) ) ),
  function(y) lm(
    
    formula = paste0(y," ~ PA_att"),
    data = .data
    
  )
) 


# PREPARE CHI-SQUARES FOR NOMINAL VARIABLES ----
compute_chisq <- function(.data, .variables) lapply(
  
  set_names( with( .variables, all_of(column[type == "categorical"]) ) ),
  function(y)
    
    chisq.test( table(.data[ , c(y, "PA_att") ]) )
  
)


# PREPARE TABLE WITH OMNIBUS TESTS RESULTS ----
prepare_table <- function(.data, .models, .chisquares, .variables) {
  
  # descriptive part of the table
  desc <- .data %>%
    
    group_by(PA_att) %>%
    summarise(
      N = length(PA_att),
      across(with( .variables, all_of(column[type == "continuous"]) ), msd),
      across(with( .variables, all_of(column[type == "categorical"]) ), frequencies)
    ) %>%
    
    column_to_rownames("PA_att") %>%
    t() %>%
    as.data.frame() %>%
    rownames_to_column("column") %>%
    left_join( .variables[ , c("column", "label")] , . ) %>%
    select(-column)
  
  # extract test results
  stats <- rbind.data.frame(
    
    extract_stats(.models = .models, labs = .variables, type = "continuous"),
    extract_stats(.models = .chisquares, labs = .variables, type = "categorical")
    
  )
  
  # put them together
  tab <- left_join(desc, stats, by = "label")
  return(tab)
  
}


# PREPARE TABLE WITH POST-HOC COMPARISONS RESULTS ----
table_pairwise <- function(.models, .variables) lapply(
  
  set_names( names(.models) ),
  function(i) tukey_hsd(.models[[i]]) %>%
    
    as.data.frame() %>%
    mutate(
      comp = paste0(group1," vs. ",group2),
      Estimate = paste0( rprint(estimate,2)," [", rprint(conf.low,2),", ",rprint(conf.high,2),"]" ),
      p.value = sapply(p.adj, function(p) zerolead(p, .dec = 3, fortext = F) )
    ) %>%
    select(comp, Estimate, p.value) %>%
    pivot_wider( names_from = comp, names_glue = "{comp} - {.value}", values_from = c(Estimate, p.value) ) %>%
    mutate(label = .variables$label[.variables$column == i], .before = 1)
  
) %>%
  
  reduce(full_join) %>%
  relocate(`I am an athletic person vs. I enjoy movement - p.value`, .after = `I am an athletic person vs. I enjoy movement - Estimate`) %>%
  relocate(`I am an athletic person vs. I exercised at least thrice a wk - p.value`, .after = `I am an athletic person vs. I exercised at least thrice a wk - Estimate`)


# PREPARE PLOT WITH POST-HOC COMPARISONS ----
plot_pairwise <- function(.data, .variables, save = F) {
  
  # keep only z-score variables
  vars <- .variables[ grepl("(z-score)", .variables$label, fixed = T), ]
  
  # extract data for later processing
  d0 <- .data %>%
    select( PA_att, all_of(vars$column) ) %>%
    pivot_longer(cols = -PA_att, names_to = "outcome", values_to = "value") %>%
    mutate( lab = sapply(outcome, function(i) vars$label[vars$column == i], USE.NAMES = F) )
  
  # extract Tukey HSD statistical comparisons
  stat.test <- d0 %>%
    group_by(lab) %>%
    tukey_hsd(value ~ PA_att) %>%
    add_y_position() %>%
    mutate(y.position = 1.25 * y.position + 0.5)
  
  # plot it
  plt <- d0 %>%
    ggviolin(x = "PA_att", y = "value", add = "boxplot") +
    stat_pvalue_manual(stat.test, label = "p.adj", facet.by = "label", bracket.size = .2, tip.length = 0) +
    labs(x = NULL, y = NULL) +
    facet_wrap(~ lab, scales = "free_y") +
    theme_bw(base_size = 14) +
    theme( panel.grid = element_blank() )
  
  # save it if asked for
  if (save == T) {
    
    if ( !dir.exists("_figures") ) dir.create("_figures")
    ggsave(plot = plt, filename = here("_figures","Tukey_HSD.jpg"), dpi = 300, width = 13.3, height = 8.6)
    
  }
  
  # finish by returnig the plot
  return(plt)
  
}
