######################
# Loading libraries
######################

library(ggplot2)
library(tidyverse)
library(metafor)
library(here)

source(here("code","Analysis.R"))

######################
# Generate synthetic data
######################

df <- generate_synthetic_meta(200)

######################
# Recode + lookup
######################

######################
# Run pipeline
######################

model <- run_meta_pipeline(
  df = df,
  recode_map = recode_map,
  out_dir = here(),
  
  m1i = "group_a_mean",
  sd1i = "group_a_sd",
  n1i = "group_a_n",
  m2i = "group_b_mean",
  sd2i = "group_b_sd",
  n2i = "group_b_n",
  
  group_col = "target_construct"
)

######################
# Multiple testing + labeling (THIS handles mapping)
######################

model <- multiple_testing_and_remapping_labels(
  model,
  lookup_df = lookup_df
)

######################
# Heatmaps
######################

results_df <- model$model_output$results

make_heatmaps(
  results_df,
  here("heatmaps")
)

######################
# Forest plots (split restored)
######################

coef_df <- model$model_output$coefs

# categorical predictors defined by lookup (same as original workflow)
categorical_predictors <- unique(lookup_df$predictor)

continuous_predictors <- setdiff(
  unique(coef_df$predictor),
  categorical_predictors
)

analyses <- unique(coef_df$target_construct)

coef_cont <- coef_df %>%
  filter(predictor %in% continuous_predictors)

coef_cat <- coef_df %>%
  filter(predictor %in% categorical_predictors)

# Continuous
walk(
  analyses,
  ~ plot_forest_analysis(
      coef_cont,
      .x,
      prefix = "continuous",
      out_dir = here("forestplots"),
      desired_p_val = "P_adj_fdr"
    )
)

# Categorical
walk(
  analyses,
  ~ plot_forest_analysis(
      coef_cat,
      .x,
      prefix = "categorical",
      out_dir = here("forestplots"),
      desired_p_val = "P_adj_fdr"
    )
)

######################
# Save outputs
######################

dir.create("tables", recursive = TRUE, showWarnings = FALSE)

write.csv(
  results_df,
  file = "tables/qmod_results.csv",
  row.names = FALSE
)

write.csv(
  coef_df,
  file = "tables/coefs_results.csv",
  row.names = FALSE
)