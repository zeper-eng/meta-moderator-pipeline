######################
# Loading libraries
######################

library(ggplot2)
library(tidyverse)
library(metafor)
library(here)

source(here("code","Analysis.R"))

dataset_name_var <- "synthetic_demo"

######################
# Generate synthetic data
######################

df <- generate_synthetic_meta(200)

######################
# Minimal recode + lookup (generic)
######################

# Simple recode (will expand this later and explain what this is really for)
recode_map <- list()

# Minimal lookup table for labeling
lookup_df <- tibble(
  predictor = character(),
  code = character(),
  label = character()
)

######################
# Run pipeline
######################

model <- run_meta_pipeline(
  df = df,
  recode_map = recode_map,
  out_dir = here(),
  dataset_name = dataset_name_var,
  
  m1i = "group_a_mean",
  sd1i = "group_a_sd",
  n1i = "group_a_n",
  m2i = "group_b_mean",
  sd2i = "group_b_sd",
  n2i = "group_b_n",
  
  group_col = "target_construct"
)

######################
# Multiple testing + labeling
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
  here("outputs", "heatmaps", dataset_name_var)
)

######################
# Forest plots
######################

coef_df <- model$model_output$coefs

analyses <- unique(coef_df$target_construct)

walk(
  analyses,
  ~ plot_forest_analysis(
      coef_df,
      .x,
      prefix = "all",
      out_dir = here("outputs", "forestplots", dataset_name_var),
      desired_p_val = "P_adj_fdr"
    )
)

######################
# Save outputs
######################

dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)

write.csv(
  results_df,
  file = paste0("outputs/tables/", dataset_name_var, "_qmod_results.csv"),
  row.names = FALSE
)

write.csv(
  coef_df,
  file = paste0("outputs/tables/", dataset_name_var, "_coefs_results.csv"),
  row.names = FALSE
)