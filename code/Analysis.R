library(ggplot2)
library(tidyverse)
library(metafor)
library(readxl)
library(rlang)

#########################
#Analysis functions#
########################

########################
#' Create heatmap of significant moderator results.
#'
#' Creates a heatmap to visualize significant moderator test results
#' across analyses and predictors.
#'
#' @param df A data.frame (or tibble) containing moderator test results.
#' Must include columns corresponding to the x-axis variable, y-axis variable,
#' and fill variable.
#'
#' @param x A column name (backticks) in 'df'. This is the variable to display on the x-axis.
#' 
#' @param y A column name (backticks) in 'df'. This is the variable to display on the y-axis.
#' 
#' @param fill A column name (backticks) in 'df'. This is the variable used to determine tile fill.
#' 
#' @param alpha Numeric(1). Significance threshold used to determine highlighted tiles.
#' 
#' @return A ggplot object.
#'
#' @examples
#' moderator_heatmap(results_df, predictor, target_construct, Q_mod_p, 0.05)
#' 
########################

moderator_heatmap <- function(df, x, y, fill, alpha) {
  
  h <- ggplot(df, aes(x = {{ x }}, y = {{ y }}, fill = {{ fill }} <= alpha)) +
    geom_tile(color = "grey80") +
    scale_fill_manual(
      values = c("TRUE" = "red", "FALSE" = "white"),
      guide = "none"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  return(h)
}

########################
#' Plot histograms for exploratory data analysis
#'
#' Creates histograms to visualize the distribution of a selected variable
#' within a dataset. Optionally facets the plot by a grouping variable.
#'
#' @param df A data.frame (or tibble) containing the dataset.
#'
#' @param var A column name (backticks) in `df` to visualize.
#'
#' @param bin_num Integer(1). Number of bins (default = 20).
#'
#' @param facet Character(1). Column name used for faceting (default = "target_construct").
#'
#' @return A ggplot object.
#'
#' @examples
#' exploratory_histogram(df, yi)

exploratory_histogram <- function(df, var, bin_num = 20, facet = "target_construct") {
  df %>%
    ggplot(aes(x = {{ var }})) +
    geom_histogram(bins = bin_num) +
    facet_wrap(vars(.data[[facet]]), scales = "free")
}

########################
#' Plot meta-regression coefficients
#'
#' Creates a forest-style plot of significant moderator coefficients.
#'
#' @param df A data.frame (or tibble) containing meta-regression results.
#' Must include columns corresponding to coefficient estimates, confidence intervals,
#' labels, and optionally a faceting variable.
#' 
#' @param beta A column name (backticks) in 'df'. This is the column containing coefficient estimates.
#' 
#' @param ci_lb A column name (backticks) in 'df'. This is the column containing the lower bound of the confidence interval.
#' 
#' @param ci_ub A column name (backticks) in 'df'. This is the column containing the upper bound of the confidence interval.
#' 
#' @param label A column name (backticks) in 'df'. This is the column containing the labels for each coefficient.
#' 
#' @param facet Character(1). Column name used for faceting.
#'
#' @param point_size Numeric(1). Size of the coefficient points.
#'
#' @param ci_height Numeric(1). Height of the confidence interval bars.
#' 
#' @param p_val Character(1). Name of column with the p_val you desire to visualize, by default set to P_adj_fdr
#'
#' @return A ggplot object.
#'
#' @examples
#' plot_betas(coef_df_only_sig, beta, ci_lb, ci_ub, label)
#' 
########################

plot_betas <- function(df,beta,ci_lb,ci_ub,label,facet = "target_construct",point_size = 3,ci_height = 0.2,p_val="P_adj_fdr"){

  beta_q <- enquo(beta)
  ci_lb_q <- enquo(ci_lb)
  ci_ub_q <- enquo(ci_ub)
  label_q <- enquo(label)
  p_val_q <- sym(p_val)
  
  ci_min <- min(pull(df, !!ci_lb_q), na.rm = TRUE)
  ci_max <- max(pull(df, !!ci_ub_q), na.rm = TRUE)
  x_span <- ci_max - ci_min
  
  if(!is.finite(x_span) || x_span == 0) x_span <- 1
  
  p <- ggplot(df,
              aes(x = {{ beta }},
                  y = reorder({{ label }}, {{ beta }}),
                  color = {{ beta }} > 0)) +
    geom_point(size = point_size) +
    geom_errorbarh(aes(xmin = {{ ci_lb }}, xmax = {{ ci_ub }}),
                   height = ci_height) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    scale_color_manual(values = c("TRUE" = "red", "FALSE" = "blue"),
                       guide = "none") +
    theme_minimal() +
  theme(
    panel.spacing.x = unit(2, "lines")  # THIS fixes left-right overlap
  )+
    geom_text(
      aes(x = !!ci_ub_q + 0.03*x_span,
          label = case_when(!!p_val_q < .001 ~ "***",
                            !!p_val_q < .01 ~ "**",
                            !!p_val_q < .05 ~ "*",
                            TRUE ~ "")),
      hjust = 0) +
    coord_cartesian(
      xlim = c(
        ci_min - 0.05*x_span,
        ci_max + 0.85*x_span   # increased so label fits (THIS IS THE MAGIC BUTTON FOR SPACING NOTE TO SELF)
      ),
      clip = "off"             #  allow drawing outside panel
    ) +
    facet_wrap(vars(.data[[facet]]), scales = "free")

  ref_df <- df %>%
    group_by(.data[[facet]]) %>%
    summarize(reference = first(reference), .groups = "drop")

  p <- p +
    geom_label(
      data = ref_df,
      aes(
        x = ci_max + 0.15 * x_span,
        y = Inf,
        label = paste("Ref:", reference)
      ),
      hjust = 0,
      vjust = 1.2,
      inherit.aes = FALSE,
      size = 3,
      fill = "white",
      color = "black",
      label.size = 0.25
    )

  return(p)
}

##############################################################################
#' Fit a univariate moderator model using metafor::rma
#'
#' Fits a meta-regression model for a specified subgroup and predictor.
#'
#' @param data A data.frame (or tibble) containing effect sizes, variances,
#' and moderator variables.
#'
#' @param group_identifier Character(1). Column name used to define subgroups.
#'
#' @param group Character(1). The subgroup level to filter on.
#'
#' @param predictor Character(1). Column name of the moderator.
#'
#' @param yi_col Character(1). Column name of the effect size (default = "yi").
#'
#' @param vi_col Character(1). Column name of the sampling variance (default = "vi").
#'
#' @return A list with:
#' \describe{
#'   \item{results}{Moderator-level test statistics}
#'   \item{coefs}{Coefficient-level estimates}
#' }
#'
#' @examples
#' fit_one_mod(df, "target_construct", "construct_a", "measurement_stage")
##############################################################################


fit_one_mod <- function(data,group_identifier,group,predictor,yi_col = "yi",vi_col = "vi"){
  
  current_subset <- data %>%
    filter(.data[[group_identifier]] == group)
  
  formula_mod <- as.formula(paste0("~ `", predictor, "`"))
  
  if (is.factor(current_subset[[predictor]])) {
    current_subset[[predictor]] <- droplevels(current_subset[[predictor]])
    ref_level <- levels(current_subset[[predictor]])[1]
  } else {
    ref_level <- NA_character_
  }
  
  model <- rma(
    yi = current_subset[[yi_col]],
    vi = current_subset[[vi_col]],
    mods = formula_mod,
    data = current_subset,
    method = "REML"
  )
  
  results_row <- tibble(
    target_construct = group,
    predictor = predictor,
    Q_mod = model$QM,
    Q_df = model$m,
    Q_mod_p = model$QMp
  )
  
  coef_tbl <- tibble(
    target_construct = group,
    predictor = predictor,
    reference = ref_level,
    term = rownames(model$beta),
    beta = as.numeric(model$beta),
    se = model$se,
    z = model$zval,
    p = model$pval,
    ci_lb = model$ci.lb,
    ci_ub = model$ci.ub
  )
  
  list(results = results_row, coefs = coef_tbl)
}
########################
#' Recode categorical variables using user-defined mappings
#'
#' Applies recoding rules to specified variables and converts them to factors.
#'
#' @param df A data.frame (or tibble).
#'
#' @param recode_maps Named list of named vectors defining recoding rules
#' for each variable.
#'
#' @return A data.frame with recoded categorical variables.
########################

map_categorical <- function(df, recode_maps) {
  
  for (var in names(recode_maps)) {
    
    map <- recode_maps[[var]]
    
    df[[var]] <- map[as.character(df[[var]])]
    
    df[[var]] <- factor(df[[var]])

  }
  
  
return(df)
  
}

########################
#' Run moderator meta-regressions across predictors and groups
#'
#' Fits a series of univariate meta-regression models using metafor::rma()
#' across all combinations of grouping levels and predictor variables.
#'
#' @param data A data.frame (or tibble) containing effect sizes (`yi`),
#' variances (`vi`), a grouping variable, and moderator variables.
#'
#' @param group_identifier Character(1). Column name defining grouping structure
#' (default = "target_construct").
#'
#' @param yi_col Character(1). Column name of effect sizes.
#'
#' @param vi_col Character(1). Column name of sampling variances.
#'
#' @return A list with:
#' \describe{
#'   \item{results}{Moderator-level statistics (QM, df, p-values)}
#'   \item{coefs}{Coefficient-level results}
#' }
#'
#' @examples
#' model_output <- run_meta_moderators(df)
########################

run_meta_moderators <- function(data,group_identifier = "target_construct",yi_col = "yi",vi_col = "vi"){
  
  predictors <- colnames(data %>%
    select(-all_of(c(group_identifier, yi_col, vi_col))))
  
  groups <- data %>%
    pull(all_of(group_identifier)) %>%
    unique()
  
  grid <- crossing(target_construct = groups, predictor = predictors)
  
  fits <- pmap(grid, function(target_construct, predictor) {
    
    tryCatch(
      fit_one_mod(data, group_identifier, target_construct, predictor,
                  yi_col = yi_col, vi_col = vi_col),
      error = function(e) NULL
    )
    
  })
  
  fits <- compact(fits)
  
  results_df <- bind_rows(map(fits, "results"))
  coef_df <- bind_rows(map(fits, "coefs"))
  
  # Return both outputs
  return(list(results = results_df,coefs = coef_df))
  
}

########################
#' Compute effect sizes and sampling variances
#'
#' Uses metafor::escalc() to compute standardized mean differences (SMD)
#' and corresponding sampling variances from group-level summary statistics.
#'
#' @param df A data.frame (or tibble).
#'
#' @param m1i Column name (backticks). Group A mean.
#' @param sd1i Column name (backticks). Group A standard deviation.
#' @param n1i Column name (backticks). Group A sample size.
#'
#' @param m2i Column name (backticks). Group B mean.
#' @param sd2i Column name (backticks). Group B standard deviation.
#'
#' @param n2i Column name (backticks). Group B sample size.
#'
#' @param analysis_col Column name representing grouping variable.
#'
#' @param study_year_col Column name representing time or study index (optional).
#'
#' @param numeric_cols Character vector of columns to coerce to numeric.
#'
#' @param measure Character(1). Effect size measure (default = "SMD").
#'
#' @return A data.frame with computed `yi` and `vi`.
#'
#' @examples
#' df_es <- compute_meta_effect_sizes(
#'   df,
#'   m1i = group_a_mean,
#'   sd1i = group_a_sd,
#'   n1i = group_a_n,
#'   m2i = group_b_mean,
#'   sd2i = group_b_sd,
#'   n2i = group_b_n,
#'   analysis_col = target_construct
#' )
########################

compute_meta_effect_sizes <- function(df,m1i, sd1i, n1i,m2i, sd2i, n2i,analysis_col,study_year_col,numeric_cols = NULL,measure = "SMD") {
  
  #represent them as symbols so its helpful for escalc to grab it from the functions

  
  #convert to numeric first so that escalc can easily use our function 
  df <- df %>%
    mutate(across(all_of(numeric_cols), as.numeric))

  #Run escalc quick and use tidy eval instead here to 
  df_escalc<-escalc(
    data=df,
    measure = measure,
    m1i = df[[m1i]],
    sd1i = df[[sd1i]],
    n1i = df[[n1i]],
    m2i = df[[m2i]],
    sd2i = df[[sd2i]],
    n2i = df[[n2i]]
  )
  
  return(df_escalc)
  }

########################
#' Map factor term codes back to readable labels.
#'
#' Joins coefficient term codes to a lookup table and creates a readable
#' combined label for plotting and output tables.
#'
#' @param coef_df A data.frame (or tibble) containing coefficient-level output.
#' Must include columns `predictor` and `term`.
#'
#' @param lookup_df A data.frame (or tibble) containing columns `predictor`,
#' `code`, and `label`.
#'
#' @return A tibble with relabeled `term` and a new `label` column.
#'
#' @examples
#' coef_df <- apply_term_labels(coef_df, lookup_df_new)
########################

apply_term_labels <- function(coef_df,lookup_df){
  coef_df %>%
    mutate(code = str_remove(term,paste0("^",predictor))) %>%
    left_join(lookup_df,by=c("predictor","code")) %>%
    mutate(
      term = coalesce(label,term),
      Label = paste(predictor, term, sep=": ")
    ) %>%
    select(-label)%>%
    left_join(
    lookup_df %>% select(predictor, code, label),
    by = c("predictor", "reference" = "code")
  ) %>%
  mutate(reference = coalesce(label, reference)) %>%
  select(-label)
}

########################
#' Save moderator significance heatmaps.
#'
#' Creates and saves raw, FDR-adjusted, and Holm-adjusted moderator heatmaps.
#'
#' @param results_df A data.frame of raw moderator test results.
#' @param results_df_adjusted A data.frame of adjusted moderator test results.
#' @param out_dir Character(1). Output directory for saved heatmaps.
#'
#' @return Invisibly saves three files to disk.
#'
#' @examples
#' make_heatmaps(results_df, results_df_adjusted, "~/plots")
########################

make_heatmaps <- function(results_df, out_dir){
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  raw_p_heatmap <- moderator_heatmap(results_df, predictor, target_construct, Q_mod_p, 0.05)
  ggsave(file.path(out_dir,"heatmap_raw_sig_pval.svg"), raw_p_heatmap, width = 16, height = 7)
  
  fdr_adj_p_heatmap <- moderator_heatmap(results_df, predictor, target_construct, P_adj_fdr, 0.05)
  ggsave(file.path(out_dir,"heatmap_adjusted_fdr_sig_pval.svg"), fdr_adj_p_heatmap, width = 16, height = 7)
  
  holm_adj_p_heatmap <- moderator_heatmap(results_df, predictor, target_construct, P_adj_holm, 0.05)
  ggsave(file.path(out_dir,"heatmap_adjusted_holm_sig_pval.svg"), holm_adj_p_heatmap, width = 16, height = 7)
}




########################################################################
#' Run full meta-analysis workflow
#'
#' Applies categorical recoding, computes effect sizes, performs optional
#' exploratory data analysis, and fits moderator meta-regression models.
#'
#' @param df A data.frame (or tibble).
#'
#' @param recode_map List defining recoding rules for categorical variables.
#'
#' @param out_dir Character(1). Output directory.
#'
#' @param dataset_name Character(1). Name used for organizing outputs.
#'
#' @param m1i,sd1i,n1i,m2i,sd2i,n2i Column names for group summary statistics.
#'
#' @param group_col Character(1). Grouping variable (default = "target_construct").
#'
#' @param numeric_cols Character vector of columns to coerce to numeric.
#'
#' @param drop_cols Character vector of columns to remove.
#'
#' @param run_eda Logical. Whether to generate histograms.
#'
#' @param bin_num Integer(1). Number of histogram bins.
#'
#' @param yi_col,vi_col Character(1). Effect size and variance columns.
#'
#' @return A list containing:
#' \describe{
#'   \item{df_es}{Processed dataset}
#'   \item{model_output}{Meta-regression results}
#' }
#'
#' @examples
#' out <- run_meta_pipeline(df, recode_map, "outputs", "demo",
#'   m1i = group_a_mean, sd1i = group_a_sd, n1i = group_a_n,
#'   m2i = group_b_mean, sd2i = group_b_sd, n2i = group_b_n
#' )
########################################################################

run_meta_pipeline <- function(df,recode_map,out_dir,dataset_name,m1i, sd1i, n1i,m2i, sd2i, n2i,group_col = "target_construct",numeric_cols = NULL,drop_cols = c("source_id","group_a_mean","group_a_sd","group_a_n","group_b_mean","group_b_sd","group_b_n","study_weight","ci_lower","ci_upper"),run_eda = TRUE,bin_num = 20,yi_col = "yi",vi_col = "vi") {
  
  # 1. Recode categorical variables
  df_recoded <- map_categorical(df, recode_map)
  
  # 2. Compute effect sizes
  df_es <- compute_meta_effect_sizes(
    df_recoded,
    m1i = m1i,
    sd1i = sd1i,
    n1i = n1i,
    m2i = m2i,
    sd2i = sd2i,
    n2i = n2i,
    analysis_col = group_col,
    study_year_col = NULL,
    numeric_cols = numeric_cols
  )
  
  if (!is.null(drop_cols)) {
    df_es <- df_es %>% select(-any_of(drop_cols))
  }
    
  # 3. EDA
  if (run_eda) {
    
    ggsave(file.path("Exploratory_Histograms", "yi_hist.svg"),exploratory_histogram(df_es, !!sym(yi_col), bin_num, group_col),width = 16, height = 7,create.dir = TRUE)
    
    ggsave(file.path("Exploratory_Histograms", "vi_hist.svg"),exploratory_histogram(df_es, !!sym(vi_col), bin_num, group_col),width = 16, height = 7,create.dir = TRUE)
  }
  
  # 4. Run models
  model_output <- run_meta_moderators(
    df_es,
    group_identifier = group_col,
    yi_col = yi_col,
    vi_col = vi_col
  )
  
  # Return everything cleanly
  return(list(
    df_es = df_es,
    model_output = model_output
  ))
}

########################
#' Add multiple testing corrections to p-values
#'
#' Applies multiple testing correction methods (e.g., FDR, Holm)
#' within each group.
#'
#' @param df A data.frame (or tibble) containing p-values.
#'
#' @param p_col Character(1). Column containing p-values.
#'
#' @param methods Character vector of adjustment methods.
#'
#' @param group_col Character(1). Column used for grouping.
#'
#' @return A data.frame with adjusted p-value columns.
#'
#' @examples
#' add_p_adjustments(df, "Q_mod_p", c("fdr","holm"), "target_construct")########################

add_p_adjustments <- function(df, p_col, methods, group_col = "target_construct") {

  df %>%
    group_by(.data[[group_col]]) %>%
    group_modify(~ {
      
      for (m in methods) {
        .x[[paste0("P_adj_", m)]] <- p.adjust(.x[[p_col]], method = m)
      }
      
      .x
    }) %>%
    ungroup()

}

########################
#' Apply multiple testing corrections and relabel coefficients
#'
#' Applies p-value adjustments to both moderator-level and coefficient-level
#' results, and maps coefficient term codes to readable labels.
#'
#' @param model_output A list output from `run_meta_pipeline()` containing
#' nested `model_output` with `results` and `coefs`.
#'
#' @param lookup_df A data.frame (or tibble) containing term label mappings.
#'
#' @param methods Character(1). Adjustment methods to apply
#' (default = c("fdr","holm")).
#'
#' @return A modified list with adjusted p-values in both `results` and `coefs`,
#' and relabeled coefficient terms.
#'
#' @examples
#' updated_output <- multiple_testing_and_remapping_labels(
#'   model_output,
#'   lookup_df
#' )
########################

multiple_testing_and_remapping_labels <- function(model_output,lookup_df,methods = c("fdr", "holm"),results_p_col = "Q_mod_p",coef_p_col = "P") {

  out <- model_output
  mo <- out$model_output

  # adjust p-values
  mo$results <- add_p_adjustments(mo$results, "Q_mod_p", methods)
  mo$coefs   <- add_p_adjustments(mo$coefs, "p", methods)

  # apply labels ONLY to coefs
  mo$coefs <- apply_term_labels(mo$coefs, lookup_df)

  # put it back
  out$model_output <- mo

  return(out)
}

########################################################################
#' Save forest plot for a single group
#'
#' Generates and saves a forest-style plot of moderator coefficients
#' for a specified group.
#'
#' @param df A data.frame containing coefficient results.
#'
#' @param analysis_name Character(1). Group identifier.
#'
#' @param prefix Character(1). Output filename prefix.
#'
#' @param out_dir Character(1). Output directory.
#'
#' @param desired_p_val Character(1). Column name for p-value annotation.
#'
#' @return Saves plot to disk (invisibly).
#'
#' @examples
########################################################################

plot_forest_analysis <- function(df, analysis_name, prefix, out_dir, desired_p_val) {

  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)  # 👈 KEY LINE

  df_sub <- df %>%
    filter(
      target_construct == analysis_name,
      term != "intrcpt",
      term != "Reference"
    )

  if (nrow(df_sub) == 0) return(NULL)

  p <- plot_betas(df_sub,beta, ci_lb, ci_ub, Label,facet = "predictor",p_val = desired_p_val) +
    labs(title = paste0(analysis_name, " (FDR adjusted)"))

  file_name <- gsub("[^[:alnum:]]","_", analysis_name)

  ggsave(
    filename = file.path(out_dir, paste0(prefix, "_", file_name, ".svg")),
    plot = p,
    width = 18,
    height = 8,
    dpi = 300
  )
}

########################################################################
#' Generate synthetic meta-analytic dataset
#'
#' Creates a fully synthetic dataset that mimics the structure of a
#' meta-analytic dataset, including group-level summary statistics,
#' effect size placeholders, and moderator variables.
#'
#' This dataset is intended for testing, demonstration, and reproducible
#' workflows without using real or identifiable data.
#'
#' @param n Integer(1). Number of rows to generate (default = 200).
#'
#' @return A tibble containing synthetic meta-analytic data.
#'
#' @examples
#' df <- generate_synthetic_meta(200)
########################################################################

generate_synthetic_meta <- function(n = 200) {
  set.seed(123)

  df <- tibble(
    target_construct = sample(c("construct_a","construct_b","construct_c"), n, TRUE),
    source_id = paste0("src_", seq_len(n)),

    # continuous
    measurement_stage = runif(n, 0, 5),
    contact_count = sample(1:12, n, TRUE),

    # categorical
    intensity_band = sample(c("tier_1","tier_2","tier_3"), n, TRUE),
    contact_duration_band = sample(c("brief","moderate","extended"), n, TRUE),

    # sample sizes + SDs
    group_a_n = sample(30:250, n, TRUE),
    group_b_n = sample(30:250, n, TRUE),
    group_a_sd = runif(n, 8, 18),
    group_b_sd = runif(n, 8, 18)
  ) %>%
    mutate(
      # baseline control group
      group_b_mean = rnorm(n, 50, 10),

      # structured effect
      effect =
        case_when(
          target_construct == "construct_a" ~ 0.2,
          target_construct == "construct_b" ~ 0.05,
          TRUE ~ -0.1
        ) +
        if_else(intensity_band == "tier_3", 0.15, 0) +
        if_else(contact_duration_band == "extended", 0.1, 0) +
        0.01 * contact_count -
        0.01 * measurement_stage +
        rnorm(n, 0, 0.2),

      pooled_sd = (group_a_sd + group_b_sd)/2,

      # treatment group reflects effect
      group_a_mean = group_b_mean + effect * pooled_sd,

      # 🔥 NOW derive meta quantities (not random anymore)
      yi = effect,

      vi = (group_a_n + group_b_n) / (group_a_n * group_b_n) +
           (yi^2 / (2 * (group_a_n + group_b_n))),

      ci_lower = yi - 1.96 * sqrt(vi),
      ci_upper = yi + 1.96 * sqrt(vi),

      study_weight = 1 / vi
    ) %>%
    select(-effect, -pooled_sd)

  df
}