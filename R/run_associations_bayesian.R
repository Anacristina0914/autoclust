run_associations_bayesian <- function(clinical_data_df, clinical_data_cols, outcome_var_colname = "subgroup",
                             adjust_by = c("Age_dx + male.female_dx"), plot = FALSE, ntop = 10,
                             order = FALSE, adjust_p = FALSE, bayesian = FALSE,
                             prior = normal(0, 2.5), prior_intercept = normal(0, 5), chains = 4,
                             iter = 2000, warmup = 1000, thin = 1, seed = 123){

  # Retrieve clinical manifestation names
  if (class(clinical_data_cols) == "numeric"){
    clinical_manifestations = colnames(clinical_data)[c(14, 27, 28, 31:36, 38:40, 48:55, 57, 59:63, 66:67, 70:72, 92)] # Select the names of the columns that contain clinical manifestations.
  } else {
    clinical_manifestations = as.character(clinical_data_cols)
  }

  # Check that response outcome variable exists in dataset
  if (!all(clinical_manifestations %in% colnames(clinical_data_df))){
    stop(sprintf("A column with outcome variable %s was not found on dataframe", outcome_var_colname))
  }

  groups <- as.character(unique(clinical_data_df[[outcome_var_colname]]))

  # Add columns based on response variable membership
  for (group in groups){
    clinical_data_df[[paste0(outcome_var_colname, group)]] = ifelse(clinical_data_df[[outcome_var_colname]] == group, 1, 0)
    clinical_data_df[[paste0(outcome_var_colname, group)]] = as.factor(clinical_data_df[[paste0(outcome_var_colname, group)]])
  }

  model_ominibus_list <- list()
  anova_list <- list()
  summary_list <- list()
  results_df <- data.frame()

  # Iterate over all clinical manifestations
  for (clin_manifest in clinical_manifestations){
    for (group in groups){
      message(sprintf("Computing %s for %s", group, clin_manifest))
      try({
        formula <- as.formula(sprintf("%s ~ %s%s + %s", clin_manifest, outcome_var_colname, group, adjust_by))

        if (bayesian) {
          message("Running bayesian methods...")
          # Bayesian logistic regression using rstanarm::stan_glm
          model <- stan_glm(formula, family = binomial(link = "logit"), data = clinical_data_df,
                            prior = prior, prior_intercept = prior_intercept, chains = chains,
                            iter = iter, warmup = warmup, thin = thin, seed = seed)

          # Extracting posterior estimates for OR and 95% credible intervals
          posterior <- as.data.frame(posterior_interval(model, prob = 0.95))
          or_ci <- exp(cbind(OR = posterior[, "50%"], `2.5 %` = posterior[, "2.5%"], `97.5 %` = posterior[, "97.5%"]))

          # Creating a summary for the coefficients
          coeff_summary <- as.data.frame(summary(model)$coefficients)
          colnames(coeff_summary) <- c("Estimate", "Std_error", "Pr>|z|")  # Posterior summary doesn't give p-values

        } else {
          # Standard logistic regression using glm
          model <- glm(formula, family = binomial(link = "logit"), data = clinical_data_df)

          # Extract OR, 95% CI, p-values
          or_ci <- exp(cbind(OR = coef(model), confint(model)))
          coeff_summary <- as.data.frame(summary(model)$coefficients)
          colnames(coeff_summary) <- c("Estimate", "Std_error", "z.value", "Pr>|z|")
        }

        # Combine results into a data frame
        or_ci_df <- as.data.frame(or_ci)
        or_ci_df$clinical_manifestation <- clin_manifest
        or_ci_df$predictor <- rownames(or_ci_df)
        or_ci_df[[outcome_var_colname]] <- group

        # Append results to the main data frame
        results_df <- bind_rows(results_df, or_ci_df)

      }, silent = TRUE)
    }
  }

  # If order, arrange by pvalue or by OR magnitude for Bayesian
  if (order){
    if (bayesian) {
      results_df <- results_df %>% arrange(desc(abs(OR)))
    } else {
      results_df <- results_df %>% arrange(`Pr>|z|`)
    }
  }

  # Create output list
  results_list <- list(models = model_ominibus_list, summarized_results = results_df)

  if (plot){
    message("Making forest plot...")
    # Forest plot code remains largely the same
    # Top results selection
    top_results <- results_df[0:ntop, ] %>%
      arrange(-OR)

    forest <- ggplot(top_results, aes(x = OR, y = reorder(paste(predictor, clinical_manifestation, `outcome_var_colname`, `groups`, sep = " - "), OR))) +
      geom_errorbarh(aes(xmin = `2.5 %`, xmax = `97.5 %`), height = 0.2, color = "black") +  # Confidence intervals
      geom_point(aes(color = OR > 1), size = 2) +                                       # OR points, colored by OR > 1
      scale_color_manual(values = c("TRUE" = "#870052", "FALSE" = "#4DB5BC")) +               # Customize colors
      labs(
        x = "Odds Ratio (OR)",
        y = "Variables",
        title = "Forest Plot of Associations",
        caption = "Confidence intervals represent 95% CI"
      ) +
      geom_text(aes(label = sprintf("p = %.4f", `Pr>|z|`)), hjust = -0.1, vjust = -0.7, color = "gray40", size = 2) +
      theme_minimal() +
      theme(
        panel.border = element_rect(color = "black", fill = NA, size = 0.5),   # Adds a border around the plot area
        panel.grid.major.x = element_line(color = "gray80", linetype = "dashed"),  # Dashed vertical grid lines
        panel.grid.major.y = element_blank(),                                     # Remove horizontal grid lines for clarity
        legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5)
      ) +
      coord_cartesian(xlim = c(min(top_results$`2.5 %`) - 0.5, max(top_results$`97.5 %`) + 0.5))

    results_list$forest_plot <- forest
  }
  message("DONE!")
  return(results_list)
}
