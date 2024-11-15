run_associations <- function(clinical_data_df, clinical_data_cols, outcome_var_colname = "subgroup",
                             adjust_by =c("Age_dx + male.female_dx"), plot = FALSE, y.axis.text.size = 12,
                             pval.label.size = 3, ntop = 10, order = FALSE, adjust_p = FALSE){

  # Retrieve clinical manifestation names
  if (class(clinical_data_cols) == "numeric"){
    clinical_manifestations = colnames(clinical_data)[c(14, 27, 28, 31:36, 38:40, 48:55, 57, 59:63, 66:67, 70:72, 92)] # Select the names of the columns that contain clinical manifestations.
  } else {
    clinical_manifestations = as.character(clinical_data_cols)
  }

  # Which columns should be factors?
  factor_cols = c(clinical_manifestations, outcome_var_colname)

  # Check that all columns involved as treated as factor
  clinical_data_df[factor_cols] <- lapply(clinical_data_df[factor_cols], function(column) {
    if (!is.factor(column)) {
      return(as.factor(column))  # Convert to factor if not already a factor
    } else {
      return(column)  # Leave as is if already a factor
    }
  })

  # Check that response outcome variable exists in dataset
  if (!all(clinical_manifestations %in% colnames(clinical_data_df))){
    missing_manifest <- clinical_manifestations[!clinical_manifestations %in% colnames(clinical_data_df)]
    stop(sprintf("Clinical manifestation: %s not found in the dataframe.\n", missing_manifest))
  }

  # Check that the outcome variable is present in the dataframe
  if (!outcome_var_colname %in% colnames(clinical_data_df)){
    stop(sprintf("A column with outcome variable  %s was not found on dataframe", outcome_var_colname))
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
      # Create formula dynamically
      message(sprintf("Computing %s for %s", group, clin_manifest))
      try({
        formula <- as.formula(sprintf("%s~%s%s + %s", clin_manifest, outcome_var_colname, group, adjust_by))
        # Fit the model and store it in the list
        model <- glm(formula, family = binomial(link = "logit"), data = clinical_data_df)
        # Save model in list
        model_ominibus_list[[group]][[clin_manifest]] <- model

        # Extract OR, 95% CI, p-values, clinical manifest, and subgroup.
        or_ci <- exp(cbind(OR = coef(model), confint(model)))
        coeff_pval_df = data.frame(summary(model)$coefficients)
        colnames(coeff_pval_df) <- c("Estimate", "Std_error", "z.value", "Pr>|z|")
        or_ci_df <- as.data.frame(cbind(or_ci, coeff_pval_df))
        or_ci_df$clinical_manifestation <- clin_manifest
        or_ci_df$predictor <- rownames(or_ci_df)
        or_ci_df[[outcome_var_colname]] <- group

        # calculate anova
        anova <- anova(model, test="Chisq")
        anova_list[[group]][[clin_manifest]] <- anova

        # Bind results to the main results data frame
        results_df <- bind_rows(results_df, or_ci_df)

      }, silent = TRUE)
    }
  }
  # If order, arrange by pvalue
  if (order){
    results_df <- results_df %>%
      arrange(`Pr>|z|`)
  }

  # Add results to list
  results_list <- list(models = model_ominibus_list,
                       anova = anova_list,
                       summarized_results = results_df)

  if (plot){
    message("Making forest plot...")
    # Take only top results specified by user
    top_results <- results_df[0:ntop, ] %>%
      arrange(-OR)
    #`outcome_var_colname`, `groups`

    # Categorize results
    top_results$category <- with(top_results, ifelse(`Pr>|z|` < 0.05 & OR > 1, "Significant (OR > 1)",
                                                     ifelse(`Pr>|z|` > 0.05 & OR < 1, "Not Significant (OR < 1)",
                                                            ifelse(`Pr>|z|` < 0.05 & OR < 1, "p-value Significant (OR < 1)",
                                                                   "OR Significant (p-value > 0.05)"))))

    forest <- ggplot(top_results, aes(x = OR, y = reorder(paste(predictor, clinical_manifestation, .data[[outcome_var_colname]], sep = " - "), OR))) +
      geom_errorbarh(aes(xmin = `2.5 %`, xmax = `97.5 %`), height = 0.2, color = "black") +  # Confidence intervals
      geom_point(aes(color = category), size = 2) +                                       # OR points, colored by OR > 1
      scale_color_manual(
        values = c(
          "Significant (OR > 1)" = "#870052",
          "Not Significant (OR < 1)" = "#4DB5BC",
          "p-value Significant (OR < 1)" = "#FF8C00",
          "OR Significant (p-value > 0.05)" = "#4682B4"
        ),
        name = "Category"
      ) +               # Customize colors
      labs(
        x = "Odds Ratio (OR)",
        y = "Variables",
        title = "Forest Plot of Associations",
        caption = "Confidence intervals represent 95% CI"
      ) +

      # Display p-value without scientific notation
      geom_text(aes(label = sprintf("p = %.4f", `Pr>|z|`)), hjust = -0.1, vjust = -0.7, color = "gray40", size = pval.label.size) +

      # Add minimal theme with border and grid lines
      theme_minimal() +
      theme(
        panel.border = element_rect(color = "black", fill = NA, size = 0.5),   # Adds a border around the plot area
        panel.grid.major.x = element_line(color = "gray80", linetype = "dashed"),  # Dashed vertical grid lines
        panel.grid.major.y = element_blank(),                                     # Remove horizontal grid lines for clarity
        legend.position = "right",
        legend.text = element_text(size = y.axis.text.size-2),
        legend.title = element_text(size = y.axis.text.size),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        axis.text.y = element_text(size = y.axis.text.size)
      ) +

      # Adjust x-axis limits for padding around CI range
      coord_cartesian(xlim = c(min(top_results$`2.5 %`) - 2, max(top_results$`97.5 %`) + 1))

    results_list$forest_plot <- forest
  }
  message("DONE!")
  return(results_list)
}
