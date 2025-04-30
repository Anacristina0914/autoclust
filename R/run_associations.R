run_associations <- function(clinical_data_df, clinical_data_cols, outcome_var_colname = "subgroup",
                             adjust_by ="Age_dx + male.female_dx", family = binomial, link = "logit", plot = FALSE, y.axis.text.size = 12,
                             or.label.size = 3, ntop = 10, order = FALSE, adjust_p = TRUE, p_adj_method = "BH", add_xylabs = FALSE, name_mapping = NULL,
                             x.axis.text.size = 8, pt.size = 2, filter_sig_OR = TRUE, forest_plot_title = "Associations"){

  # Retrieve clinical manifestation names
  if (is(clinical_data_cols, "numeric")){
    clinical_manifestations = colnames(clinical_data)[clinical_data_cols] # Select the names of the columns that contain clinical manifestations.
  } else {
    clinical_manifestations = as.character(clinical_data_cols)
  }

  #print(c(clinical_manifestations, outcome_var_colname))
  # Which columns should be factors?
  factor_cols = c(clinical_manifestations, outcome_var_colname)

  # Check which columns exist
  adjustment_vars <- unlist(strsplit(adjust_by, "\\+")) # Split by "+"
  adjustment_vars <- trimws(adjustment_vars) # Remove extra spaces
  required_vars <- unique(c(clinical_manifestations, outcome_var_colname, adjustment_vars))
  missing_required <- setdiff(required_vars, colnames(clinical_data_df))
  if (length(missing_required) > 0){
    stop(sprintf("The following required variables are missing from the dataframe: %s", paste(missing_required, collapse = ", ")))
  }


  # Check that all columns involved as treated as factor (only for log regression)
  if (identical(family, binomial)){
    clinical_data_df[factor_cols] <- lapply(clinical_data_df[factor_cols], function(column) {
      if (!is.factor(column)) {
        return(as.factor(column))  # Convert to factor if not already a factor
      } else {
        return(column)  # Leave as is if already a factor
      }
    })
  }


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
      try({
        message(sprintf("Computing %s for %s", group, clin_manifest))
        formula <- as.formula(sprintf("`%s` ~ %s%s + %s",
                                      clin_manifest,
                                      outcome_var_colname,
                                      group,
                                      adjust_by))

        # Fit the model and store it in the list
        model <- glm(formula,
                     family = family(link = link),
                     data = clinical_data_df)
        message("Finished computing model!")
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

      }, silent = FALSE)
    }
  }
  # If order, arrange by pvalue
  if (order){
    results_df <- results_df %>%
      arrange(`Pr>|z|`)
  }

  if (adjust_p) {
    if (!"Pr>|z|" %in% colnames(results_df)) {
      stop("Column 'Pr>|z|' not found in results_df; cannot adjust p-values.")
    }
    #results_df$`Pr>|z|` <- p.adjust(results_df$`Pr>|z|`, method = p_adj_method)
    results_df$adj_p <- p.adjust(results_df$`Pr>|z|`, method = p_adj_method)
    results_df$adj_p_method <- p_adj_method
  }


  # Add results to list
  results_list <- list(models = model_ominibus_list,
                       anova = anova_list,
                       summarized_results = results_df,
                       df = clinical_data_df)


  if (plot){
    message("Making forest plot...")
    forest <- make_forest_plot(results_df = results_df, ntop = ntop, clinical_manifestation = clinical_manifestation,
                     outcome_var_colname = outcome_var_colname, pt.size = pt.size, y.axis.text.size = y.axis.text.size,
                     x.axis.text.size = x.axis.text.size, name_mapping = name_mapping, adjust_vars = adjustment_vars,
                     add_xylabs = add_xylabs, or.label.size = or.label.size, plot_title = forest_plot_title,
                     filter_sig_OR = filter_sig_OR, adjust_p = adjust_p, p_adj_method = p_adj_method)
    results_list$forest_plot <- forest
  }

  message("DONE!")
  return(results_list)
}

