#' Run Association Analyses Between Clinical Manifestations (outcome) and a predictor variable (e.g. patient subgroup)
#'
#' This function performs a series of (generalized) linear regression models to test associations
#' between one or more clinical manifestations and a categorical predictor variable (e.g., subgroup or cluster),
#' optionally adjusting for covariates such as age and sex. It supports logistic regression (default)
#' and returns model objects, ANOVA tables, and a combined summary of odds ratios, confidence intervals,
#' and p-values. Optionally, it can generate a forest plot of the top associations.
#'
#' @param clinical_data_df A data frame containing clinical variables (outcomes), the predictor variable,
#'   and optional adjustment covariates.
#' @param clinical_data_cols Character or numeric vector specifying the column names or indices
#'   of the clinical manifestations (outcome variables) to test.
#' @param predictor_var_colname Character string specifying the name of the predictor variable column
#'   (default: `"subgroup"`).
#' @param adjust_by Character string defining adjustment variables in formula format (e.g., `"Age_dx + male.female_dx"`).
#'   Can include multiple covariates separated by `"+"`.
#' @param family A family function for the regression (default: `binomial`).
#' @param link Character string specifying the link function (default: `"logit"`).
#' @param plot Logical; if `TRUE`, generates a forest plot of the top associations (default: `FALSE`).
#' @param y.axis.text.size Numeric; text size for the y-axis in the forest plot (default: `12`).
#' @param or.label.size Numeric; text size for the odds ratio labels in the forest plot (default: `3`).
#' @param ntop Integer; number of top associations to include in the forest plot (default: `10`).
#' @param order Logical; if `TRUE`, orders the output data frame by p-value (default: `FALSE`).
#' @param adjust_p Logical; whether to perform multiple testing correction (default: `TRUE`).
#' @param p_adj_method Character; method for p-value adjustment (default: `"BH"` for Benjamini–Hochberg).
#' @param add_xylabs Logical; whether to add x and y axis labels to the forest plot (default: `FALSE`).
#' @param name_mapping Optional named vector or data frame for mapping variable names to
#'   human-readable labels in the plot (default: `NULL`).
#' @param x.axis.text.size Numeric; text size for the x-axis in the forest plot (default: `8`).
#' @param pt.size Numeric; point size in the forest plot (default: `2`).
#' @param filter_sig_OR Logical; if `TRUE`, filters forest plot to only show significant associations
#'   (based on adjusted p-values) (default: `TRUE`).
#' @param forest_plot_title Character string; title for the forest plot (default: `"Associations"`).
#'
#' @details
#' For each clinical manifestation and each level of the predictor variable, the function fits a model of the form:
#' \deqn{outcome ~ predictor_level + adjustment_covariates}
#' where `predictor_level` is a binary variable indicating membership in a given subgroup.
#' Odds ratios, 95% confidence intervals, and p-values are extracted from each model.
#' P-values can optionally be adjusted for multiple testing using the specified method.
#'
#' @return A named list containing:
#' \itemize{
#'   \item \code{models} — Nested list of fitted model objects per group and outcome.
#'   \item \code{anova} — Nested list of ANOVA tables for each model.
#'   \item \code{summarized_results} — Combined data frame of model estimates, odds ratios,
#'         confidence intervals, p-values, and (if enabled) adjusted p-values.
#'   \item \code{df} — Input data frame with additional binary indicator columns for each predictor group.
#'   \item \code{forest_plot} — (Optional) ggplot object with a forest plot of top associations.
#' }
#'
#' @examples
#' \dontrun{
#' results <- run_associations(
#'   clinical_data_df = lupus_df,
#'   clinical_data_cols = c("nephritis", "arthritis", "rash"),
#'   predictor_var_colname = "cluster",
#'   adjust_by = "Age_dx + sex",
#'   plot = TRUE,
#'   ntop = 15
#' )
#' }
#'
#' @seealso [glm()], [p.adjust()], [make_forest_plot()]
#' @importFrom stats as.formula binomial coef confint dist glm p.adjust rbinom reorder rnorm setNames
#' @export



run_associations <- function(clinical_data_df, clinical_data_cols, predictor_var_colname = "subgroup",
                             adjust_by ="Age_dx + male.female_dx", family = binomial, link = "logit", plot = FALSE, y.axis.text.size = 12,
                             or.label.size = 3, ntop = 10, order = FALSE, adjust_p = TRUE, p_adj_method = "BH", add_xylabs = FALSE, name_mapping = NULL,
                             x.axis.text.size = 8, pt.size = 2, filter_sig_OR = TRUE, forest_plot_title = "Associations"){

  # Retrieve clinical manifestation names
  if (is(clinical_data_cols, "numeric")){
    clinical_manifestations = colnames(clinical_data)[clinical_data_cols] # Select the names of the columns that contain clinical manifestations.
  } else {
    clinical_manifestations = as.character(clinical_data_cols)
  }

  #print(c(clinical_manifestations, predictor_var_colname))
  # Which columns should be factors?
  factor_cols = c(clinical_manifestations, predictor_var_colname)

  # Check which columns exist
  adjustment_vars <- unlist(strsplit(adjust_by, "\\+")) # Split by "+"
  adjustment_vars <- trimws(adjustment_vars) # Remove extra spaces
  required_vars <- unique(c(clinical_manifestations, predictor_var_colname, adjustment_vars))
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


  # Check that response predictor variable exists in dataset
  if (!all(clinical_manifestations %in% colnames(clinical_data_df))){
    missing_manifest <- clinical_manifestations[!clinical_manifestations %in% colnames(clinical_data_df)]
    stop(sprintf("Clinical manifestation: %s not found in the dataframe.\n", missing_manifest))
  }

  # Check that the predictor variable is present in the dataframe
  if (!predictor_var_colname %in% colnames(clinical_data_df)){
    stop(sprintf("A column with predictor variable  %s was not found on dataframe", predictor_var_colname))
  }

  groups <- as.character(unique(clinical_data_df[[predictor_var_colname]]))
  # Add columns based on response variable membership
  for (group in groups){
    clinical_data_df[[paste0(predictor_var_colname, group)]] = ifelse(clinical_data_df[[predictor_var_colname]] == group, 1, 0)
    clinical_data_df[[paste0(predictor_var_colname, group)]] = as.factor(clinical_data_df[[paste0(predictor_var_colname, group)]])
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
        message(sprintf("Computing %s for %s using %s family and %s link", group, clin_manifest,  deparse(substitute(family)), link))
        formula <- as.formula(sprintf("`%s` ~ %s%s + %s",
                                      clin_manifest,
                                      predictor_var_colname,
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
        or_ci_df[[predictor_var_colname]] <- group

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
                     predictor_var_colname = predictor_var_colname, pt.size = pt.size, y.axis.text.size = y.axis.text.size,
                     x.axis.text.size = x.axis.text.size, name_mapping = name_mapping, adjust_vars = adjustment_vars,
                     add_xylabs = add_xylabs, or.label.size = or.label.size, plot_title = forest_plot_title,
                     filter_sig_OR = filter_sig_OR, adjust_p = adjust_p, p_adj_method = p_adj_method)
    results_list$forest_plot <- forest
  }

  message("DONE!")
  return(results_list)
}
