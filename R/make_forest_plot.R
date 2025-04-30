make_forest_plot <- function(results_df, clinical_manifestation, outcome_var_colname, adjust_vars,
                             clinical_manifestation_colname = "clinical_manifestation", ntop = NULL,
                             adjust_p = adjust_p, p_adj_method = p_adj_method,
                             pt.size = pt.size, y.axis.text.size, x.axis.text.size, or.label.size, plot_title = "",
                             name_mapping = NULL, add_xylabs = FALSE, filter_sig_OR = TRUE) {

  # Filter results to remove intercepts and adjust variables.
  if (length(adjust_vars) == 1 && grepl("\\+", adjust_vars)) {
    adjust_vars <- unlist(strsplit(adjust_vars, "\\+"))
    adjust_vars <- trimws(adjust_vars)
  }


  if (filter_sig_OR){
    print("Adjusted variables to be removed:")
    print(adjust_vars)
    results_df <- results_df %>%
      filter(`2.5 %` < 1 & `97.5 %` < 1 | `2.5 %` > 1 & `97.5 %` > 1) %>%
      filter(predictor != "(Intercept)") %>%
      filter(!str_detect(predictor, paste(adjust_vars, collapse = "|")))
  }


  if (!is.null(ntop)){
    # Select top results
    top_results <- results_df[0:ntop, ] %>%
      arrange(-OR)
  } else {
    top_results <- results_df
  }

  if (adjust_p){
    pvalue_var_name <- "adj_p"
  } else {
    p_value_var_name <- "Pr>|z|"
  }

  # Categorize results
  #top_results$category <- with(top_results, ifelse(`Pr>|z|` < 0.05, "Pval < 0.05", "Pval > 0.05"))
  top_results$category <- ifelse(top_results[[pvalue_var_name]] < 0.05, "Pval < 0.05", "Pval > 0.05")

  # Map names if provided by user
  if (!is.null(name_mapping)) {
    clinical_manifestation <- sapply(clinical_manifestation, function(col) name_mapping[[col]] %||% col)
    # Rename clinical manifestation
    top_results[[clinical_manifestation_colname]] <- sapply(top_results[[clinical_manifestation_colname]], function(clin) name_mapping[[clin]] %||% clin)
  }

  # Adjust x max and min limits for the plot.
  xlim_min <- min(top_results[["2.5 %"]][is.finite(top_results[["2.5 %"]])], na.rm = TRUE) - 0.5
  xlim_max <- max(top_results[["97.5 %"]][is.finite(top_results[["97.5 %"]])], na.rm = TRUE) + 0.5
  break_sizes <- (xlim_max - xlim_min) / 10

  # Forest plot
  forest <- ggplot(top_results, aes(x = OR, y = reorder(paste(clinical_manifestation, .data[[outcome_var_colname]], predictor, sep = " - "), OR))) +
    geom_errorbarh(aes(xmin = `2.5 %`, xmax = `97.5 %`), height = 0.2, color = "black") +  # Confidence intervals
    geom_point(aes(color = .data[[pvalue_var_name]], shape = category), size = pt.size, ) +                                   # OR points, colored by category
    scale_color_gradient(
      low = ki_colors(color_palette = "functional")["1_dark_plum-text"],
      high = ki_colors(color_palette = "functional")["3_blue"],
      name = ifelse(adjust_p, "Adj P-value", "P-value")
    ) +
    #scale_color_manual(
    #  values = c(
    #    "Pval < 0.05" = "#870052",
    #    "Pval > 0.05" = "#4682B4"
    #  ),
    #  name = "Category"
    #) +
    geom_text(
      aes(label = sprintf("%.2f [%.2f, %.2f]", OR, `2.5 %`, `97.5 %`)),  # Format: OR [2.5%, 97.5%]
      hjust = -0.1, vjust = -0.7,
      color = "gray40", size = or.label.size
    ) +

    # Add a vertical line at OR = 1 for reference
    geom_vline(xintercept = 1, color = "gray40", linetype = "dashed", size = 0.8) +

    # Add minimal theme with border and grid lines
    theme_classic() +
    theme(
      panel.border = element_rect(color = "black", fill = NA, size = 0.5),  # Border around plot
      panel.grid.major.y = element_blank(),                                     # Remove horizontal grid lines
      legend.position = "right",
      legend.text = element_text(size = y.axis.text.size - 2),
      legend.title = element_text(size = y.axis.text.size),
      plot.title = element_text(hjust = 0.5),
      plot.caption = element_text(hjust = 0.5),
      axis.text.y = element_text(size = y.axis.text.size),
      axis.text.x = element_text(size = x.axis.text.size, angle = 45, hjust = 1, face = "bold")
    ) +

    # Adjust x-axis limits based on user-defined parameters
    scale_x_continuous(
      limits = c(xlim_min, xlim_max),
      breaks = seq(xlim_min, xlim_max, by = break_sizes),#,  # Generate default breaks
      labels = function(x) sprintf("%.2f", x)
    )

    if (add_xylabs){
      forest = forest + labs(
        x = "Odds Ratio (OR)",
        y = "Variable",
        title = plot_title,
        caption = "Confidence intervals represent 95% CI"
      )
    } else {
      forest = forest + labs(
        x = "",
        y = "",
        title = "")
    }

  return(forest)
}

