#' Create a Forest Plot of Association Results
#'
#' This function generates a publication-ready forest plot summarizing the odds ratios (ORs),
#' 95% confidence intervals, and p-values (or adjusted p-values) from association analyses,
#' such as those produced by \code{run_associations()}. It filters, ranks, and visualizes
#' the most significant associations across clinical manifestations and predictor variables.
#'
#' @param results_df A data frame containing association results, including columns for
#'   \code{OR}, \code{2.5 %}, \code{97.5 %}, p-values (e.g., \code{Pr>|z|}),
#'   and optionally adjusted p-values (\code{adj_p}).
#' @param clinical_manifestation Character vector of clinical manifestation names to include in the plot.
#' @param predictor_var_colname Character string specifying the column name for the predictor variable.
#' @param adjust_vars Character vector or string of adjustment variables used in the model
#'   (e.g., \code{"Age_dx + sex"}). Used to remove adjusted terms from the plotted results.
#' @param clinical_manifestation_colname Character string indicating the column name in
#'   \code{results_df} corresponding to the clinical manifestation variable (default: \code{"clinical_manifestation"}).
#' @param ntop Integer specifying the number of top associations (by OR) to include in the plot (default: \code{NULL}, plots all).
#' @param adjust_p Logical indicating whether adjusted p-values (\code{adj_p}) should be used
#'   for coloring points (default: inherits from calling environment).
#' @param p_adj_method Character string specifying the p-value adjustment method (default: inherits from calling environment).
#' @param pt.size Numeric value controlling point size in the forest plot.
#' @param y.axis.text.size Numeric value specifying text size for the y-axis labels.
#' @param x.axis.text.size Numeric value specifying text size for the x-axis labels.
#' @param or.label.size Numeric value controlling the size of the odds ratio text labels.
#' @param plot_title Character string specifying the plot title (default: \code{""}).
#' @param name_mapping Optional named vector or list mapping variable names to human-readable labels.
#' @param add_xylabs Logical; if \code{TRUE}, adds axis labels and title to the plot (default: \code{FALSE}).
#' @param filter_sig_OR Logical; if \code{TRUE}, filters results to include only significant ORs
#'   (i.e., 95\% CI does not cross 1) (default: \code{TRUE}).
#'
#' @details
#' The function removes intercepts and adjustment variables from the results before plotting.
#' If \code{filter_sig_OR = TRUE}, only terms with 95% confidence intervals entirely above or below 1
#' are included. Associations are colored by their p-value or adjusted p-value, depending on user settings.
#' The x-axis represents the odds ratio (OR) with 95% confidence intervals, and the y-axis shows the
#' clinical manifestation and predictor variable combinations.
#'
#' When \code{name_mapping} is provided, both the clinical manifestation and predictor names are
#' replaced by human-readable labels. This is particularly useful for figure preparation in publications.
#'
#' @return A \code{ggplot2} object representing the forest plot, ready for further customization or saving.
#'
#' @examples
#' \dontrun{
#' forest <- make_forest_plot(
#'   results_df = association_results,
#'   clinical_manifestation = c("arthritis", "nephritis"),
#'   predictor_var_colname = "cluster",
#'   adjust_vars = "Age_dx + sex",
#'   ntop = 10,
#'   adjust_p = TRUE,
#'   pt.size = 2,
#'   y.axis.text.size = 10,
#'   x.axis.text.size = 8,
#'   or.label.size = 3,
#'   plot_title = "Top 10 Cluster Associations",
#'   add_xylabs = TRUE
#' )
#' print(forest)
#' }
#'
#' @seealso [run_associations()], [ggplot2::ggplot()]
#' @import ggplot2 dplyr stringr
#' @export
#'
make_forest_plot <- function(results_df, clinical_manifestation, predictor_var_colname, adjust_vars,
                             clinical_manifestation_colname = "clinical_manifestation", ntop = NULL,
                             adjust_p = adjust_p, p_adj_method = p_adj_method,
                             pt.size = pt.size, y.axis.text.size, x.axis.text.size, or.label.size, plot_title = "",
                             name_mapping = NULL, add_xylabs = FALSE, filter_sig_OR = TRUE) {

  # Filter results to remove intercepts and adjust variables.
  if (length(adjust_vars) == 1 && grepl("\\+", adjust_vars)) {
    adjust_vars <- unlist(strsplit(adjust_vars, "\\+"))
    adjust_vars <- trimws(adjust_vars)
  }

  # Remove intercepts and adjusted variables
  message(sprintf("Adjusted variables to be removed: %s", adjust_vars))
  results_df <- results_df %>%
    filter(predictor != "(Intercept)") %>%
    filter(!str_detect(predictor, paste(adjust_vars, collapse = "|")))

  print(head(results_df))

  if (filter_sig_OR){
    message("Filtering ORs...")
    results_df <- results_df %>%
      filter(`2.5 %` < 1 & `97.5 %` < 1 | `2.5 %` > 1 & `97.5 %` > 1) #%>%
      #filter(predictor != "(Intercept)") %>%
      #filter(!str_detect(predictor, paste(adjust_vars, collapse = "|")))

    # Add another step to skip plotting if no significant results are present
    if (dim(results_df)[1] == 0){
      stop(message("No significant results were found, try setting filter_sig_OR to FALSE to plot results"))
    }
  }


  if (!is.null(ntop)){
    # Select top results
    message(sprintf("Selecting first %s terms to plot...", ntop))
    if (dim(results_df)[1] < ntop ) {
      message(sprintf("Not enough ntop results exist, using ntop = %s instead", dim(results_df)[1]))
      ntop = dim(results_df)[1]
    }

    top_results <- results_df[1:ntop, ] %>%
      arrange(-OR)
  } else {
    top_results <- results_df %>%
      arrange(-OR)
  }

  if (adjust_p){
    pvalue_var_name <- "adj_p"
  } else {
    pvalue_var_name <- "Pr>|z|"
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
  forest <- ggplot(top_results, aes(x = OR, y = reorder(paste(clinical_manifestation, .data[[predictor_var_colname]], predictor, sep = " - "), OR))) +
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

