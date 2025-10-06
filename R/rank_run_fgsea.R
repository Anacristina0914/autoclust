#' Run Ranked Gene Set Enrichment Analysis (fGSEA) from limma Results
#'
#' This function performs a ranked Gene Set Enrichment Analysis (GSEA) using \pkg{fgsea}
#' based on differential expression results (e.g., from \pkg{limma}).
#' Genes are ranked according to log fold change, p-value, or a combined score,
#' and enrichment is tested against user-specified MSigDB gene set collections.
#' Optionally, enrichment plots and summary visualizations of the top enriched pathways are produced.
#'
#' @param limma_results_df A data frame containing differential expression results,
#'   typically from a \code{limma::topTable()} output. Must include columns for
#'   \code{logFC}, \code{P.Value}, and a gene identifier column.
#' @param msigdb_collection Character vector specifying one or more MSigDB collection codes
#'   (e.g., \code{"H"} for hallmark, \code{"C2"} for curated gene sets).
#'   Valid options are retrieved via \code{msigdbr::msigdbr_collections()}.
#' @param msigdb_cached Optional pre-fetched MSigDB data frame (to avoid repeated API calls).
#'   If supplied, must contain at least columns \code{gs_name}, \code{gene_symbol},
#'   \code{gs_cat}, \code{gs_subcat}, and \code{gs_description}.
#' @param fgsea_maxsize Integer; maximum size of gene sets to include in analysis (default: \code{500}).
#' @param fgsea_minsize Integer; minimum size of gene sets to include in analysis (default: \code{15}).
#' @param top_pathways Integer; number of top enriched pathways to display in the summary plot (default: \code{10}).
#' @param rank_by Character string specifying how genes are ranked. Options are:
#'   \itemize{
#'     \item \code{"logFC"} — rank by log fold change.
#'     \item \code{"P.Value"} — rank by \eqn{-log10(P.Value)}.
#'     \item \code{"logFC+P.Value"} — combined ranking using \eqn{-log10(P.Value) * sign(logFC)} (default).
#'   }
#' @param gene_name_colname Character string indicating the column name in \code{limma_results_df}
#'   corresponding to gene symbols (default: \code{"Gene Name"}).
#' @param plot_enrichment Logical; if \code{TRUE}, generates both the enrichment plot for the top pathway
#'   and a summary plot for the top enriched pathways (default: \code{TRUE}).
#' @param comparison_name Optional character string used in plot titles to label the comparison
#'   (e.g., "Cluster 1 vs Control").
#'
#' @details
#' The function first validates the chosen MSigDB collections and retrieves or filters the corresponding gene sets.
#' Genes are ranked according to the selected \code{rank_by} method, and only unique gene entries
#' (highest absolute score per gene) are retained.
#' The ranked gene vector is then used as input to \code{fgseaMultilevel()} to compute enrichment scores (NES)
#' and adjusted p-values.
#'
#' Two plots can be optionally generated:
#' \enumerate{
#'   \item An enrichment curve for the top pathway.
#'   \item A bubble plot summarizing the top enriched pathways, where bubble color represents adjusted p-value,
#'         size represents gene set size, and shape indicates significance.
#' }
#'
#' @return A named list containing:
#' \itemize{
#'   \item \code{ranked_df} — Data frame of ranked genes with scores used for enrichment.
#'   \item \code{ranked_genes} — Named numeric vector of ranked gene scores (input to fgsea).
#'   \item \code{gene_set_metadata} — Data frame describing the gene sets used (name, category, subcategory, description).
#'   \item \code{fgsea_results} — Data frame of enrichment results from \code{fgseaMultilevel()}, including NES, p-values, and adjusted p-values.
#'   \item \code{fgsea_plot} — (Optional) \code{ggplot2} object showing enrichment for the top pathway.
#'   \item \code{top_pathways_plot} — (Optional) \code{ggplot2} object summarizing the top enriched pathways.
#' }
#'
#' @examples
#' \dontrun{
#' # Example using limma differential expression results
#' gsea_results <- rank_run_fsgsea(
#'   limma_results_df = limma_res,
#'   msigdb_collection = c("H", "C2"),
#'   rank_by = "logFC+P.Value",
#'   comparison_name = "Cluster 1 vs Control"
#' )
#'
#' # View results and plots
#' head(gsea_results$fgsea_results)
#' gsea_results$top_pathways_plot
#' }
#'
#' @seealso [fgsea::fgseaMultilevel()], [msigdbr::msigdbr()], [limma::topTable()]
#' @import dplyr ggplot2 fgsea msigdbr purrr forcats
#' @export
rank_run_fsgsea <- function(limma_results_df, msigdb_collection, msigdb_cached = NULL,
                            fgsea_maxsize=500, fgsea_minsize=15, top_pathways = 10,
                            rank_by = "logFC+P.Value", gene_name_colname = "Gene Name",
                            plot_enrichment = TRUE, comparison_name = NULL){

  # Ensure that a valid option was given to rank genes
  rank_by <- match.arg(rank_by, choices = c("logFC", "P.Value", "logFC+P.Value"), several.ok = FALSE)

  # Validate collection(s)
  valid_collections <- msigdbr::msigdbr_collections() %>%
    dplyr::pull(gs_collection) %>%
    unique()

  if (!all(msigdb_collection %in% valid_collections)) {
    stop("Invalid MSigDB collection name(s). Valid options include: ",
         paste(valid_collections, collapse = ", "))
  }

  # Use cached data or pull from msigdbr
  if (is.null(msigdb_cached)) {
    msig_collection <- purrr::map_dfr(msigdb_collection, function(collection) {
      msigdbr(species = "Homo sapiens", category = collection)
    }) %>%
      dplyr::select(gs_name, gene_symbol, gs_cat, gs_subcat, gs_description)
  } else {
    msig_collection <- msigdb_cached %>%
      filter(gs_cat %in% msigdb_collection) %>%
      dplyr::select(gs_name, gene_symbol, gs_cat, gs_subcat, gs_description)
  }

  # Create a list to store the results
  result_list <- list()
  # Decide ranking strategy
  if (setequal(rank_by, c("logFC+P.Value"))) {
    message("Score = -log10(P.Value) * sign(logFC)")
    ranked_df <- limma_results_df %>%
      filter(!is.na(.data[[gene_name_colname]])) %>%
      mutate(score = -log10(P.Value)* sign(logFC))

  } else if (identical(rank_by, "P.Value")) {
    message("score = -log10(P.Value)")
    ranked_df <- limma_results_df %>%
      filter(!is.na(.data[[gene_name_colname]])) %>%
      mutate(score = -log10(P.Value))

  } else if (identical(rank_by, "logFC")) {
    message("score = logFC")
    ranked_df <- limma_results_df %>%
      filter(!is.na(.data[[gene_name_colname]])) %>%
      mutate(score = logFC)

  } else {
    stop("Invalid ranking option. Choose from: 'logFC', 'P.Value', or 'logFC+P.Value'")
  }

  # Ensure unique gene entries
  ranked_df <- ranked_df %>%
    group_by(.data[[gene_name_colname]]) %>%
    slice_max(order_by = abs(score), n = 1, with_ties = FALSE) %>%
    ungroup()

  # Rank gene names
  ranked_genes <- ranked_df$score
  names(ranked_genes) <- ranked_df[[gene_name_colname]]
  ranked_genes <- sort(ranked_genes, decreasing = TRUE)

  # Save ranked df and genes in list
  result_list[["ranked_df"]] <- ranked_df
  result_list[["ranked_genes"]] <- ranked_genes

  # Save description info separately
  gene_set_metadata <- msig_collection %>%
    distinct(gs_name, gs_cat, gs_subcat, gs_description)
  result_list[["gene_set_metadata"]] <- gene_set_metadata

  # Convert to list format required by fgsea (lists with gs names and all the elements as character).
  pathways_collection <- split(msig_collection$gene_symbol, msig_collection$gs_name)

  # Run fgsea
  fgsea_results <- fgseaMultilevel(
    pathways = pathways_collection,
    stats = ranked_genes,
    minSize = fgsea_minsize,
    maxSize = fgsea_maxsize
  ) %>% arrange(padj)

  # Store in results list
  result_list[["fgsea_results"]] <- fgsea_results

  if (plot_enrichment && nrow(fgsea_results) > 0) {
    top_pathway <- fgsea_results$pathway[1]
    enrichment_plot <- plotEnrichment(pathways_collection[[top_pathway]], ranked_genes) +
      ggtitle(top_pathway)
    result_list[["fgsea_plot"]] <- enrichment_plot

    # Prepare data for top pathways summary plot
    top_n <- top_pathways
    fgsea_top <- fgsea_results %>%
      slice_min(order_by = padj, n = top_n) %>%
      mutate(log10padj = -log10(padj),
             pathway = forcats::fct_reorder(pathway, log10padj),
             significant = padj < 0.05) %>%
      arrange(NES)

    # Plot top pathways
    top_pathways_plot <- ggplot(fgsea_top, aes(x = NES, y = reorder(pathway, NES))) +
      geom_point(aes(size = size, color = log10padj, shape = significant), stroke = 1.5) +
      scale_color_gradient(low = "#4DB5BC", high = "#B84145") +
      scale_shape_manual(values = c(`TRUE` = 19, `FALSE` = 1)) +
      scale_size_continuous(range = c(2, 8)) +
      labs(
        x = "Normalized Enrichment Score (NES)",
        y = NULL,
        size = "Leading Edge Size",
        color = "-log10(adjusted P-value)",
        title = paste("Top", top_n, "Enriched Pathways in", comparison_name)
      ) +
      theme_bw(base_size = 12) +
      theme(panel.grid.major.y = element_blank())

    result_list[["top_pathways_plot"]] <- top_pathways_plot
  }

  return(result_list)
}
