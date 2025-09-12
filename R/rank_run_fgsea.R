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
