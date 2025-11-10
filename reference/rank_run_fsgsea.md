# Run Ranked Gene Set Enrichment Analysis (fGSEA) from limma Results

This function performs a ranked Gene Set Enrichment Analysis (GSEA)
using fgsea based on differential expression results (e.g., from limma).
Genes are ranked according to log fold change, p-value, or a combined
score, and enrichment is tested against user-specified MSigDB gene set
collections. Optionally, enrichment plots and summary visualizations of
the top enriched pathways are produced.

## Usage

``` r
rank_run_fsgsea(
  limma_results_df,
  msigdb_collection,
  msigdb_cached = NULL,
  fgsea_maxsize = 500,
  fgsea_minsize = 15,
  top_pathways = 10,
  rank_by = "logFC+P.Value",
  gene_name_colname = "Gene Name",
  plot_enrichment = TRUE,
  comparison_name = NULL
)
```

## Arguments

- limma_results_df:

  A data frame containing differential expression results, typically
  from a `limma::topTable()` output. Must include columns for `logFC`,
  `P.Value`, and a gene identifier column.

- msigdb_collection:

  Character vector specifying one or more MSigDB collection codes (e.g.,
  `"H"` for hallmark, `"C2"` for curated gene sets). Valid options are
  retrieved via
  [`msigdbr::msigdbr_collections()`](https://igordot.github.io/msigdbr/reference/msigdbr_collections.html).

- msigdb_cached:

  Optional pre-fetched MSigDB data frame (to avoid repeated API calls).
  If supplied, must contain at least columns `gs_name`, `gene_symbol`,
  `gs_collection`, `gs_subcollection`, and `gs_description`.

- fgsea_maxsize:

  Integer; maximum size of gene sets to include in analysis (default:
  `500`).

- fgsea_minsize:

  Integer; minimum size of gene sets to include in analysis (default:
  `15`).

- top_pathways:

  Integer; number of top enriched pathways to display in the summary
  plot (default: `10`).

- rank_by:

  Character string specifying how genes are ranked. Options are:

  - `"logFC"` — rank by log fold change.

  - `"P.Value"` — rank by \\-log10(P.Value)\\.

  - `"logFC+P.Value"` — combined ranking using \\-log10(P.Value) \*
    sign(logFC)\\ (default).

- gene_name_colname:

  Character string indicating the column name in `limma_results_df`
  corresponding to gene symbols (default: `"Gene Name"`).

- plot_enrichment:

  Logical; if `TRUE`, generates both the enrichment plot for the top
  pathway and a summary plot for the top enriched pathways (default:
  `TRUE`).

- comparison_name:

  Optional character string used in plot titles to label the comparison
  (e.g., "Cluster 1 vs Control").

## Value

A named list containing:

- `ranked_df` — Data frame of ranked genes with scores used for
  enrichment.

- `ranked_genes` — Named numeric vector of ranked gene scores (input to
  fgsea).

- `gene_set_metadata` — Data frame describing the gene sets used (name,
  category, subcategory, description).

- `fgsea_results` — Data frame of enrichment results from
  `fgseaMultilevel()`, including NES, p-values, and adjusted p-values.

- `fgsea_plot` — (Optional) `ggplot2` object showing enrichment for the
  top pathway.

- `top_pathways_plot` — (Optional) `ggplot2` object summarizing the top
  enriched pathways.

## Details

The function first validates the chosen MSigDB collections and retrieves
or filters the corresponding gene sets. Genes are ranked according to
the selected `rank_by` method, and only unique gene entries (highest
absolute score per gene) are retained. The ranked gene vector is then
used as input to `fgseaMultilevel()` to compute enrichment scores (NES)
and adjusted p-values.

Two plots can be optionally generated:

1.  An enrichment curve for the top pathway.

2.  A bubble plot summarizing the top enriched pathways, where bubble
    color represents adjusted p-value, size represents gene set size,
    and shape indicates significance.

## See also

[`fgsea::fgseaMultilevel()`](https://rdrr.io/pkg/fgsea/man/fgseaMultilevel.html),
[`msigdbr::msigdbr()`](https://igordot.github.io/msigdbr/reference/msigdbr.html),
`limma::topTable()`

## Examples

``` r
if (FALSE) { # \dontrun{
# Example using limma differential expression results
gsea_results <- rank_run_fsgsea(
  limma_results_df = limma_res,
  msigdb_collection = c("H", "C2"),
  rank_by = "logFC+P.Value",
  comparison_name = "Cluster 1 vs Control"
)

# View results and plots
head(gsea_results$fgsea_results)
gsea_results$top_pathways_plot
} # }
```
