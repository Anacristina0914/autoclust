#' Plot Most Similar and Most Dissimilar Pairs from a Dissimilarity Matrix
#'
#' This function visualizes the most similar and most dissimilar pairs of individuals
#' based on a dissimilarity matrix. It returns an interactive heatmap showing the
#' dissimilarity values, with the most similar pair highlighted in blue and the most
#' dissimilar pair highlighted in red.
#'
#' @param x A dissimilarity matrix or an object of class `dissimilarity`.
#' @param df A dataframe containing individual IDs.
#' @param id_col A string specifying the column in `df` with individual IDs.
#'
#' @return A `ggplotly` object displaying the heatmap of the dissimilarity matrix.
#'         The most similar and most dissimilar pairs are highlighted.
#' @export
#' @examples
#' # Assuming `dissim_matrix` is your dissimilarity matrix and `id_df` is your ID dataframe
#' # plot_most_similar_dissimilar(dissim_matrix, id_df, "ID")
plot_most_similar_dissimilar <- function(x, df, id_col) {

  # If x is a dissimilarity object, convert it to a matrix
  if (class(x)[1] == "dissimilarity") {
    x <- as.matrix(x)
  }

  # Set row and column names from the ID column of the dataframe
  ids <- df[[id_col]]
  rownames(x) <- ids
  colnames(x) <- ids

  # Exclude duplicate pairs by setting upper triangle to NA
  x[upper.tri(x)] <- NA

  # Identify indices of the most similar and most dissimilar pairs
  min_idx <- which(x == min(x, na.rm = TRUE), arr.ind = TRUE)
  max_idx <- which(x == max(x, na.rm = TRUE), arr.ind = TRUE)

  # Convert dissimilarity matrix to a long format for ggplot
  dissimilarity_df <- as.data.frame(as.table(as.matrix(x)))
  names(dissimilarity_df) <- c("ID1", "ID2", "Dissimilarity")

  # Min points
  min_dissim <- dissimilarity_df %>%
    arrange(Dissimilarity) %>%
    filter(ID1 != ID2 & Dissimilarity == min(Dissimilarity, na.rm = TRUE))

  max_dissim <- dissimilarity_df %>%
    arrange(desc(Dissimilarity)) %>%
    filter(ID1 != ID2 & Dissimilarity == max(Dissimilarity, na.rm = TRUE))

  # Create a ggplot heatmap of the dissimilarity matrix
  heatmap_plot <- ggplot(dissimilarity_df, aes(x = ID1, y = ID2, fill = Dissimilarity)) +
    geom_tile() +
    theme_minimal() +
    labs(title = "Dissimilarity Matrix Heatmap") +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1),
          axis.text = element_text(size = 5))

  # Highlight the most similar (blue) and most dissimilar (red) pairs
  heatmap_plot <- heatmap_plot +
    geom_point(data = min_dissim, color = "#4DB5BC", size = 1) +
    geom_point(data = max_dissim, color = "#870052", size = 1)

  # Return ggplot
  #return(heatmap_plot)

  # Convert to a plotly object for interactivity and return it
  ggplotly(heatmap_plot)
}

