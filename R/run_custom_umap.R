#' Run UMAP with Custom Parameters
#'
#' This function performs Uniform Manifold Approximation and Projection (UMAP)
#' on the input data, allowing customization of UMAP parameters, such as the
#' number of neighbors, minimum distance, and the number of training epochs.
#'
#' @param data A matrix or dataframe of numeric values where rows represent samples
#'             and columns represent features.
#' @param neighbors An integer specifying the number of neighbors to consider
#'                  for each point in the UMAP algorithm. Higher values result
#'                  in capturing broader patterns, while lower values capture finer details.
#' @param min_dist A numeric value indicating the minimum distance between points
#'                 in the UMAP embedding. Smaller values keep similar points close,
#'                 while larger values increase separation.
#' @param n_epochs An integer specifying the number of training epochs for UMAP.
#'                 The default value is 200. Higher values may improve stability
#'                 of the embedding at the cost of computation time.
#'
#' @return A dataframe with two columns (`UMAP1`, `UMAP2`) representing the
#'         UMAP embedding coordinates for each sample.
#'
#' @details The function allows for flexible UMAP configurations to be used
#'          with distance-based input. It leverages `umap.defaults` as a base
#'          and overrides specific parameters based on function inputs.
#'
#' @import umap
#' @export
#' @examples
#' # Example usage:
#' # Assuming `data_matrix` is a matrix of distances or a high-dimensional dataset:
#' run_custom_umap(data = data_matrix, neighbors = 15, min_dist = 0.1, n_epochs = 300)
# Function to run UMAP
run_custom_umap <- function(data, neighbors, min_dist, n_epochs = 200) {
  custom.config <- umap.defaults
  custom.config$n_neighbors <- neighbors
  custom.config$min_dist <- min_dist
  custom.config$n_epochs <- n_epochs

  umap_result <- umap(as.matrix(data), input = "dist", config = custom.config)
  umap_coords <- as.data.frame(umap_result$layout)
  colnames(umap_coords) <- c("UMAP1", "UMAP2")

  return(umap_coords)
}
