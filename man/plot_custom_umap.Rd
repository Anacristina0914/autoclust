% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_custom_umap.R
\name{plot_custom_umap}
\alias{plot_custom_umap}
\title{Plot UMAP across different neighbors and min_distance values.}
\usage{
plot_custom_umap(umap_coords, clusters, neighbors, min_dist, pt.size = 0.5)
}
\arguments{
\item{umap_coords}{umap coordinates for plots.}

\item{clusters}{vector containing clusters for each point.}

\item{neighbors}{number of neighbors used to compute umap coords.}

\item{min_dist}{minimum distance used to compute umap coords.}

\item{pt.size}{point size for umap plot.}
}
\value{
a ggplot object of UMAP plot.
}
\description{
Plot UMAP across different neighbors and min_distance values.
}
\examples{
# Example of how to use the function
umap_plot <- plot_custom_umap(umap_coords = coords, neighbors = 10,
min_dist = 0.4, pt.size = 1)

}
