# test-clustering-utils.R

library(testthat)
library(cluster)
library(dplyr)

# Load your functions
devtools::load_all("/home/anagon@ad.cmm.se/Documents/packages/autoclust/")

test_that("generate_synthetic_clusters() returns exactly n_samples", {
  df <- generate_synthetic_clusters(n_samples = 100, n_vars = 5, n_clusters = 3,
                                    data_type = "continuous", cluster_sep = 1)
  expect_equal(nrow(df), 100)
})


test_that("generate_synthetic_clusters() returns correct structure", {
  df <- generate_synthetic_clusters(n_samples = 90, n_vars = 5, n_clusters = 3,
                                    data_type = "continuous", cluster_sep = 2
                                    )

  expect_true(is.data.frame(df))
  expect_equal(ncol(df), 7)  # 5 vars + Cluster + Batch
  expect_equal(nrow(df), 90)
  expect_true("cluster" %in% names(df))
  expect_true("batch" %in% names(df))
})

test_that("compute_dissimilarity_matrix() returns expected output", {
  df <- generate_synthetic_clusters(n_samples = 60, n_vars = 5, data_type = "binary")
  diss <- compute_dissimilarity_matrix(df[, 1:5], metric = "gower", plot_dissim = FALSE)

  expect_true("dissimilarity_matrix" %in% names(diss))
  #expect_type(diss$dissimilarity_matrix, "matrix")
  expect_equal(nrow(diss$dissimilarity_matrix), 60)
})


