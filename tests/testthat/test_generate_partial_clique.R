library(testthat)
library(UWBiost561)

test_check("UWBiost561")

context("Testing generate_partial_clique in the corresponding R folder")

test_that("generate_partial_clique has proper density", {
  medium = 10
  simulation = UWBiost561::generate_partial_clique(n = medium, clique_fraction = 1, clique_edge_density = 1)
  simulation$adj_mat

  expect_true(all(rowSums == medium))
})

test_that("generate_partial_clique doesn't accept invalid args", {
  invalid = 1.1
  expect_error(UWBiost561::generate_partial_clique(n = 10,
                                                   clique_fraction = invalid,
                                                   clique_edge_density = invalid),
               "clique_fraction must be a numeric value between 0 and 1")
})

