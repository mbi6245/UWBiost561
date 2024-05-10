library(testthat)
library(UWBiost561)

test_check("UWBiost561")

context("Testing compute_maximal_partial_clique in the corresponding R folder")

test_that("compute_maximal_partial_clique is right size", {
  big = 51
  expect_error(UWBiost561::compute_maximal_partial_clique(adj_mat = matrix(1,
                                                                           nrow = big,
                                                                           nrow = big),
                                                          alpha = 0.9),
               "adj_mat must have between 5 and 50 rows inclusive")
})

test_that("compute maximal_partial clique accepts correct alpha", {
  dense = 1.1
  expect_error(UWBiost561::compute_maximal_partial_clique(adj_mat = matrix(1,
                                                                           nrow = 10,
                                                                           ncol = 10),
                                                          alpha = dense),
               "alpha must be between 0 and 1 inclusive")
})

test_that("compute_maximal_partial_clique has correct output", {
  set.seed(0)
  simulation = UWBiost561::generate_partial_clique(
    n = 10,
    clique_fraction = 0.5,
    clique_edge_density = 0.9
  )

  res <- UWBiost561::compute_maximal_partial_clique(
    adj_mat = simulation$adj_mat,
    alpha = 0.9
  )

  expect_true(res$clique_idx = c(1, 2, 3, 4, 5), res$edge_density = 0.9)
})

test_that("compute maximal_partial_clique has correct output", {
  set.seed(0)
  res = UWBiost561::compute_maximal_partial_clique(
    adj_mat = matrix(1, nrow = 10, ncol = 10),
    alpha = 1
  )

  expect_true(res$clique_idx = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), res$edge_density = 1)
})

test_that("compute_maximal_partial_clique has correct output", {
  set.seed(0)

  simulation = UWBiost561::generate_partial_clique(
    n = 10,
    clique_fraction = 0,
    clique_edge_density = 0
  )

  res = UWBiost561::compute_maximal_partial_clique(
    adj_mat = simulation$adj_mat,
    alpha = 0
  )

  expect_true(res$clique_idx = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), res$edge_density = 0)
})
