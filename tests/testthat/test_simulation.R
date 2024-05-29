library(testthat)
library(UWBiost561)

test_check("UWBiost561")

context("Testing simulation in the corresponding R folder")

test_that("simulation correctly errors upon invalid alphas", {
  alphas = c(-1, 44)
  expect_error(UWBiost561::simulation(10, alphas),
               "size of adjacency matrices has exceeded recommendations")
})

test_that("simulation correctly errors upon matrix of excessive size", {
  n = 100
  expect_error(UWBiost561::simulation(n, c(0.8, 0.9)),
               "size of adjacency matrices has exceeded recommendations")
})
