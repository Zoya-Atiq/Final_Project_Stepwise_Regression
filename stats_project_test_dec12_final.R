# test_that

library(testthat)

context("check function parameters")
source("~/Box Sync/Fall19-20/Statistical Computing/stats_project_functions.R")

test_that("parameters are of correct type", {
  expect_true(class(depvar) == "numeric")
  expect_true(class(df) == "data.frame")
  expect_true(ncol(df) > 2)
})
