context("data_grid")



test_that("data_grid", {
  testthat::expect_equal(nrow(data_grid(iris, length = 2)), 48)
  testthat::expect_equal(nrow(data_grid(iris, target = "Sepal.Length", length = 2, factors = "combinations")), 6)
  testthat::expect_equal(nrow(data_grid(iris, target = "Species", length = 2, factors = "combinations")), 3)
  testthat::expect_equal(nrow(data_grid(iris, target = "Species", length = 2, numerics = 0)), 3)
})