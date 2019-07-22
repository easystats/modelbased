context("data_grid")



test_that("data_grid", {
  testthat::expect_equal(nrow(data_grid(iris, length = 2)), 48)
  testthat::expect_equal(nrow(data_grid(iris, target = "Sepal.Length", length = 2, factors = "combinations")), 6)
  testthat::expect_equal(nrow(data_grid(iris, target = "Species", length = 2, factors = "combinations")), 3)
  testthat::expect_equal(nrow(data_grid(iris, target = "Species", length = 2, numerics = 0)), 3)


  x1 <- data_grid(iris, target = c("Species", "Sepal.Length"), length = 30, preserve_range = TRUE)
  testthat::expect_equal(c(nrow(x1), ncol(x1)), c(53, 5))
  x2 <- data_grid(iris[c("Species", "Sepal.Length")], length = 30, preserve_range = TRUE)
  testthat::expect_equal(c(nrow(x2), ncol(x2)), c(53, 2))
})
