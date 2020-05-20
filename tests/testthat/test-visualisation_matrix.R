if (require("testthat") && require("modelbased")) {
  test_that("visualisation_matrix", {
    testthat::expect_equal(nrow(visualisation_matrix(iris, length = 2)), 48)
    testthat::expect_equal(nrow(visualisation_matrix(iris, target = "Sepal.Length", length = 2, factors = "combinations")), 6)
    testthat::expect_equal(nrow(visualisation_matrix(iris, target = "Species", length = 2, factors = "combinations")), 3)
    testthat::expect_equal(nrow(visualisation_matrix(iris, target = "Species", length = 2, numerics = 0)), 3)
    testthat::expect_equal(nrow(visualisation_matrix(iris, target = "Sepal.Length", length = 3, standardize = TRUE)), 3)
    testthat::expect_equal(nrow(visualisation_matrix(iris, target = "Sepal.Length", length = 3, standardize = TRUE, standardize_robust = TRUE)), 3)
    testthat::expect_warning(nrow(visualisation_matrix(iris, target = "Sepal.Length", length = 4, standardize = TRUE)), 5)

    testthat::expect_equal(nrow(visualisation_matrix(data.frame(
      X = c("A", "A", "B"),
      Y = c(1, 5, 2)
    ), target = "Y", factors = "mode", length = 5, standardize = TRUE)), 5)

    testthat::expect_equal(nrow(visualisation_matrix(iris, target = c("Sepal.Length = 3", "Species"))), 3)
    testthat::expect_equal(nrow(visualisation_matrix(iris, target = c("Sepal.Length = c(3, 1)", "Species = 'setosa'"))), 2)

    x1 <- visualisation_matrix(iris, target = c("Species", "Sepal.Length"), length = 30, preserve_range = TRUE)
    testthat::expect_equal(c(nrow(x1), ncol(x1)), c(55, 5))
    x2 <- visualisation_matrix(iris[c("Species", "Sepal.Length")], length = 30, preserve_range = TRUE)
    testthat::expect_equal(c(nrow(x2), ncol(x2)), c(55, 2))
  })
}
