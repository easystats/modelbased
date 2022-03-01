if (require("testthat") && require("modelbased")) {


  # Vectors and columns -----------------------------------------------------


  test_that("visualisation_matrix - vectors", {

    # Factors
    expect_equal(length(visualisation_matrix(iris$Species)), 3)
    expect_equal(length(visualisation_matrix(c("A", "A", "B"))), 2)
    expect_equal(length(visualisation_matrix(x = iris$Species, target = "c('versicolor')")), 1)
    expect_equal(length(visualisation_matrix(iris$Species, target = "A = c('versicolor')")), 1)
    expect_equal(length(visualisation_matrix(c("A", "A", "B"), target = "dupa = 'A'")), 1)
    expect_equal(length(visualisation_matrix(iris$Species, target = "['versicolor', 'virginica']")), 2)
    expect_equal(length(visualisation_matrix(iris$Species, target = "[versicolor, virginica]")), 2)


    # Numerics
    expect_equal(length(visualisation_matrix(x = iris$Sepal.Length)), 10)
    expect_equal(length(visualisation_matrix(x = iris$Sepal.Length, length = 5)), 5)
    expect_equal(min(visualisation_matrix(x = iris$Sepal.Length, range = "iqr")), as.numeric(quantile(iris$Sepal.Length, 0.025)))
    expect_equal(min(visualisation_matrix(x = iris$Sepal.Length, range = "hdi")), as.numeric(bayestestR::hdi(iris$Sepal.Length, ci = 0.95))[2])
    expect_equal(min(visualisation_matrix(x = iris$Sepal.Length, range = "eti")), as.numeric(bayestestR::eti(iris$Sepal.Length, ci = 0.95))[2])
    expect_equal(length(visualisation_matrix(iris$Sepal.Length, target = "c(1, 3, 4)")), 3)
    expect_equal(length(visualisation_matrix(iris$Sepal.Length, target = "A = c(1, 3, 4)")), 3)
    expect_equal(length(visualisation_matrix(iris$Sepal.Length, target = "[1, 3, 4]")), 3)
    expect_equal(length(visualisation_matrix(iris$Sepal.Length, target = "[1, 4]")), 10)
    # expect_equal(length(visualisation_matrix(iris$Sepal.Length, standardize = TRUE)), 7)
    # expect_equal(visualisation_matrix(iris$Sepal.Length, standardize = TRUE)[4], mean(iris$Sepal.Length))
  })


  # Dataframes --------------------------------------------------------------


  test_that("visualisation_matrix - dataframes", {
    expect_equal(nrow(visualisation_matrix(iris, length = 2)), 48)
    expect_equal(nrow(visualisation_matrix(iris, at = "Species", length = 2, numerics = 0)), 3)
    expect_equal(nrow(visualisation_matrix(iris, at = "Sepal.Length", length = 3)), 3)

    expect_equal(nrow(visualisation_matrix(data.frame(
      X = c("A", "A", "B"),
      Y = c(1, 5, 2)
    ), target = "Y", factors = "mode", length = 5)), 5)

    expect_equal(nrow(visualisation_matrix(iris, at = c("Sepal.Length = 3", "Species"))), 3)
    expect_equal(nrow(visualisation_matrix(iris, at = c("Sepal.Length = c(3, 1)", "Species = 'setosa'"))), 2)

    x1 <- visualisation_matrix(iris, at = c("Species", "Sepal.Length"), length = 30, preserve_range = TRUE)
    expect_equal(dim(x1), c(55, 5))
    x2 <- visualisation_matrix(iris[c("Species", "Sepal.Length")], length = 30, preserve_range = TRUE)
    expect_equal(dim(x2), c(55, 2))
  })
}
