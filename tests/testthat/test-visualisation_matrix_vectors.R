if (require("testthat") && require("modelbased")) {
  test_that("visualisation_matrix - vectors", {
    expect_equal(length(visualisation_matrix(iris$Species)), 3)
    expect_equal(length(visualisation_matrix(c("A", "A", "B"))), 2)

    expect_equal(length(visualisation_matrix(iris$Species, target = "c('versicolor')")), 1)
    expect_equal(length(visualisation_matrix(c("A", "A", "B"), target = "dupa = 'A'")), 1)
    expect_equal(length(visualisation_matrix(iris$Species, target = "['versicolor', 'virginica']")), 2)
    expect_equal(length(visualisation_matrix(iris$Species, target = "[versicolor, virginica]")), 2)
  })
}
