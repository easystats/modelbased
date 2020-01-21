if (require("testthat") && require("modelbased")) {
  test_that("attributes_means", {

    model <- lm(Sepal.Length ~ Species * Sepal.Width, data = iris)

    estim <- estimate_means(model)
    testthat::expect_equal(attributes(estim)$levels, "Species")
    testthat::expect_equal(attributes(estim)$fixed, NULL)
    testthat::expect_equal(attributes(estim)$modulate, NULL)

    estim <- estimate_means(model, fixed = "Sepal.Width")
    testthat::expect_equal(attributes(estim)$levels, "Species")
    testthat::expect_equal(attributes(estim)$fixed, "Sepal.Width")
    testthat::expect_equal(attributes(estim)$modulate, NULL)

    estim <- estimate_means(model, modulate = "Sepal.Width")
    testthat::expect_equal(attributes(estim)$levels, "Species")
    testthat::expect_equal(attributes(estim)$fixed, NULL)
    testthat::expect_equal(attributes(estim)$modulate, "Sepal.Width")
  })



  test_that("attributes_contrasts", {

    model <- lm(Sepal.Length ~ Species * Sepal.Width, data = iris)

    estim <- estimate_contrasts(model)
    testthat::expect_equal(attributes(estim)$levels, "Species")
    testthat::expect_equal(attributes(estim)$fixed, NULL)
    testthat::expect_equal(attributes(estim)$modulate, NULL)

    estim <- estimate_means(model, fixed = "Sepal.Width")
    testthat::expect_equal(attributes(estim)$levels, "Species")
    testthat::expect_equal(attributes(estim)$fixed, "Sepal.Width")
    testthat::expect_equal(attributes(estim)$modulate, NULL)

    estim <- estimate_means(model, modulate = "Sepal.Width")
    testthat::expect_equal(attributes(estim)$levels, "Species")
    testthat::expect_equal(attributes(estim)$fixed, NULL)
    testthat::expect_equal(attributes(estim)$modulate, "Sepal.Width")
  })

  test_that("attributes_link", {

    model <- lm(Sepal.Length ~ Species * Sepal.Width, data = iris)

    estim <- estimate_link(model)
    testthat::expect_equal(attributes(estim)$response, "Sepal.Length")
  })
}
