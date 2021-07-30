if (require("testthat") && require("modelbased")) {
  test_that("attributes_means", {
    model <- lm(Sepal.Length ~ Species * Sepal.Width, data = iris)

    estim <- estimate_means(model)
    expect_equal(attributes(estim)$at, "Species")
    expect_equal(attributes(estim)$fixed, NULL)

    estim <- estimate_means(model, fixed = "Sepal.Width")
    expect_equal(attributes(estim)$at, "Species")
    expect_equal(attributes(estim)$fixed, "Sepal.Width")

    estim <- estimate_means(model, at = "all")
    expect_equal(attributes(estim)$at, c("Species", "Sepal.Width"))
  })



  test_that("attributes_contrasts", {
    model <- lm(Sepal.Length ~ Species * Sepal.Width, data = iris)

    estim <- estimate_contrasts(model)
    expect_equal(attributes(estim)$contrast, "Species")
    expect_equal(attributes(estim)$at, "Sepal.Width")
    expect_equal(attributes(estim)$fixed, NULL)

    estim <- estimate_contrasts(model, fixed = "Sepal.Width")
    expect_equal(attributes(estim)$contrast, "Species")
    expect_equal(attributes(estim)$fixed, "Sepal.Width")
    expect_equal(attributes(estim)$modulate, NULL)

  })


  test_that("attributes_link", {
    model <- lm(Sepal.Length ~ Species * Sepal.Width, data = iris)

    estim <- estimate_link(model)
    expect_equal(attributes(estim)$response, "Sepal.Length")
  })
}
