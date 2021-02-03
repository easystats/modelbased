if (require("testthat") && require("modelbased")) {
  test_that("attributes_means", {
    model <- lm(Sepal.Length ~ Species * Sepal.Width, data = iris)

    estim <- estimate_means(model)
    expect_equal(attributes(estim)$levels, "Species")
    expect_equal(attributes(estim)$fixed, NULL)
    expect_equal(attributes(estim)$modulate, NULL)

    estim <- estimate_means(model, fixed = "Sepal.Width")
    expect_equal(attributes(estim)$levels, "Species")
    expect_equal(attributes(estim)$fixed, "Sepal.Width")
    expect_equal(attributes(estim)$modulate, NULL)

    estim <- estimate_means(model, modulate = "Sepal.Width")
    expect_equal(attributes(estim)$levels, "Species")
    expect_equal(attributes(estim)$fixed, NULL)
    expect_equal(attributes(estim)$modulate, "Sepal.Width")
  })



  test_that("attributes_contrasts", {
    model <- lm(Sepal.Length ~ Species * Sepal.Width, data = iris)

    estim <- estimate_contrasts(model)
    expect_equal(attributes(estim)$levels, "Species")
    expect_equal(attributes(estim)$fixed, NULL)
    expect_equal(attributes(estim)$modulate, NULL)

    estim <- estimate_means(model, fixed = "Sepal.Width")
    expect_equal(attributes(estim)$levels, "Species")
    expect_equal(attributes(estim)$fixed, "Sepal.Width")
    expect_equal(attributes(estim)$modulate, NULL)

    estim <- estimate_means(model, modulate = "Sepal.Width")
    expect_equal(attributes(estim)$levels, "Species")
    expect_equal(attributes(estim)$fixed, NULL)
    expect_equal(attributes(estim)$modulate, "Sepal.Width")
  })

  test_that("attributes_link", {
    model <- lm(Sepal.Length ~ Species * Sepal.Width, data = iris)

    estim <- estimate_link(model)
    expect_equal(attributes(estim)$response, "Sepal.Length")
  })
}
