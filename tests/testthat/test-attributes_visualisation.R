if (requiet("emmeans")) {
  test_that("attributes_means", {
    model <- lm(Sepal.Length ~ Species * Sepal.Width, data = iris)

    estim <- estimate_means(model)
    expect_identical(attributes(estim)$at, "Species")
    expect_identical(attributes(estim)$fixed, NULL)

    estim <- estimate_means(model, fixed = "Sepal.Width")
    expect_identical(attributes(estim)$at, "Species")
    expect_identical(attributes(estim)$fixed, "Sepal.Width")

    estim <- estimate_means(model, at = "all")
    expect_identical(attributes(estim)$at, c("Species", "Sepal.Width"))
  })



  test_that("attributes_contrasts", {
    model <- lm(Sepal.Length ~ Species * Sepal.Width, data = iris)

    estim <- estimate_contrasts(model)
    expect_identical(attributes(estim)$contrast, "Species")
    expect_identical(attributes(estim)$at, NULL)
    expect_identical(attributes(estim)$fixed, NULL)

    estim <- estimate_contrasts(model, fixed = "Sepal.Width")
    expect_identical(attributes(estim)$contrast, "Species")
    expect_identical(attributes(estim)$fixed, "Sepal.Width")
    expect_identical(attributes(estim)$modulate, NULL)
  })


  test_that("attributes_link", {
    model <- lm(Sepal.Length ~ Species * Sepal.Width, data = iris)

    estim <- estimate_link(model)
    expect_identical(attributes(estim)$response, "Sepal.Length")
  })
}
