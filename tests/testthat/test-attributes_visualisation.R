test_that("attributes_means", {
  skip_if_not_installed("emmeans")
  model <- lm(Sepal.Length ~ Species * Sepal.Width, data = iris)

  estim <- suppressMessages(estimate_means(model, backend = "emmeans"))
  expect_identical(attributes(estim)$by, "Species")

  estim <- suppressMessages(estimate_means(model, by = "all", backend = "emmeans"))
  expect_identical(attributes(estim)$by, c("Species", "Sepal.Width"))
})


test_that("attributes_contrasts", {
  skip_if_not_installed("emmeans")
  model <- lm(Sepal.Length ~ Species * Sepal.Width, data = iris)

  estim <- suppressMessages(estimate_contrasts(model, backend = "emmeans"))
  expect_identical(attributes(estim)$contrast, "Species")
  expect_null(attributes(estim)$by)
})


test_that("attributes_link", {
  skip_if_not_installed("emmeans")
  model <- lm(Sepal.Length ~ Species * Sepal.Width, data = iris)

  estim <- estimate_link(model)
  expect_identical(attributes(estim)$response, "Sepal.Length")
})
