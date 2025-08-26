skip_if_not_installed("marginaleffects", minimum_version = "0.28.0.22")
skip_if_not_installed("emmeans")

test_that("verbose", {
  data(iris)
  model <- lm(Sepal.Width ~ Species, data = iris)
  expect_message(estimate_contrasts(model, backend = "emmeans"), regex = "No variable was specified")
  expect_silent(estimate_contrasts(model, backend = "emmeans", verbose = FALSE))
  expect_message(estimate_means(model, backend = "emmeans"), regex = "We selected")
  expect_silent(estimate_means(model, backend = "emmeans", verbose = FALSE))
  expect_silent(estimate_contrasts(model, backend = "marginaleffects", verbose = FALSE))
  expect_message(estimate_means(model, backend = "marginaleffects"), regex = "We selected")
  expect_silent(estimate_means(model, backend = "marginaleffects", verbose = FALSE))
})
