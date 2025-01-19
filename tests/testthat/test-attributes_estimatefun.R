skip_if_not_installed("emmeans")
skip_if_not_installed("marginaleffects")

test_that("attributes_means", {
  data(iris)
  model <- lm(Sepal.Length ~ Species + Sepal.Width, data = iris)

  estim <- suppressMessages(estimate_means(model, "Species", backend = "emmeans"))
  expect_named(
    attributes(estim),
    c(
      "names", "row.names", "class", "at", "by", "table_title", "table_footer",
      "model", "response", "ci", "backend", "coef_name", "focal_terms",
      "predict"
    )
  )
  estim <- suppressMessages(estimate_means(model, "Species", backend = "marginaleffects"))
  expect_named(
    attributes(estim),
    c(
      "names", "class", "row.names", "at", "by", "focal_terms", "adjusted_for",
      "predict", "marginalize", "datagrid", "preserve_range", "table_title",
      "table_footer", "model", "response", "ci", "backend", "coef_name"
    )
  )
})


test_that("attributes_means, contrasts", {
  data(iris)
  model <- lm(Sepal.Length ~ Species + Sepal.Width, data = iris)

  estim <- suppressMessages(estimate_contrasts(model, "Species", backend = "emmeans"))
  expect_named(
    attributes(estim),
    c(
      "names", "class", "row.names", "table_title", "table_footer",
      "model", "response", "ci", "p_adjust", "backend", "predict",
      "comparison", "contrast"
    )
  )
  estim <- suppressMessages(estimate_contrasts(model, "Species", backend = "marginaleffects"))
  expect_named(
    attributes(estim),
    c(
      "names", "row.names", "class", "table_title", "table_footer",
      "model", "response", "ci", "p_adjust", "backend", "focal_terms",
      "adjusted_for", "predict", "comparison", "contrast", "marginalize",
      "datagrid", "preserve_range", "coef_name"
    )
  )
})


test_that("attributes_means, slopes", {
  skip_if_not_installed("marginaleffects", minimum_version = "0.24.1")
  data(iris)
  model <- lm(Sepal.Length ~ Species + Sepal.Width, data = iris)

  estim <- suppressMessages(estimate_slopes(model, "Sepal.Width", backend = "emmeans"))
  expect_named(
    attributes(estim),
    c(
      "names", "row.names", "class", "table_title", "table_footer",
      "model", "response", "ci", "trend", "coef_name"
    )
  )
  estim <- suppressMessages(estimate_slopes(model, "Sepal.Width", backend = "marginaleffects"))
  expect_named(
    attributes(estim),
    c(
      "names", "class", "row.names", "trend", "comparison", "coef_name",
      "slope", "table_title", "table_footer", "model", "response", "ci"
    )
  )
})


test_that("attributes_means", {
  data(iris)
  model <- lm(Sepal.Width ~ Petal.Length + Species * Petal.Width, data = iris)
  estim <- estimate_expectation(
    model,
    by = c("Species", "Petal.Width = [fivenum]"),
    preserve_range = FALSE
  )
  expect_named(
    attributes(estim),
    c(
      "names", "class", "row.names", "ci", "keep_iterations", "response",
      "model", "focal_terms", "preserve_range", "table_title", "table_footer",
      "adjusted_for", "at_specs", "at", "by", "reference", "data"
    )
  )
})
