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
      "predict", "estimate", "transform", "datagrid", "preserve_range",
      "model_info", "table_title", "table_footer", "model", "response", "ci",
      "backend", "coef_name"
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
      "adjusted_for", "predict", "comparison", "contrast", "estimate",
      "transform", "datagrid", "preserve_range", "coef_name", "model_info"
    )
  )
  estim <- suppressMessages(estimate_contrasts(model, "Species=c('setosa','virginica')", backend = "marginaleffects"))
  expect_named(
    attributes(estim),
    c(
      "names", "row.names", "class", "table_title", "table_footer",
      "model", "response", "ci", "p_adjust", "backend", "focal_terms",
      "adjusted_for", "predict", "comparison", "contrast", "estimate",
      "transform", "datagrid", "preserve_range", "coef_name", "model_info"
    )
  )
  estim <- suppressMessages(estimate_contrasts(model, "Species=c('setosa','virginica')", backend = "marginaleffects", estimate = "average"))
  expect_named(
    attributes(estim),
    c(
      "names", "row.names", "class", "table_title", "table_footer",
      "model", "response", "ci", "p_adjust", "backend", "focal_terms",
      "adjusted_for", "predict", "comparison", "contrast", "estimate",
      "transform", "datagrid", "preserve_range", "coef_name", "model_info",
      "contrast_filter"
    )
  )
})


test_that("attributes_means, slopes", {
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
      "names", "class", "row.names", "trend", "comparison", "p_adjust",
      "transform", "coef_name", "slope", "ci", "model_info", "table_title",
      "table_footer", "model", "response"
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
      "names", "row.names", "class", "ci", "keep_iterations", "response",
      "transform", "model", "datagrid", "focal_terms", "preserve_range",
      "table_title", "coef_name", "model_info", "table_footer", "adjusted_for",
      "at_specs", "at", "by", "reference", "data"
    )
  )
})
