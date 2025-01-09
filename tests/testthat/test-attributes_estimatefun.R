skip_if_not_installed("emmeans")
skip_if_not_installed("marginaleffects")

test_that("attributes_means", {
  model <- lm(Sepal.Length ~ Species + Sepal.Width, data = iris)

  estim <- suppressMessages(estimate_means(model, "Species"))
  expect_named(
    attributes(estim),
    c(
      "names", "row.names", "class", "at", "by", "table_title", "table_footer",
      "model", "response", "ci", "backend", "coef_name", "predict",  "focal_terms"
    )
  )
  estim <- suppressMessages(estimate_means(model, "Species", backend = "marginaleffects"))
  expect_named(
    attributes(estim),
    c(
      "names", "class", "row.names", "by", "at", "predict", "datagrid",
      "focal_terms", "table_title", "table_footer", "model", "response",
      "ci", "backend", "coef_name"
    )
  )
})


test_that("attributes_means, contrasts", {
  model <- lm(Sepal.Length ~ Species + Sepal.Width, data = iris)

  estim <- suppressMessages(estimate_contrasts(model, "Species"))
  expect_named(
    attributes(estim),
    c(
      "names", "class", "row.names", "table_title", "table_footer",
      "model", "response", "ci", "p_adjust", "backend", "contrast", "predict"
    )
  )
  estim <- suppressMessages(estimate_contrasts(model, "Species", backend = "marginaleffects"))
  expect_named(
    attributes(estim),
    c(
      "names", "row.names", "datagrid", "focal_terms", "table_title",
      "table_footer", "model", "response", "ci", "backend", "coef_name",
      "class", "contrast", "p_adjust"
    )
  )
})


test_that("attributes_means, slopes", {
  model <- lm(Sepal.Length ~ Species + Sepal.Width, data = iris)

  estim <- suppressMessages(estimate_slopes(model, "Sepal.Width"))
  expect_named(
    attributes(estim),
    c(
      "names", "row.names", "class", "table_title", "table_footer",
      "model", "response", "ci", "focal_terms", "trend", "coef_name"
    )
  )
  ## FIXME: should be more attributes here
  estim <- suppressMessages(estimate_slopes(model, "Sepal.Width", backend = "marginaleffects"))
  expect_named(
    attributes(estim),
    c(
      "names", "class", "row.names", "trend", "table_title", "table_footer",
      "model", "response", "ci", "coef_name"
    )
  )
})
