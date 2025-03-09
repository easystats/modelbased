skip_on_cran()
skip_if_not_installed("marginaleffects")
skip_if_not_installed("emmeans")
skip_if_not_installed("brms")
skip_if_not_installed("curl")
skip_if_offline()
skip_if_not_installed("httr2")

test_that("estimate_means() - posterior draws", {
  m <- insight::download_model("brms_1")
  skip_if(is.null(m))
  out <- estimate_means(m, by = "wt", keep_iterations = 5)
  expect_named(
    attributes(out),
    c(
      "names", "class", "row.names", "at", "by", "focal_terms", "adjusted_for",
      "predict", "estimate", "transform", "datagrid", "preserve_range",
      "model_info", "keep_iterations", "posterior_draws", "table_title",
      "table_footer", "model", "response", "ci", "backend", "coef_name"
    )
  )
  expect_named(
    out,
    c(
      "wt", "ROPE_CI", "Median", "CI_low", "CI_high", "pd", "ROPE_low",
      "ROPE_high", "ROPE_Percentage", "iter_1", "iter_2", "iter_3",
      "iter_4", "iter_5"
    )
  )
  expect_identical(dim(out), c(10L, 14L))

  out <- estimate_means(m, by = "wt", keep_iterations = TRUE)
  expect_named(
    attributes(out),
    c(
      "names", "class", "row.names", "at", "by", "focal_terms", "adjusted_for",
      "predict", "estimate", "transform", "datagrid", "preserve_range",
      "model_info", "keep_iterations", "posterior_draws", "table_title",
      "table_footer", "model", "response", "ci", "backend", "coef_name"
    )
  )
  expect_named(
    out,
    c(
      "wt", "ROPE_CI", "Median", "CI_low", "CI_high", "pd", "ROPE_low",
      "ROPE_high", "ROPE_Percentage"
    )
  )
  expect_identical(dim(out), c(10L, 9L))
})


test_that("estimate_contrasts() - posterior draws", {
  m <- insight::download_model("brms_1")
  skip_if(is.null(m))
  out <- estimate_contrasts(m, "wt=c(3,4,5)", keep_iterations = 5)
  expect_named(
    attributes(out),
    c(
      "names", "row.names", "class", "table_title", "table_footer",
      "model", "response", "ci", "p_adjust", "backend", "focal_terms",
      "adjusted_for", "predict", "comparison", "contrast", "estimate",
      "transform", "datagrid", "preserve_range", "coef_name", "model_info",
      "keep_iterations", "posterior_draws"
    )
  )
  expect_named(
    out,
    c(
      "Level1", "Level2", "ROPE_CI", "Median", "CI_low", "CI_high",
      "pd", "ROPE_low", "ROPE_high", "ROPE_Percentage", "iter_1", "iter_2",
      "iter_3", "iter_4", "iter_5"
    )
  )
  expect_identical(dim(out), c(3L, 15L))

  out <- estimate_contrasts(m, "wt=c(3,4,5)", keep_iterations = TRUE)
  expect_named(
    attributes(out),
    c(
      "names", "row.names", "class", "table_title", "table_footer",
      "model", "response", "ci", "p_adjust", "backend", "focal_terms",
      "adjusted_for", "predict", "comparison", "contrast", "estimate",
      "transform", "datagrid", "preserve_range", "coef_name", "model_info",
      "keep_iterations", "posterior_draws"
    )
  )
  expect_named(
    out,
    c(
      "Level1", "Level2", "ROPE_CI", "Median", "CI_low", "CI_high",
      "pd", "ROPE_low", "ROPE_high", "ROPE_Percentage"
    )
  )
  expect_identical(dim(out), c(3L, 10L))
})


test_that("estimate_slopes() - posterior draws", {
  m <- insight::download_model("brms_1")
  skip_if(is.null(m))
  out <- estimate_slopes(m, "wt", keep_iterations = 5)
  expect_named(
    attributes(out),
    c(
      "names", "class", "row.names", "trend", "comparison", "p_adjust",
      "transform", "coef_name", "slope", "ci", "model_info", "keep_iterations",
      "posterior_draws", "table_title", "table_footer", "model", "response"
    )
  )
  expect_named(
    out,
    c(
      "ROPE_CI", "Median", "CI_low", "CI_high", "pd", "ROPE_low",
      "ROPE_high", "ROPE_Percentage", "iter_1", "iter_2", "iter_3",
      "iter_4", "iter_5"
    )
  )
  expect_identical(dim(out), c(1L, 13L))

  out <- estimate_slopes(m, "wt", keep_iterations = TRUE)
  expect_named(
    attributes(out),
    c(
      "names", "class", "row.names", "trend", "comparison", "p_adjust",
      "transform", "coef_name", "slope", "ci", "model_info", "keep_iterations",
      "posterior_draws", "table_title", "table_footer", "model", "response"
    )
  )
  expect_named(
    out,
    c(
      "ROPE_CI", "Median", "CI_low", "CI_high", "pd", "ROPE_low",
      "ROPE_high", "ROPE_Percentage"
    )
  )
  expect_identical(dim(out), c(1L, 8L))
})


test_that("estimate_means() - posterior draws, emmeans", {
  m <- insight::download_model("brms_1")
  skip_if(is.null(m))
  out <- estimate_means(m, by = "wt", keep_iterations = 5, backend = "emmeans")
  expect_named(
    attributes(out),
    c(
      "names", "class", "row.names", "at", "by", "focal_terms", "adjusted_for",
      "predict", "estimate", "transform", "datagrid", "preserve_range",
      "model_info", "keep_iterations", "posterior_draws", "table_title",
      "table_footer", "model", "response", "ci", "backend", "coef_name"
    )
  )
  expect_named(
    out,
    c(
      "wt", "ROPE_CI", "Median", "CI_low", "CI_high", "pd", "ROPE_low",
      "ROPE_high", "ROPE_Percentage", "iter_1", "iter_2", "iter_3",
      "iter_4", "iter_5"
    )
  )
  expect_identical(dim(out), c(10L, 14L))

  out <- estimate_means(m, by = "wt", keep_iterations = TRUE, backend = "emmeans")
  expect_named(
    attributes(out),
    c(
      "names", "class", "row.names", "at", "by", "focal_terms", "adjusted_for",
      "predict", "estimate", "transform", "datagrid", "preserve_range",
      "model_info", "keep_iterations", "posterior_draws", "table_title",
      "table_footer", "model", "response", "ci", "backend", "coef_name"
    )
  )
  expect_named(
    out,
    c(
      "wt", "ROPE_CI", "Median", "CI_low", "CI_high", "pd", "ROPE_low",
      "ROPE_high", "ROPE_Percentage"
    )
  )
  expect_identical(dim(out), c(10L, 9L))
})


test_that("estimate_contrasts() - posterior draws, emmeans", {
  m <- insight::download_model("brms_1")
  skip_if(is.null(m))
  out <- estimate_contrasts(m, "wt=c(3,4,5)", keep_iterations = 5, backend = "emmeans")
  expect_named(
    attributes(out),
    c(
      "names", "row.names", "class", "table_title", "table_footer",
      "model", "response", "ci", "p_adjust", "backend", "focal_terms",
      "adjusted_for", "predict", "comparison", "contrast", "estimate",
      "transform", "datagrid", "preserve_range", "coef_name", "model_info",
      "keep_iterations", "posterior_draws"
    )
  )
  expect_named(
    out,
    c(
      "Level1", "Level2", "ROPE_CI", "Median", "CI_low", "CI_high",
      "pd", "ROPE_low", "ROPE_high", "ROPE_Percentage", "iter_1", "iter_2",
      "iter_3", "iter_4", "iter_5"
    )
  )
  expect_identical(dim(out), c(3L, 15L))

  out <- estimate_contrasts(m, "wt=c(3,4,5)", keep_iterations = TRUE, backend = "emmeans")
  expect_named(
    attributes(out),
    c(
      "names", "row.names", "class", "table_title", "table_footer",
      "model", "response", "ci", "p_adjust", "backend", "focal_terms",
      "adjusted_for", "predict", "comparison", "contrast", "estimate",
      "transform", "datagrid", "preserve_range", "coef_name", "model_info",
      "keep_iterations", "posterior_draws"
    )
  )
  expect_named(
    out,
    c(
      "Level1", "Level2", "ROPE_CI", "Median", "CI_low", "CI_high",
      "pd", "ROPE_low", "ROPE_high", "ROPE_Percentage"
    )
  )
  expect_identical(dim(out), c(3L, 10L))
})


test_that("estimate_slopes() - posterior draws, emmeans", {
  m <- insight::download_model("brms_1")
  skip_if(is.null(m))
  out <- estimate_slopes(m, "wt", keep_iterations = 5, backend = "emmeans")
  expect_named(
    attributes(out),
    c(
      "names", "class", "row.names", "trend", "comparison", "p_adjust",
      "transform", "coef_name", "slope", "ci", "model_info", "keep_iterations",
      "posterior_draws", "table_title", "table_footer", "model", "response"
    )
  )
  expect_named(
    out,
    c(
      "ROPE_CI", "Median", "CI_low", "CI_high", "pd", "ROPE_low",
      "ROPE_high", "ROPE_Percentage", "iter_1", "iter_2", "iter_3",
      "iter_4", "iter_5"
    )
  )
  expect_identical(dim(out), c(1L, 13L))

  out <- estimate_slopes(m, "wt", keep_iterations = TRUE, backend = "emmeans")
  expect_named(
    attributes(out),
    c(
      "names", "class", "row.names", "trend", "comparison", "p_adjust",
      "transform", "coef_name", "slope", "ci", "model_info", "keep_iterations",
      "posterior_draws", "table_title", "table_footer", "model", "response"
    )
  )
  expect_named(
    out,
    c(
      "ROPE_CI", "Median", "CI_low", "CI_high", "pd", "ROPE_low",
      "ROPE_high", "ROPE_Percentage"
    )
  )
  expect_identical(dim(out), c(1L, 8L))
})


test_that("estimate_slopes() - posterior draws, get_predicted", {
  m <- insight::download_model("brms_1")
  skip_if(is.null(m))
  out <- estimate_relation(m, by = "wt", keep_iterations = 5)
  expect_named(
    attributes(out),
    c(
      "names", "row.names", "class", "ci", "keep_iterations", "posterior_draws",
      "response", "transform", "model", "datagrid", "focal_terms",
      "preserve_range", "table_title", "coef_name", "model_info", "table_footer",
      "adjusted_for", "at_specs", "at", "by", "reference", "data"
    )
  )
  expect_named(
    out,
    c(
      "wt", "cyl", "Predicted", "SE", "CI_low", "CI_high", "iter_1",
      "iter_2", "iter_3", "iter_4", "iter_5"
    )
  )
})
