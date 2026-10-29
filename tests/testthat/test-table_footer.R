skip_if_not_installed("marginaleffects", minimum_version = "0.29.0")
skip_if_not_installed("emmeans")

test_that("table_footer", {
  data(efc, package = "modelbased")
  efc <- datawizard::to_factor(efc, c("c161sex", "c172code", "e16sex", "e42dep"))
  m <- lm(neg_c_7 ~ c12hour + barthtot + c161sex + e42dep + c172code, data = efc)
  out <- utils::capture.output(estimate_means(m, "c172code", estimate = "specific", verbose = FALSE))
  expect_identical(
    out[11],
    "Predictors controlled: c12hour (42), barthtot (65), c161sex (Male), e42dep (independent)"
  )
  out <- utils::capture.output(estimate_means(m, "c172code", estimate = "typical", verbose = FALSE))
  expect_identical(
    out[11],
    "Predictors averaged: c12hour (42), barthtot (65), c161sex, e42dep"
  )
  out <- utils::capture.output(estimate_means(m, "c172code", estimate = "average", verbose = FALSE))
  expect_length(out, 10)
})


test_that("table_footer, prediction intervals", {
  pi_line <- "Intervals are prediction intervals."
  m <- lm(mpg ~ wt, data = mtcars)
  out <- utils::capture.output(estimate_prediction(m, data = "grid", length = 3))
  expect_identical(sum(out == pi_line), 1L)
  out <- utils::capture.output(estimate_relation(m, predict = "prediction", length = 3))
  expect_identical(sum(out == pi_line), 1L)

  out <- utils::capture.output(estimate_prediction(
    m,
    data = "grid",
    length = 3,
    ci = NULL
  ))
  expect_false(pi_line %in% out)
  out <- utils::capture.output(estimate_expectation(m, data = "grid", length = 3))
  expect_false(pi_line %in% out)
})


test_that("table_footer, link-scale and transform", {
  d <- mtcars
  d$cyl <- factor(d$cyl)
  g <- glm(am ~ wt + cyl, data = d, family = binomial)

  out <- utils::capture.output(estimate_means(
    g,
    "cyl",
    predict = "link",
    backend = "emmeans"
  ))
  expect_true("Predictions are on the link-scale." %in% out)
  out <- utils::capture.output(estimate_contrasts(
    g,
    "cyl",
    predict = "link",
    backend = "emmeans"
  ))
  expect_true("Contrasts are on the link-scale." %in% out)

  out <- utils::capture.output(estimate_contrasts(
    g,
    "cyl",
    predict = "link",
    transform = exp
  ))
  expect_true("Contrasts are on the transformed link-scale." %in% out)
  out <- utils::capture.output(estimate_contrasts(
    g,
    "cyl",
    predict = "link",
    transform = "exp"
  ))
  expect_true("Contrasts are on the exp-transformed link-scale." %in% out)

  # the response is not transformed, so `transform = TRUE` changes nothing
  out <- utils::capture.output(estimate_link(g, by = "cyl", transform = TRUE))
  expect_true("Predictions are on the link-scale." %in% out)
  out <- utils::capture.output(estimate_means(
    g,
    "cyl",
    predict = "link",
    transform = TRUE
  ))
  expect_true("Predictions are on the link-scale." %in% out)

  out <- utils::capture.output(estimate_expectation(
    g,
    data = estimate_link(g, by = "cyl")
  ))
  expect_true("Predictions are on the response-scale." %in% out)
  out <- utils::capture.output(estimate_link(
    g,
    data = estimate_expectation(g, by = "cyl")
  ))
  expect_true("Predictions are on the link-scale." %in% out)

  out <- utils::capture.output(estimate_contrasts(estimate_link(
    g,
    by = "cyl",
    transform = exp
  )))
  expect_true("Contrasts are on the transformed link-scale." %in% out)
  out <- utils::capture.output(
    estimate_contrasts(g, "wt", by = "cyl", predict = "link", transform = exp)
  )
  expect_true("Contrasts are on the transformed link-scale." %in% out)
})


test_that("table_footer, no prediction intervals if insight falls back", {
  skip_if_not_installed("mgcv")
  m <- mgcv::gam(mpg ~ s(wt), data = mtcars)
  # insight returns expectations for gam-models, even if prediction is requested
  predict_type <- attr(
    suppressWarnings(insight::get_predicted(m, predict = "prediction", ci = 0.95)),
    "predict"
  )
  skip_if(identical(predict_type, "prediction"))
  out <- utils::capture.output(suppressWarnings(estimate_prediction(m, data = "grid", length = 3)))
  expect_false("Intervals are prediction intervals." %in% out)
})
