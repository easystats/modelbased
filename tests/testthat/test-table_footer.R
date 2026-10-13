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
  m_lm <- lm(mpg ~ wt, data = mtcars)
  m_glm <- glm(vs ~ wt, data = mtcars, family = binomial())

  out <- utils::capture.output(estimate_prediction(m_lm, data = "grid", length = 3))
  expect_identical(sum(out == pi_line), 1L)
  out <- utils::capture.output(estimate_prediction(m_glm, data = "grid", length = 3))
  expect_identical(sum(out == pi_line), 1L)
  out <- utils::capture.output(estimate_relation(m_lm, predict = "prediction", length = 3))
  expect_identical(sum(out == pi_line), 1L)

  # no intervals, no line
  out <- utils::capture.output(estimate_prediction(m_lm, data = "grid", length = 3, ci = NULL))
  expect_false(pi_line %in% out)

  # no prediction intervals
  for (m in list(m_lm, m_glm)) {
    out <- utils::capture.output(estimate_expectation(m, data = "grid", length = 3))
    expect_false(pi_line %in% out)
    out <- utils::capture.output(estimate_relation(m, length = 3))
    expect_false(pi_line %in% out)
    out <- utils::capture.output(estimate_link(m, length = 3))
    expect_false(pi_line %in% out)
  }

  skip_if_not_installed("lme4")
  m_lmer <- lme4::lmer(mpg ~ wt + (1 | cyl), data = mtcars)
  out <- utils::capture.output(estimate_prediction(m_lmer, data = "grid", length = 3))
  expect_identical(sum(out == pi_line), 1L)
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
