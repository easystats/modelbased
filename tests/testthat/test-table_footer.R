skip_if_not_installed("marginaleffects")
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
