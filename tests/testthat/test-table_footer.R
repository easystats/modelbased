skip_if_not_installed("marginaleffects")
skip_if_not_installed("emmeans")

test_that("table_footer", {
  skip_if_not_installed("ggeffects")
  data(efc, package = "ggeffects")
  efc <- datawizard::to_factor(efc, c("c161sex", "c172code", "e16sex", "e42dep"))
  m <- lm(neg_c_7 ~ c12hour + barthtot + c161sex + e42dep + c172code, data = efc)
  out <- utils::capture.output(estimate_means(m, "c172code", marginalize = "specific", verbose = FALSE))
  expect_identical(
    out[11],
    "Predictors controlled: c12hour (42), barthtot (65), c161sex (Male), e42dep (independent)"
  )
  out <- utils::capture.output(estimate_means(m, "c172code", marginalize = "average", verbose = FALSE))
  expect_identical(
    out[11],
    "Predictors averaged: c12hour (42), barthtot (65), c161sex, e42dep"
  )
})
