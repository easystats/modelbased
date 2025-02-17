skip_on_cran()
skip_if_not_installed("emmeans")
skip_if_not_installed("marginaleffects")
skip_if_not_installed("glmmTMB")
skip_if_not_installed("lme4")

test_that("estimate_means() - counterfactuals", {
  data(Salamanders, package = "glmmTMB")
  m <- glmmTMB::glmmTMB(
    count ~ spp + mined + (1 | site),
    family = poisson(),
    data = Salamanders
  )
  expect_snapshot(print(estimate_means(m, "spp", backend = "marginaleffects"), zap_small = TRUE, table_width = Inf), variant = "windows") # nolint
  expect_snapshot(print(estimate_means(m, "spp", backend = "marginaleffects", estimate = "population"), zap_small = TRUE, table_width = Inf), variant = "windows") # nolint

  expect_snapshot(print(estimate_means(m, "spp", backend = "marginaleffects", predict = "inverse_link"), zap_small = TRUE, table_width = Inf), variant = "windows") # nolint
  expect_snapshot(print(estimate_means(m, "spp", backend = "marginaleffects", estimate = "population", predict = "inverse_link"), zap_small = TRUE, table_width = Inf), variant = "windows") # nolint

  out1 <- estimate_means(m, "spp", backend = "marginaleffects", predict = "inverse_link")
  out2 <- estimate_means(m, "spp", backend = "emmeans")
  expect_equal(out1$Mean, out2$Rate, tolerance = 1e-1)

  data(sleepstudy, package = "lme4")
  # create imbalanced data set
  set.seed(123)
  strapped <- sleepstudy[sample.int(nrow(sleepstudy), nrow(sleepstudy), replace = TRUE), ]
  m <- lme4::lmer(Reaction ~ Days + (1 + Days | Subject), data = strapped)
  expect_snapshot(print(estimate_means(m, "Days", backend = "marginaleffects"), zap_small = TRUE, table_width = Inf), variant = "windows") # nolint
  expect_snapshot(print(estimate_means(m, "Days", backend = "marginaleffects", estimate = "population"), zap_small = TRUE, table_width = Inf), variant = "windows") # nolint
})
