skip_on_cran()
skip_if_not_installed("marginaleffects", minimum_version = "0.29.0")
skip_on_os("mac")
skip_if_not_installed("MatchIt")
skip_if_not_installed("sandwich")

test_that("estimate_contrast, counterfactual", {
  data("lalonde", package = "MatchIt")
  m <- glm(treat ~ age + educ + race + re74, data = lalonde, family = binomial)

  # IPW
  tmp <- marginaleffects::predictions(m, newdata = lalonde)
  lalonde$wts <- ifelse(tmp$treat == 1, 1 / tmp$estimate, 1 / (1 - tmp$estimate))

  mod <- lm(re78 ~ treat * (age + educ + race + re74), data = lalonde, weights = wts)

  out1 <- marginaleffects::avg_comparisons(mod, variables = "treat", wts = "wts", vcov = "HC3")

  out2 <- estimate_contrasts(
    mod,
    contrast = "treat",
    estimate = "population",
    weights = "wts",
    vcov = "HC3"
  )
  expect_equal(out1$estimate, out2$Difference, tolerance = 1e-4)
  expect_named(out2, c("Comparison", "Difference", "SE", "CI_low", "CI_high", "z", "p"))

  out1 <- marginaleffects::avg_comparisons(
    mod,
    variables = "treat",
    by = "treat",
    wts = "wts",
    vcov = "HC3"
  )

  out2 <- estimate_contrasts(
    mod,
    contrast = "treat",
    by = "treat",
    estimate = "population",
    weights = "wts",
    vcov = "HC3"
  )

  expect_equal(out1$estimate, out2$Difference, tolerance = 1e-4)
  expect_named(
    out2,
    c("treat", "Comparison", "Difference", "SE", "CI_low", "CI_high", "z", "p")
  )
})
