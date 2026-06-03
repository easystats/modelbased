skip_on_cran()
skip_on_os("mac")
skip_if(getRversion() < "4.5.0")
skip_if_not_installed("marginaleffects", minimum_version = "0.29.0")
skip_if_not_installed("lme4")
skip_if_not_installed("datawizard")
skip_if_not_installed("parameters")

test_that("estimate_contrast, post_process", {
  data("qol_cancer", package = "parameters")
  model <- lme4::lmer(QoL ~ time * education + (1 + time | ID), data = qol_cancer)

  out1 <- estimate_contrasts(model, "education=c('low', 'mid')", by = "time")
  expect_named(
    out1,
    c("Level1", "Level2", "time", "Difference", "SE", "CI_low", "CI_high", "t", "df", "p")
  )
  expect_equal(out1$Difference, c(6.0324, 8.8203, 11.6082), tolerance = 1e-3)

  out2 <- estimate_contrasts(
    model,
    "education=c('low', 'mid')",
    by = "time",
    post_process = ~reference
  )
  expect_named(out2, c("Parameter", "Difference", "SE", "CI_low", "CI_high", "z", "p"))
  expect_equal(out2$Difference, c(2.7879, 5.5758), tolerance = 1e-3, ignore_attr = TRUE)
  expect_identical(out2$Parameter, c("mid - low, 2 - 1", "mid - low, 3 - 1"))

  out3 <- estimate_contrasts(
    model,
    "education=c('low', 'mid')",
    by = "time",
    post_process = list(~reference, ~poly)
  )
  expect_named(out3, c("Parameter", "Difference", "SE", "CI_low", "CI_high", "z", "p"))
  expect_equal(out3$Difference, 1.9713, tolerance = 1e-3, ignore_attr = TRUE)
  expect_identical(out3$Parameter, "Linear")
})
