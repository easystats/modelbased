if (require("testthat") && require("modelbased") && require("glmmTMB") && require("emmeans")) {
  data(Salamanders)
  model <- glmmTMB(
    count ~ cover + mined + (1 | site),
    ziformula = ~ cover + mined,
    family = truncated_poisson,
    data = Salamanders
  )

  test_that("estimate_slope", {
    estim <- estimate_slopes(model, trend = "cover")
    # estim2 <- as.data.frame(emmeans::emtrends(model, "mined", var = "cover", transform = "response"))
    # testthat::expect_equal(estim$rate, rev(estim2$rate), tolerance = 1e-3)
  })
}
