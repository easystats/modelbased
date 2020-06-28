if (require("testthat") && require("modelbased") && require("glmmTMB") && require("emmeans")) {
  data <- glmmTMB::Salamanders
  model <- glmmTMB::glmmTMB(
    count ~ mined + (1 | site),
    ziformula = ~mined,
    family = poisson,
    data = data
  )

  test_that("estimate_means - glmmTMB", {
    estim <- estimate_means(model)
    estim2 <- as.data.frame(emmeans::emmeans(model, ~mined, type = "response"))
    testthat::expect_equal(estim$rate, rev(estim2$rate), tolerance = 1e-3)

    estim <- estimate_means(model, component = "zi")
    estim2 <- as.data.frame(emmeans::emmeans(model, ~mined, component = "zi", type = "response"))
    testthat::expect_equal(estim$rate, rev(estim2$rate), tolerance = 1e-3)
  })

  test_that("estimate_contrasts - glmmTMB", {
    estim <- estimate_contrasts(model)
    testthat::expect_equal(c(nrow(estim), ncol(estim)), c(1, 10))

    estim <- estimate_contrasts(model, component = "zi")
    testthat::expect_equal(c(nrow(estim), ncol(estim)), c(1, 10))
  })

  test_that("estimate_slope - glmmTMB", {
    model <- glmmTMB::glmmTMB(
      count ~ cover + mined + (1 | site),
      ziformula = ~ cover + mined,
      family = glmmTMB::truncated_poisson,
      data = data
    )

    estim <- estimate_slopes(model, trend = "cover")
    estim2 <- as.data.frame(emmeans::emtrends(model, "mined", var = "cover", transform = "response"))
    testthat::expect_equal(estim$Coefficient, estim2$cover.trend, tolerance = 1e-3)
  })
}
