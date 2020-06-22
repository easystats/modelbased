if (require("testthat") && require("modelbased") && require("glmmTMB") && require("emmeans")) {
  model <- glmmTMB(
    count ~ mined + (1 | site),
    ziformula = ~mined,
    family = poisson,
    data = Salamanders
  )

  test_that("estimate_means", {
    estim <- estimate_means(model)
    estim2 <- as.data.frame(emmeans(model, ~mined, type = "response"))
    testthat::expect_equal(estim$rate, rev(estim2$rate), tolerance = 1e-3)
  })

  test_that("estimate_means", {
    estim <- estimate_means(model, component = "zi")
    estim2 <- as.data.frame(emmeans(model, ~mined, component = "zi", type = "response"))
    testthat::expect_equal(estim$rate, rev(estim2$rate), tolerance = 1e-3)
  })
}
