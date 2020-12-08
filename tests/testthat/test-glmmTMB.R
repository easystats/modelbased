if (require("testthat") && require("modelbased") && require("glmmTMB") && require("emmeans")) {

  # testthat::skip_on_cran()

  data <- glmmTMB::Salamanders
  model <- glmmTMB::glmmTMB(
    count ~ mined + (1 | site),
    ziformula = ~mined,
    family = poisson,
    data = data
  )

  model2 <- glmmTMB::glmmTMB(
    count ~ cover + mined + (1 | site),
    ziformula = ~ cover + mined,
    family = glmmTMB::truncated_poisson,
    data = data
  )

  test_that("estimate_means - glmmTMB", {
    estim <- estimate_means(model)
    estim2 <- as.data.frame(emmeans::emmeans(model, ~mined, type = "response"))
    testthat::expect_equal(estim$rate, estim2$rate, tolerance = 1e-3)

    estim <- estimate_means(model, component = "zi")
    estim2 <- as.data.frame(emmeans::emmeans(model, ~mined, component = "zi", type = "response"))
    testthat::expect_equal(estim$rate, estim2$rate, tolerance = 1e-3)
  })

  test_that("estimate_contrasts - glmmTMB", {
    estim <- estimate_contrasts(model)
    testthat::expect_equal(c(nrow(estim), ncol(estim)), c(1, 10))
    testthat::expect_equal(estim$Difference, -1.141923, tolerance = 1e-1)
    testthat::expect_equal(c(estim$Level1[1], estim$Level2[1]), c("yes", "no"))

    estim <- estimate_contrasts(model, component = "zi")
    testthat::expect_equal(c(nrow(estim), ncol(estim)), c(1, 10))
    testthat::expect_equal(estim$Difference, 1.736067, tolerance = 1e-1)
    testthat::expect_equal(c(estim$Level1[1], estim$Level2[1]), c("yes", "no"))
  })

  test_that("estimate_slope - glmmTMB", {
    estim <- estimate_slopes(model2, trend = "cover")
    estim2 <- as.data.frame(emmeans::emtrends(model2, "mined", var = "cover", transform = "response"))
    testthat::expect_equal(estim$Coefficient, estim2$cover.trend, tolerance = 1e-2)
  })

  test_that("estimate_smooth - glmmTMB", {
    model <- glmmTMB::glmmTMB(Sepal.Width ~ poly(Petal.Length, 2) + (1|Species), data = iris)
    estim <- estimate_smooth(model, smooth = "Petal.Length")
  })

  test_that("estimate_response - glmmTMB", {
    estim <- estimate_response(model2)
    testthat::expect_equal(c(nrow(estim), ncol(estim)), c(nrow(data), 6))
  })

  test_that("estimate_link - glmmTMB", {
    estim <- estimate_link(model2)
    testthat::expect_equal(c(nrow(estim), ncol(estim)), c(35, 6))
  })
}
