skip_on_cran()
skip_if_not_installed("pscl")
skip_if_not_installed("glmmTMB")
skip_if_not_installed("emmeans")
skip_if_not_installed("marginaleffects", minimum_version = "0.28.0.22")


test_that("estimate_means - pscl zeroinfl", {
  data(Salamanders, package = "glmmTMB")
  m1 <- pscl::zeroinfl(count ~ mined | mined, dist = "poisson", data = Salamanders)

  # zero-inflated model, response
  estim1 <- estimate_means(m1, "mined", backend = "emmeans")
  estim2 <- as.data.frame(emmeans::emmeans(m1, ~mined, type = "response"))
  estim3 <- estimate_means(m1, "mined", backend = "marginaleffects")
  estim4 <- estimate_relation(m1, by = "mined")
  expect_equal(estim1$Mean, estim4$Predicted, tolerance = 1e-3)
  expect_equal(estim1$Mean, estim2$emmean, tolerance = 1e-3)
  expect_equal(estim1$Mean, estim3$Mean, tolerance = 1e-3)

  # zero-inflated model, count
  estim1 <- estimate_means(m1, "mined", backend = "marginaleffects", predict = "count")
  estim2 <- as.data.frame(emmeans::emmeans(m1, ~mined, mode = "count"))
  expect_equal(estim1$Mean, estim2$emmean, tolerance = 1e-3)

  # zero-inflated model, ZI probabilities
  estim1 <- estimate_means(m1, "mined", backend = "marginaleffects", predict = "zero")
  estim2 <- as.data.frame(emmeans::emmeans(m1, ~mined, mode = "zero"))
  estim4 <- estimate_relation(m1, by = "mined", predict = "zero")
  expect_equal(estim1$Probability, estim4$Predicted, tolerance = 1e-3)
  expect_equal(estim1$Probability, estim2$emmean, tolerance = 1e-3)
})


test_that("estimate_means - pscl hurdle", {
  data(Salamanders, package = "glmmTMB")
  m1 <- pscl::hurdle(count ~ mined | mined, dist = "poisson", zero.dist = "poisson", data = Salamanders)

  # zero-inflated model, response
  estim1 <- estimate_means(m1, "mined", backend = "emmeans")
  estim2 <- as.data.frame(emmeans::emmeans(m1, ~mined, type = "response"))
  estim3 <- estimate_means(m1, "mined", backend = "marginaleffects")
  estim4 <- estimate_relation(m1, by = "mined")
  expect_equal(estim1$Mean, estim4$Predicted, tolerance = 1e-3)
  expect_equal(estim1$Mean, estim2$emmean, tolerance = 1e-3)
  expect_equal(estim1$Mean, estim3$Mean, tolerance = 1e-3)

  # zero-inflated model, count
  estim1 <- estimate_means(m1, "mined", backend = "marginaleffects", predict = "count")
  estim2 <- as.data.frame(emmeans::emmeans(m1, ~mined, mode = "count"))
  expect_equal(estim1$Mean, estim2$emmean, tolerance = 1e-3)

  # zero-inflated model, ZI probabilities
  estim1 <- estimate_means(m1, "mined", backend = "marginaleffects", predict = "zero")
  estim2 <- as.data.frame(emmeans::emmeans(m1, ~mined, mode = "zero"))
  estim4 <- estimate_relation(m1, by = "mined", predict = "zero")
  expect_equal(estim1$Probability, estim4$Predicted, tolerance = 1e-3)
  expect_equal(estim1$Probability, estim2$emmean, tolerance = 1e-3)
})


test_that("estimate_means - pscl hurdle-2", {
  data(Salamanders, package = "glmmTMB")
  m1 <- pscl::hurdle(count ~ mined | mined, dist = "poisson", zero.dist = "binomial", data = Salamanders)

  # zero-inflated model, response
  estim1 <- estimate_means(m1, "mined", backend = "emmeans")
  estim2 <- as.data.frame(emmeans::emmeans(m1, ~mined, type = "response"))
  estim3 <- estimate_means(m1, "mined", backend = "marginaleffects")
  estim4 <- estimate_relation(m1, by = "mined")
  expect_equal(estim1$Mean, estim4$Predicted, tolerance = 1e-3)
  expect_equal(estim1$Mean, estim2$emmean, tolerance = 1e-3)
  expect_equal(estim1$Mean, estim3$Mean, tolerance = 1e-3)

  # zero-inflated model, count
  estim1 <- estimate_means(m1, "mined", backend = "marginaleffects", predict = "count")
  estim2 <- as.data.frame(emmeans::emmeans(m1, ~mined, mode = "count"))
  expect_equal(estim1$Mean, estim2$emmean, tolerance = 1e-3)

  # zero-inflated model, ZI probabilities
  estim1 <- estimate_means(m1, "mined", backend = "marginaleffects", predict = "zero")
  estim2 <- as.data.frame(emmeans::emmeans(m1, ~mined, mode = "zero"))
  estim4 <- estimate_relation(m1, by = "mined", predict = "zero")
  expect_equal(estim1$Probability, estim4$Predicted, tolerance = 1e-3)
  expect_equal(estim1$Probability, estim2$emmean, tolerance = 1e-3)
})


test_that("estimate_means - pscl hurdle-3", {
  data(Salamanders, package = "glmmTMB")
  m1 <- pscl::hurdle(count ~ mined | mined, dist = "poisson", zero.dist = "binomial", link = "log", data = Salamanders)

  # zero-inflated model, response
  estim1 <- estimate_means(m1, "mined", backend = "emmeans")
  estim2 <- as.data.frame(emmeans::emmeans(m1, ~mined, type = "response"))
  estim3 <- estimate_means(m1, "mined", backend = "marginaleffects")
  estim4 <- estimate_relation(m1, by = "mined")
  expect_equal(estim1$Mean, estim4$Predicted, tolerance = 1e-3)
  expect_equal(estim1$Mean, estim2$emmean, tolerance = 1e-3)
  expect_equal(estim1$Mean, estim3$Mean, tolerance = 1e-3)

  # zero-inflated model, count
  estim1 <- estimate_means(m1, "mined", backend = "marginaleffects", predict = "count")
  estim2 <- as.data.frame(emmeans::emmeans(m1, ~mined, mode = "count"))
  expect_equal(estim1$Mean, estim2$emmean, tolerance = 1e-3)

  # zero-inflated model, ZI probabilities
  estim1 <- estimate_means(m1, "mined", backend = "marginaleffects", predict = "zero")
  estim2 <- as.data.frame(emmeans::emmeans(m1, ~mined, mode = "zero"))
  estim4 <- estimate_relation(m1, by = "mined", predict = "zero")
  expect_equal(estim1$Probability, estim4$Predicted, tolerance = 1e-3)
  expect_equal(estim1$Probability, estim2$emmean, tolerance = 1e-3)
})


test_that("estimate_means - pscl zeroinfl-2", {
  data(Salamanders, package = "glmmTMB")
  m1 <- suppressWarnings(pscl::zeroinfl(count ~ mined | mined, dist = "negbin", link = "log", data = Salamanders))

  # zero-inflated model, response
  estim1 <- estimate_means(m1, "mined", backend = "emmeans")
  estim2 <- as.data.frame(emmeans::emmeans(m1, ~mined, type = "response"))
  estim3 <- estimate_means(m1, "mined", backend = "marginaleffects")
  estim4 <- estimate_relation(m1, by = "mined")
  expect_equal(estim1$Mean, estim4$Predicted, tolerance = 1e-3)
  expect_equal(estim1$Mean, estim2$emmean, tolerance = 1e-3)
  expect_equal(estim1$Mean, estim3$Mean, tolerance = 1e-3)


  # zero-inflated model, count
  estim1 <- estimate_means(m1, "mined", backend = "marginaleffects", predict = "count")
  estim2 <- as.data.frame(emmeans::emmeans(m1, ~mined, mode = "count"))
  expect_equal(estim1$Mean, estim2$emmean, tolerance = 1e-3)

  # zero-inflated model, ZI probabilities
  estim1 <- estimate_means(m1, "mined", backend = "marginaleffects", predict = "zero")
  estim2 <- as.data.frame(emmeans::emmeans(m1, ~mined, mode = "zero"))
  estim4 <- estimate_relation(m1, by = "mined", predict = "zero")
  expect_equal(estim1$Probability, estim4$Predicted, tolerance = 1e-3)
  expect_equal(estim1$Probability, estim2$emmean, tolerance = 1e-3)
})
