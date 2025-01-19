skip_if_not_installed("emmeans")
skip_if_not_installed("marginaleffects")

test_that("estimate_means() - mixed models", {
  skip_if_not_installed("emmeans")
  skip_if_not_installed("lme4")
  skip_if_not_installed("glmmTMB")

  data(iris)
  dat <- iris
  dat$Petal.Length_factor <- as.factor(ifelse(dat$Petal.Length < 4.2, "A", "B"))
  dat <<- dat

  model <- lme4::lmer(Sepal.Width ~ Species + (1 | Petal.Length_factor), data = dat)

  estim1 <- estimate_means(model, backend = "emmeans", verbose = FALSE)
  expect_identical(dim(estim1), c(3L, 5L))
  estim2 <- estimate_means(model, backend = "marginaleffects", verbose = FALSE)
  expect_identical(dim(estim2), c(3L, 7L))
  expect_lt(max(estim1$Mean - estim2$Mean), 1e-10)

  model <- lme4::glmer(Sepal.Width ~ Species + (1 | Petal.Length_factor), data = dat, family = "Gamma")
  estim1 <- estimate_means(model, backend = "emmeans", verbose = FALSE)
  expect_identical(dim(estim1), c(3L, 5L))
  estim2 <- estimate_means(model, backend = "marginaleffects", verbose = FALSE)
  expect_identical(dim(estim2), c(3L, 7L))
  expect_lt(max(estim1$Mean - estim2$Mean), 1e-10)

  data(Salamanders, package = "glmmTMB")
  m <- glmmTMB::glmmTMB(
    count ~ mined * spp + (1 | site),
    zi = ~mined,
    family = poisson(),
    data = Salamanders
  )
  expect_snapshot(estimate_means(m, c("mined", "spp"), backend = "marginaleffects"))
  out1 <- estimate_means(m, c("mined", "spp"), type = "conditional", backend = "marginaleffects")
  out2 <- estimate_means(m, c("mined", "spp"), backend = "emmeans")
  expect_equal(out1$Mean[order(out1$spp)], out2$Rate, tolerance = 1e-1)

  m <- glm(count ~ mined + spp, family = poisson(), data = Salamanders)
  expect_snapshot(estimate_means(m, c("mined", "spp"), backend = "marginaleffects"))
  out1 <- estimate_means(m, c("mined", "spp"), backend = "marginaleffects")
  out2 <- estimate_means(m, c("mined", "spp"), backend = "emmeans")
  expect_equal(out1$Mean[order(out1$spp)], out2$Rate, tolerance = 1e-3)

  data(sleepstudy, package = "lme4")
  model <- lme4::lmer(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)

  estim1 <- estimate_means(model, by = "Days", backend = "emmeans")
  estim2 <- estimate_means(model, by = "Days", backend = "marginaleffects")
  expect_identical(dim(estim1), c(10L, 5L))
  expect_identical(dim(estim2), c(10L, 7L))
  expect_equal(estim1$Mean, estim2$Mean, tolerance = 1e-4)
  expect_equal(estim1$CI_low, estim2$CI_low, tolerance = 1e-1)

  data(cbpp, package = "lme4")
  gm1 <- lme4::glmer(
    cbind(incidence, size - incidence) ~ period + (1 | herd),
    data = cbpp,
    family = "binomial"
  )
  estim1 <- estimate_means(gm1, backend = "emmeans", verbose = FALSE)
  estim2 <- estimate_means(gm1, backend = "marginaleffects", verbose = FALSE)
  expect_identical(dim(estim1), c(4L, 5L))
  expect_identical(dim(estim2), c(4L, 6L))
  expect_equal(estim2$Probability, c(0.21521, 0.0954, 0.08453, 0.05599), tolerance = 1e-3)
  expect_equal(estim2$CI_low, c(0.14233, 0.04475, 0.03608, 0.01266), tolerance = 1e-3)
})
