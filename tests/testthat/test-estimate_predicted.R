skip_if_not_installed("insight", minimum_version = "1.1.0")

test_that("estimate_relation - shape", {
  skip_if_not_installed("gamm4")
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("lme4")
  skip_if_not_installed("glmmTMB")
  skip_if_not_installed("mgcv")

  # CI
  model <- lm(Petal.Length ~ Petal.Width, data = iris)
  estim <- estimate_relation(model, ci = 0.90)
  expect_equal(attributes(estim)$ci, 0.9)
  estim <- estimate_relation(model, ci = c(0.90, 0.95))
  expect_equal(attributes(estim)$ci, c(0.90, 0.95))
  expect_equal(dim(estim), c(10, 7))

  # Range
  model <- lm(Petal.Length ~ Petal.Width * Species, data = iris)
  estim <- estimate_relation(model, length = 10)
  expect_equal(dim(estim), c(10, 6))
  estim <- estimate_relation(model, length = 10, preserve_range = FALSE)
  expect_equal(dim(estim), c(30, 6))
})


test_that("estimate_link", {
  skip_if_not_installed("gamm4")
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("lme4")
  skip_if_not_installed("glmmTMB")
  skip_if_not_installed("mgcv")
  # LMER4
  model <- lme4::lmer(Petal.Length ~ Petal.Width + (1 | Species), data = iris)
  expect_equal(nrow(estimate_link(model, length = 5, verbose = FALSE)), 5)
  expect_equal(nrow(estimate_link(model, include_random = TRUE, preserve_range = FALSE, length = 5)), 15)

  # GLMMTMB
  model <- suppressWarnings(glmmTMB::glmmTMB(Petal.Length ~ Petal.Width + (1 | Species), data = iris))
  expect_equal(nrow(estimate_link(model, length = 5, verbose = FALSE)), 5)
  expect_equal(nrow(estimate_link(model, include_random = TRUE, preserve_range = FALSE, length = 5)), 15)

  # MGCV
  model <- mgcv::gam(Petal.Length ~ Petal.Width + s(Sepal.Length), data = iris)
  expect_equal(dim(estimate_link(model, length = 3)), c(9, 6))
  expect_equal(dim(estimate_link(model, include_smooth = FALSE, length = 3)), c(3, 5))

  model <- mgcv::gamm(Petal.Length ~ Petal.Width + s(Sepal.Length), random = list(Species = ~1), data = iris)

  # GAMM4
  model <- gamm4::gamm4(Petal.Length ~ Petal.Width + s(Sepal.Length),
    random = ~ (1 | Species), data = iris
  )
  expect_equal(nrow(estimate_link(model, length = 3, verbose = FALSE)), 9)
  expect_equal(dim(estimate_link(model, include_smooth = FALSE, length = 3, verbose = FALSE)), c(3, 5))

  # STAN_GAMM4
  skip_if_not(.Platform$OS.type == "windows")
  model <- suppressWarnings(rstanarm::stan_gamm4(Petal.Length ~ Petal.Width + s(Sepal.Length),
    random = ~ (1 | Species), data = iris,
    iter = 100, chains = 2, refresh = 0
  ))
  expect_equal(nrow(estimate_relation(model, length = 3)), 9)
  expect_equal(dim(estimate_link(model, include_smooth = FALSE, length = 3)), c(3, 5))
})

test_that("estimate_expectation - Bayesian", {
  skip_if_not_installed("gamm4")
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("lme4")
  skip_if_not_installed("glmmTMB")
  skip_if_not_installed("mgcv")
  model <- suppressWarnings(rstanarm::stan_glm(
    mpg ~ wt + poly(cyl, 2, raw = TRUE),
    data = mtcars,
    refresh = 0,
    iter = 200,
    chains = 2
  ))
  estim <- estimate_prediction(model, seed = 333)
  expect_equal(nrow(estim), nrow(mtcars))

  model <- suppressWarnings(rstanarm::stan_glm(
    mpg ~ wt * as.factor(gear),
    data = mtcars,
    refresh = 0,
    iter = 200,
    chains = 2
  ))
  estim <- estimate_prediction(model,
    data = "grid",
    seed = 333,
    preserve_range = FALSE
  )
  expect_equal(dim(estim), c(30, 6))

  model <- suppressWarnings(rstanarm::stan_glm(
    mpg ~ as.factor(gear) / wt,
    data = mtcars,
    refresh = 0,
    iter = 200,
    chains = 2
  ))
  estim <- estimate_prediction(model)
  expect_equal(dim(estim), c(32, 7))

  model <- suppressWarnings(
    rstanarm::stan_glm(
      Sepal.Width ~ Petal.Width,
      data = iris,
      refresh = 0,
      iter = 200,
      chains = 2
    )
  )
  estim <- estimate_link(model, keep_iterations = TRUE)
  draws <- bayestestR::reshape_iterations(estim)
  expect_equal(c(nrow(draws), ncol(draws)), c(2000, 8))

  # Non-sampling algorithms
  model <- rstanarm::stan_glm(mpg ~ disp, data = mtcars, algorithm = "meanfield", refresh = 0)
  estim <- estimate_link(model, keep_iterations = TRUE)
  expect_equal(dim(estim), c(10, 1005))
})


test_that("estimate_expectation - Frequentist", {
  skip_if_not_installed("gamm4")
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("lme4")
  skip_if_not_installed("glmmTMB")
  skip_if_not_installed("mgcv")

  model <- lm(mpg ~ wt + cyl, data = mtcars)
  estim <- estimate_expectation(model)
  expect_equal(dim(estim), c(32, 7))

  estim <- estimate_expectation(model, ci = NULL)
  expect_equal(dim(estim), c(32, 4))

  model <- glm(vs ~ wt + cyl, data = mtcars, family = "binomial")
  estim <- estimate_link(model, by = "wt")
  expect_equal(dim(estim), c(10, 6))


  data <- mtcars
  data$gear <- as.factor(data$gear)

  model <- lme4::lmer(wt ~ cyl + (1 | gear), data = data)
  estim <- estimate_link(model)
  expect_identical(dim(estim), c(3L, 6L))
  estim <- estimate_expectation(model)
  expect_equal(dim(estim), c(32, 7))

  model <- lme4::glmer(vs ~ cyl + (1 | gear), data = data, family = "binomial")
  estim <- estimate_link(model)
  expect_identical(dim(estim), c(3L, 6L))
  estim <- estimate_expectation(model)
  expect_equal(dim(estim), c(32, 7))
})


test_that("estimate_expectation - VisMatrix", {
  skip_if_not_installed("gamm4")
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("lme4")
  skip_if_not_installed("glmmTMB")
  skip_if_not_installed("mgcv")

  m <- lm(Sepal.Length ~ Petal.Length * Petal.Width, data = iris)
  vm <- insight::get_datagrid(m, by = c("Petal.Length", "Petal.Width = seq(-3, 3)"))
  estim <- estimate_relation(vm)
  expect_identical(dim(estim), c(70L, 6L))
  expect_named(estim, c("Petal.Length", "Petal.Width", "Predicted", "SE", "CI_low", "CI_high"))
})


test_that("estimate_expectation - predicting RE works", {
  skip_if_not_installed("lme4")
  skip_if_not_installed("glmmTMB")

  data(Salamanders, package = "glmmTMB")
  m1 <- glmmTMB::glmmTMB(
    count ~ spp + mined + (1 | site),
    family = poisson(),
    data = Salamanders
  )

  data(efc, package = "modelbased")
  efc$e15relat <- datawizard::to_factor(efc$e15relat)
  m2 <- lme4::lmer(neg_c_7 ~ c12hour + c160age + c161sex + (1 | e15relat), data = efc)

  out <- estimate_relation(m1, by = "site")
  expect_equal(
    out$Predicted,
    c(
      0.23843, 0.23843, 0.18498, 0.35626, 0.24159, 0.29875, 0.14771,
      0.20692, 0.13234, 0.17874, 0.1171, 0.16318, 0.21392, 0.78665,
      0.0828, 0.1716, 0.11488, 0.11488, 0.35552, 0.45806, 0.19238,
      0.09819, 0.25902
    ),
    tolerance = 1e-4
  )

  out <- estimate_relation(m2, by = "e15relat")
  expect_equal(
    out$Predicted,
    c(
      12.20636, 12.06311, 11.2071, 11.62862, 11.2327, 10.58387, 11.20853,
      11.12288
    ),
    tolerance = 1e-4
  )
})
