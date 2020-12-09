if (require("testthat") && require("modelbased") && require("rstanarm") && require("MASS") && require("insight") && require("brms") && require("lme4")) {
  if (Sys.info()["sysname"] != "Darwin") {
    test_that("estimate_response - Bayesian", {
      model <- suppressWarnings(rstanarm::stan_glm(mpg ~ wt + poly(cyl, 2, raw = TRUE), data = mtcars, refresh = 0, iter = 200, chains = 2))
      estim <- estimate_response(model, seed = 333)
      testthat::expect_equal(nrow(estim), nrow(mtcars))

      model <- suppressWarnings(rstanarm::stan_glm(mpg ~ wt * as.factor(gear), data = mtcars, refresh = 0, iter = 200, chains = 2))
      estim <- estimate_response(model, data = "grid", seed = 333)
      testthat::expect_equal(c(nrow(estim), ncol(estim)), c(43, 5))

      model <- suppressWarnings(rstanarm::stan_glm(mpg ~ as.factor(gear) / wt, data = mtcars, refresh = 0, iter = 200, chains = 2))
      estim <- estimate_response(model)
      testthat::expect_equal(c(nrow(estim), ncol(estim)), c(32, 5))

      model <- suppressWarnings(rstanarm::stan_glm(Sepal.Width ~ Petal.Width, data = iris, refresh = 0, iter = 200, chains = 2))
      estim <- estimate_link(model, keep_draws = TRUE)
      draws <- reshape_draws(estim)
      testthat::expect_equal(c(nrow(draws), ncol(draws)), c(5000, 7))

      # Polr
      model <- suppressWarnings(rstanarm::stan_polr(Species ~ Petal.Width + Petal.Length, data = iris, refresh = 0, iter = 200, chains = 2, prior = rstanarm::R2(0.2, "mean")))
      estim <- estimate_link(model, length = 5)
      testthat::expect_equal(c(nrow(estim), ncol(estim)), c(25, 5))

      # Non-sampling algorithms
      # model <- rstanarm::stan_glm(mpg ~ disp, data = mtcars, algorithm = "meanfield", refresh=0)
      # estim <- estimate_link(model, keep_draws = TRUE)
      # testthat::expect_equal(c(nrow(estim), ncol(estim)), c(25, 1004))

      # model <- brms::brm(mpg ~ drat, data = mtcars, algorithm = "meanfield", refresh=0)
      # estim <- estimate_link(model, keep_draws = TRUE)
      # testthat::expect_equal(c(nrow(estim), ncol(estim)), c(25, 1004))
    })
  }


  test_that("estimate_response - Frequentist", {
    model <- lm(mpg ~ wt + cyl, data = mtcars)
    estim <- estimate_response(model)
    testthat::expect_equal(c(nrow(estim), ncol(estim)), c(32, 5))

    estim <- modelbased::estimate_response(model, ci = NULL)
    testthat::expect_equal(c(nrow(estim), ncol(estim)), c(32, 3))

    model <- glm(vs ~ wt + cyl, data = mtcars, family = "binomial")
    estim <- estimate_link(model, target = "wt")
    testthat::expect_equal(c(nrow(estim), ncol(estim)), c(25, 5))


    data <- mtcars
    data$gear <- as.factor(data$gear)

    model <- lme4::lmer(wt ~ cyl + (1 | gear), data = data)
    estim <- estimate_link(model)
    testthat::expect_equal(c(nrow(estim), ncol(estim)), c(25, 4))
    estim <- estimate_response(model)
    testthat::expect_equal(c(nrow(estim), ncol(estim)), c(32, 5))

    model <- lme4::glmer(vs ~ cyl + (1 | gear), data = data, family = "binomial")
    estim <- estimate_link(model)
    testthat::expect_equal(c(nrow(estim), ncol(estim)), c(25, 4))
    estim <- estimate_response(model)
    testthat::expect_equal(c(nrow(estim), ncol(estim)), c(32, 5))

    model <- MASS::polr(Species ~ Sepal.Width, data = iris)
    estim <- estimate_link(model)
    testthat::expect_equal(c(nrow(estim), ncol(estim)), c(25, 4))
  })
}
