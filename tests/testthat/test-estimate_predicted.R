osx <- tryCatch({
  si <- Sys.info()
  if (!is.null(si["sysname"])) {
    si["sysname"] == "Darwin" || grepl("^darwin", R.version$os)
  } else {
    FALSE
  }
})

if (require("testthat") && require("modelbased") && require("gamm4") && require("rstanarm") && require("lme4") && require("glmmTMB") && require("mgcv") && require("MASS") && require("brms") && require("testthat")) {
  test_that("estimate_relation - shape", {

    # CI
    model <- lm(Petal.Length ~ Petal.Width, data = iris)
    estim <- modelbased::estimate_relation(model, ci = 0.90)
    expect_equal(attributes(estim)$ci, 0.9)
    estim <- modelbased::estimate_relation(model, ci = c(0.90, .95))
    expect_equal(attributes(estim)$ci, c(0.90, 0.95))
    expect_equal(dim(estim), c(10, 7))

    # vizdata <- visualisation_matrix(model)
    # insight::get_predicted(model, as.data.frame(vizdata), ci = c(0.90, .95))

    # Range
    model <- lm(Petal.Length ~ Petal.Width * Species, data = iris)
    estim <- modelbased::estimate_relation(model, length = 10)
    expect_equal(dim(estim), c(10, 6))
    estim <- modelbased::estimate_relation(model, length = 10, preserve_range = FALSE)
    expect_equal(dim(estim), c(30, 6))
  })


  test_that("estimate_link", {

    # LMER4
    model <- lme4::lmer(Petal.Length ~ Petal.Width + (1 | Species), data = iris)
    expect_equal(nrow(modelbased::estimate_link(model, length = 5)), 5)
    expect_equal(nrow(modelbased::estimate_link(model, include_random = TRUE, preserve_range = FALSE, length = 5)), 15)

    # GLMMTMB
    model <- suppressWarnings(glmmTMB::glmmTMB(Petal.Length ~ Petal.Width + (1 | Species), data = iris))
    expect_equal(nrow(modelbased::estimate_link(model, length = 5)), 5)
    expect_equal(nrow(modelbased::estimate_link(model, include_random = TRUE, preserve_range = FALSE, length = 5)), 15)

    # MGCV
    model <- mgcv::gam(Petal.Length ~ Petal.Width + s(Sepal.Length), data = iris)
    expect_equal(dim(modelbased::estimate_link(model, length = 3)), c(9, 6))
    expect_equal(dim(modelbased::estimate_link(model, include_smooth = FALSE, length = 3)), c(3, 5))

    model <- mgcv::gamm(Petal.Length ~ Petal.Width + s(Sepal.Length), random = list(Species = ~1), data = iris)

    # GAMM4
    model <- gamm4::gamm4(Petal.Length ~ Petal.Width + s(Sepal.Length),
      random = ~ (1 | Species), data = iris
    )
    expect_equal(nrow(modelbased::estimate_link(model, length = 3)), 9)
    expect_equal(dim(modelbased::estimate_link(model, include_smooth = FALSE, length = 3)), c(3, 5))


    if (!osx) {
      # STAN_GAMM4
      model <- suppressWarnings(rstanarm::stan_gamm4(Petal.Length ~ Petal.Width + s(Sepal.Length),
        random = ~ (1 | Species), data = iris,
        iter = 100, chains = 2, refresh = 0
      ))
      expect_equal(nrow(modelbased::estimate_relation(model, length = 3)), 9)
      expect_equal(dim(modelbased::estimate_link(model, include_smooth = FALSE, length = 3)), c(3, 5))
    }
  })





  test_that("estimate_response - Bayesian", {
    model <- suppressWarnings(rstanarm::stan_glm(mpg ~ wt + poly(cyl, 2, raw = TRUE), data = mtcars, refresh = 0, iter = 200, chains = 2))
    estim <- estimate_response(model, seed = 333)
    expect_equal(nrow(estim), nrow(mtcars))

    model <- suppressWarnings(rstanarm::stan_glm(mpg ~ wt * as.factor(gear), data = mtcars, refresh = 0, iter = 200, chains = 2))
    estim <- estimate_response(model, data = "grid", seed = 333, preserve_range = FALSE)
    expect_equal(dim(estim), c(30, 6))

    model <- suppressWarnings(rstanarm::stan_glm(mpg ~ as.factor(gear) / wt, data = mtcars, refresh = 0, iter = 200, chains = 2))
    estim <- estimate_response(model)
    expect_equal(dim(estim), c(32, 7))

    model <- suppressWarnings(rstanarm::stan_glm(Sepal.Width ~ Petal.Width, data = iris, refresh = 0, iter = 200, chains = 2))
    estim <- estimate_link(model, keep_iterations = TRUE)
    draws <- bayestestR::reshape_iterations(estim)
    expect_equal(c(nrow(draws), ncol(draws)), c(2000, 8))

    # Polr
    # model <- suppressWarnings(rstanarm::stan_polr(Species ~ Petal.Width + Petal.Length, data = iris, refresh = 0, iter = 200, chains = 2, prior = rstanarm::R2(0.2, "mean")))
    # estim <- estimate_link(model, length = 6)
    # expect_equal(dim(estim), c(36, 6))

    # Non-sampling algorithms
    model <- rstanarm::stan_glm(mpg ~ disp, data = mtcars, algorithm = "meanfield", refresh = 0)
    estim <- estimate_link(model, keep_iterations = TRUE)
    expect_equal(dim(estim), c(10, 1005))

    # model <- brms::brm(mpg ~ drat, data = mtcars, algorithm = "meanfield", refresh=0)
    # estim <- estimate_link(model, keep_iterations = TRUE)
    # expect_equal(dim(estim), c(25, 1004))
  })


  test_that("estimate_response - Frequentist", {
    model <- lm(mpg ~ wt + cyl, data = mtcars)
    estim <- estimate_expectation(model)
    expect_equal(dim(estim), c(32, 7))

    estim <- estimate_expectation(model, ci = NULL)
    expect_equal(dim(estim), c(32, 5))

    model <- glm(vs ~ wt + cyl, data = mtcars, family = "binomial")
    estim <- estimate_link(model, target = "wt")
    expect_equal(dim(estim), c(10, 6))


    data <- mtcars
    data$gear <- as.factor(data$gear)

    model <- lme4::lmer(wt ~ cyl + (1 | gear), data = data)
    estim <- estimate_link(model)
    expect_equal(dim(estim), c(10, 5))
    estim <- estimate_expectation(model)
    expect_equal(dim(estim), c(32, 7))

    model <- lme4::glmer(vs ~ cyl + (1 | gear), data = data, family = "binomial")
    estim <- estimate_link(model)
    expect_equal(dim(estim), c(10, 5))
    estim <- estimate_expectation(model)
    expect_equal(dim(estim), c(32, 7))

    # model <- MASS::polr(Species ~ Sepal.Width, data = iris)
    # estim <- estimate_link(model)
    # # TODO: why no CI?
    # expect_equal(dim(estim), c(10, 1))
  })
}
