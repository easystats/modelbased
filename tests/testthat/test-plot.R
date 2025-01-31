skip_on_os(c("mac", "solaris", "linux"))
skip_if_not_installed("ggplot2")
skip_if_not_installed("see")
skip_if_not_installed("vdiffr")
skip_if_not_installed("emmeans")
skip_if_not_installed("marginaleffects")
skip_on_cran()

test_that("plots emmeans", {
  model <- lm(Sepal.Length ~ Species * Sepal.Width, data = iris)

  # Estimate means -------------------------------------
  x <- estimate_means(model, by = "Species", backend = "emmeans")
  vdiffr::expect_doppelganger(
    "plot-means-1",
    plot(modelbased::visualisation_recipe(x, show_data = FALSE))
  )
  x <- estimate_means(model, by = "Sepal.Width", backend = "emmeans")
  vdiffr::expect_doppelganger(
    "plot-means-2",
    plot(modelbased::visualisation_recipe(x, show_data = FALSE))
  )
  x <- estimate_means(model, by = c("Sepal.Width", "Species"), backend = "emmeans")
  vdiffr::expect_doppelganger(
    "plot-means-3",
    plot(modelbased::visualisation_recipe(x, show_data = FALSE))
  )
  x <- estimate_means(model, by = c("Species", "Sepal.Width"), backend = "emmeans")
  vdiffr::expect_doppelganger(
    "plot-means-4",
    plot(modelbased::visualisation_recipe(x, show_data = FALSE))
  )
})


test_that("plots, show_data", {
  model <- lm(Sepal.Length ~ Species * Sepal.Width, data = iris)

  # Estimate means -------------------------------------
  x <- estimate_means(model, by = "Species")
  set.seed(123)
  vdiffr::expect_doppelganger(
    "plot-means-showdata-1",
    plot(modelbased::visualisation_recipe(x, show_data = TRUE))
  )
  x <- estimate_means(model, by = "Sepal.Width")
  set.seed(123)
  vdiffr::expect_doppelganger(
    "plot-means-showdata-2",
    plot(modelbased::visualisation_recipe(x, show_data = TRUE))
  )
  x <- estimate_means(model, by = c("Sepal.Width", "Species"))
  set.seed(123)
  vdiffr::expect_doppelganger(
    "plot-means-showdata-3",
    plot(modelbased::visualisation_recipe(x, show_data = TRUE))
  )
  x <- estimate_means(model, by = c("Species", "Sepal.Width"))
  set.seed(123)
  vdiffr::expect_doppelganger(
    "plot-means-showdata-4",
    plot(modelbased::visualisation_recipe(x, show_data = TRUE))
  )
})


test_that("plots marginalmeans", {
  model <- lm(Sepal.Length ~ Species * Sepal.Width, data = iris)

  # Estimate means -------------------------------------
  x <- estimate_means(model, by = "Species", backend = "marginaleffects")
  vdiffr::expect_doppelganger(
    "plot-me-means-1",
    plot(modelbased::visualisation_recipe(x, show_data = FALSE))
  )
  x <- estimate_means(model, by = "Sepal.Width", backend = "marginaleffects")
  vdiffr::expect_doppelganger(
    "plot-me-means-2",
    plot(modelbased::visualisation_recipe(x, show_data = FALSE))
  )
  x <- estimate_means(model, by = c("Sepal.Width", "Species"), backend = "marginaleffects")
  vdiffr::expect_doppelganger(
    "plot-me-means-3",
    plot(modelbased::visualisation_recipe(x, show_data = FALSE))
  )
  x <- estimate_means(model, by = c("Species", "Sepal.Width"), backend = "marginaleffects")
  vdiffr::expect_doppelganger(
    "plot-me-means-4",
    plot(modelbased::visualisation_recipe(x, show_data = FALSE))
  )
})


test_that("plots marginalmeans, show_data", {
  model <- lm(Sepal.Length ~ Species * Sepal.Width, data = iris)

  # Estimate means -------------------------------------
  x <- estimate_means(model, by = "Species", backend = "marginaleffects")
  set.seed(123)
  vdiffr::expect_doppelganger(
    "plot-me-means-showdata-1",
    plot(modelbased::visualisation_recipe(x, show_data = TRUE))
  )
  x <- estimate_means(model, by = "Sepal.Width", backend = "marginaleffects")
  set.seed(123)
  vdiffr::expect_doppelganger(
    "plot-me-means-showdata-2",
    plot(modelbased::visualisation_recipe(x, show_data = TRUE))
  )
  x <- estimate_means(model, by = c("Sepal.Width", "Species"), backend = "marginaleffects")
  set.seed(123)
  vdiffr::expect_doppelganger(
    "plot-me-means-showdata-3",
    plot(modelbased::visualisation_recipe(x, show_data = TRUE))
  )
  x <- estimate_means(model, by = c("Species", "Sepal.Width"), backend = "marginaleffects")
  set.seed(123)
  vdiffr::expect_doppelganger(
    "plot-me-means-showdata-4",
    plot(modelbased::visualisation_recipe(x, show_data = TRUE))
  )
})


test_that("plots relation", {
  model <- lm(Sepal.Length ~ Species * Sepal.Width, data = iris)

  # Estimate means -------------------------------------
  x <- estimate_relation(model, by = "Species")
  vdiffr::expect_doppelganger(
    "plot-relation-1",
    plot(modelbased::visualisation_recipe(x, show_data = FALSE))
  )
  x <- estimate_relation(model, by = "Sepal.Width")
  vdiffr::expect_doppelganger(
    "plot-relation-2",
    plot(modelbased::visualisation_recipe(x, show_data = FALSE))
  )
  x <- estimate_relation(model, by = c("Sepal.Width", "Species"))
  vdiffr::expect_doppelganger(
    "plot-relation-3",
    plot(modelbased::visualisation_recipe(x, show_data = FALSE))
  )
  x <- estimate_relation(model, by = c("Species", "Sepal.Width"))
  vdiffr::expect_doppelganger(
    "plot-relation-4",
    plot(modelbased::visualisation_recipe(x, show_data = FALSE))
  )
})


test_that("plots, relation show_data", {
  model <- lm(Sepal.Length ~ Species * Sepal.Width, data = iris)

  # Estimate means -------------------------------------
  x <- estimate_relation(model, by = "Species")
  set.seed(123)
  vdiffr::expect_doppelganger(
    "plot-relation-showdata-1",
    plot(modelbased::visualisation_recipe(x, show_data = TRUE))
  )
  x <- estimate_relation(model, by = "Sepal.Width")
  set.seed(123)
  vdiffr::expect_doppelganger(
    "plot-relation-showdata-2",
    plot(modelbased::visualisation_recipe(x, show_data = TRUE))
  )
  x <- estimate_relation(model, by = c("Sepal.Width", "Species"))
  set.seed(123)
  vdiffr::expect_doppelganger(
    "plot-relation-showdata-3",
    plot(modelbased::visualisation_recipe(x, show_data = TRUE))
  )
  x <- estimate_relation(model, by = c("Species", "Sepal.Width"))
  set.seed(123)
  vdiffr::expect_doppelganger(
    "plot-relation-showdata-4",
    plot(modelbased::visualisation_recipe(x, show_data = TRUE))
  )
})


test_that("plots, grouplevel lme4", {
  skip_if_not_installed("lme4")
  d <- rbind(lme4::sleepstudy, lme4::sleepstudy)
  d$Newfactor <- rep(c("A", "B", "C", "D"))
  model <- lme4::lmer(
    Reaction ~ Days + (1 + Days | Subject) + (1 | Newfactor),
    data = d
  )
  set.seed(123)
  vdiffr::expect_doppelganger(
    "plot-grouplevel-lme4-1",
    plot(estimate_grouplevel(model))
  )
})


test_that("plots, relation, multiple CI", {
  data(mtcars)
  m <- lm(mpg ~ qsec, data = mtcars)
  em <- estimate_relation(m, ci = c(0.5, 0.8, 0.9))
  set.seed(123)
  vdiffr::expect_doppelganger(
    "plot-relation-multiple-ci-1",
    plot(em, show_data = TRUE)
  )
})


test_that("plots, estimate_means works with Poisson", {
  set.seed(123)
  dat <- data.frame(y = rpois(100, 3), fa = gl(4, 20, 100))
  dat_glm <- glm(y ~ fa, data = dat, family = poisson(link = "log"))
  x <- estimate_means(dat_glm, "fa", backend = "emmeans")
  set.seed(123)
  vdiffr::expect_doppelganger(
    "plot-means-poisson-1",
    plot(x, show_data = TRUE)
  )
  x <- estimate_means(dat_glm, "fa", backend = "marginaleffects")
  set.seed(123)
  vdiffr::expect_doppelganger(
    "plot-means-poisson-2",
    plot(x, show_data = TRUE)
  )
})


test_that("plots, estimate_means, order of predictors", {
  data(iris)
  model <- lm(Petal.Length ~ Sepal.Width * Species, data = iris)
  pr <- estimate_means(model, by = c("Species", "Sepal.Width"), backend = "emmeans")
  set.seed(123)
  vdiffr::expect_doppelganger(
    "plot-means-predictor-order-1",
    plot(pr, show_data = FALSE)
  )
  pr <- estimate_means(model, by = c("Sepal.Width", "Species"), backend = "emmeans")
  set.seed(123)
  vdiffr::expect_doppelganger(
    "plot-means-predictor-order-2",
    plot(pr, show_data = FALSE)
  )
  pr <- estimate_means(model, by = c("Species", "Sepal.Width"), backend = "marginaleffects")
  set.seed(123)
  vdiffr::expect_doppelganger(
    "plot-means-predictor-order-3",
    plot(pr, show_data = FALSE)
  )
  pr <- estimate_means(model, by = c("Sepal.Width", "Species"), backend = "marginaleffects")
  set.seed(123)
  vdiffr::expect_doppelganger(
    "plot-means-predictor-order-4",
    plot(pr, show_data = FALSE)
  )
})


test_that("plots, numeric or categorical predictors are detected", {
  data(mtcars)
  dat <- mtcars
  dat$cyl <- factor(dat$cyl)
  m_cat <- lm(mpg ~ cyl, data = dat)
  pr <- estimate_expectation(m_cat, by = "cyl")
  set.seed(123)
  vdiffr::expect_doppelganger(
    "plot-cat-num-predictor-1",
    plot(pr, show_data = FALSE)
  )
  pr <- estimate_means(m_cat, by = "cyl", backend = "marginaleffects")
  set.seed(123)
  vdiffr::expect_doppelganger(
    "plot-cat-num-predictor-2",
    plot(pr, show_data = FALSE)
  )
  m_cat <- lm(mpg ~ cyl, data = mtcars)
  pr <- estimate_expectation(m_cat, by = "cyl")
  set.seed(123)
  vdiffr::expect_doppelganger(
    "plot-cat-num-predictor-3",
    plot(pr, show_data = FALSE)
  )
  pr <- estimate_means(m_cat, by = "cyl", backend = "marginaleffects")
  set.seed(123)
  vdiffr::expect_doppelganger(
    "plot-cat-num-predictor-4",
    plot(pr, show_data = FALSE)
  )
})


test_that("plots, at special values", {
  data(iris)
  model <- lm(Sepal.Width ~ Petal.Length + Species * Petal.Width, data = iris)
  pr <- estimate_expectation(
    model,
    by = c("Species", "Petal.Width = [fivenum]"),
    preserve_range = FALSE
  )
  set.seed(123)
  vdiffr::expect_doppelganger(
    "plot-expectation-fivenum",
    plot(pr, show_data = FALSE)
  )
})


test_that("plots, estimate_slope", {
  data(iris)

  model <- lm(Sepal.Width ~ Species * Petal.Length, data = iris)
  slopes <- estimate_slopes(model, trend = "Petal.Length", by = "Species", backend = "emmeans")
  set.seed(123)
  vdiffr::expect_doppelganger(
    "plot-slopes-1",
    plot(slopes)
  )

  model <- lm(Sepal.Width ~ Petal.Width * Petal.Length, data = iris)
  slopes <- estimate_slopes(model, trend = "Petal.Length", by = "Petal.Width", backend = "emmeans")
  set.seed(123)
  vdiffr::expect_doppelganger(
    "plot-slopes-2",
    plot(slopes)
  )

  model <- lm(Sepal.Width ~ Species * Petal.Length, data = iris)
  slopes <- estimate_slopes(model, trend = "Petal.Length", by = "Species", backend = "marginaleffects")
  set.seed(123)
  vdiffr::expect_doppelganger(
    "plot-slopes-3",
    plot(slopes)
  )

  model <- lm(Sepal.Width ~ Petal.Width * Petal.Length, data = iris)
  slopes <- estimate_slopes(model, trend = "Petal.Length", by = "Petal.Width", backend = "marginaleffects")
  set.seed(123)
  vdiffr::expect_doppelganger(
    "plot-slopes-4",
    plot(slopes)
  )

  data(iris)
  x <- iris
  x$fac <- rep_len(c("A", "B"), 150)
  x$fac2 <- rep_len(c("X", "X", "X", "Y", "Y", "Y"), 150)
  model <- lm(Sepal.Length ~ Species * fac * Sepal.Width * fac2, data = x)
  slopes <- estimate_slopes(model, trend = "Species", by = "Sepal.Width", backend = "marginaleffects")
  set.seed(123)
  vdiffr::expect_doppelganger(
    "plot-slopes-5",
    plot(slopes)
  )
})


test_that("plots, automatically join dots", {
  data(iris)
  m <- lm(Sepal.Width ~ Petal.Length + Species * Petal.Width, data = iris)
  out <- estimate_expectation(m, by = c("Species", "Petal.Width"))
  set.seed(123)
  vdiffr::expect_doppelganger(
    "plot-join-dots-1",
    plot(out, show_data = TRUE)
  )
  out <- estimate_expectation(m, by = c("Species", "Petal.Width"), preserve_range = FALSE)
  set.seed(123)
  vdiffr::expect_doppelganger(
    "plot-join-dots-2",
    plot(out, show_data = TRUE)
  )
})


test_that("plots, logistic regression", {
  data(mtcars)
  model <- glm(vs ~ wt, data = mtcars, family = "binomial")
  out <- estimate_means(model, "wt", length = 100, backend = "marginaleffects")
  set.seed(123)
  vdiffr::expect_doppelganger(
    "plot-glm-logistic-1",
    plot(out, show_data = TRUE)
  )
  out <- estimate_means(model, "wt", length = 100, predict = "link", backend = "marginaleffects")
  set.seed(123)
  vdiffr::expect_doppelganger(
    "plot-glm-logistic-2",
    plot(out, show_data = TRUE)
  )
})


test_that("plots, 4-way with numeric", {
  data(efc, package = "modelbased")
  # make categorical
  efc <- datawizard::to_factor(efc, c("c161sex", "c172code", "e16sex"))
  levels(efc$c172code) <- c("low", "mid", "high")
  m <- lm(neg_c_7 ~ e16sex + c161sex + c172code * barthtot + c12hour, data = efc)
  by <- c("c12hour", "c161sex", "c172code", "barthtot = [fivenum]")
  estim <- estimate_means(m, by = by, backend = "marginaleffects")
  set.seed(123)
  vdiffr::expect_doppelganger(
    "plot-4way-numeric-1",
    plot(estim, show_data = FALSE)
  )
})


test_that("plots, glm logistic inside bound", {
  set.seed(5)
  data <- data.frame(
    outcome = rbinom(100, 1, 0.5),
    var1 = as.factor(rbinom(100, 1, 0.1)),
    var2 = rnorm(100, 10, 7)
  )
  m <- glm(
    outcome ~ var1 * var2,
    data = data,
    family = binomial(link = "logit")
  )
  out1 <- estimate_means(m, c("var2 = [sd]", "var1"), backend = "marginaleffects")
  set.seed(123)
  vdiffr::expect_doppelganger(
    "plot-logistic-bounds-1",
    plot(out1, show_data = FALSE)
  )
})


test_that("plots no CI", {
  model <- lm(Sepal.Length ~ Species * Sepal.Width, data = iris)

  # Estimate means -------------------------------------
  x <- estimate_means(model, by = "Species", backend = "marginaleffects")
  vdiffr::expect_doppelganger(
    "plot-means-no-ci-1",
    plot(modelbased::visualisation_recipe(x, pointrange = list(geom = "point")))
  )
  vdiffr::expect_doppelganger(
    "plot-means-no-ci-2",
    plot(modelbased::visualisation_recipe(x, pointrange = list(geom = "point"), join_dots = FALSE))
  )
  x <- estimate_means(model, by = "Sepal.Width", backend = "marginaleffects")
  vdiffr::expect_doppelganger(
    "plot-means-no_ci-3",
    plot(modelbased::visualisation_recipe(x, show_data = FALSE, ribbon = "none"))
  )
})


test_that("interaction of numerics (Johnson-Neyman)", {
  data(mtcars)
  model <- lm(mpg ~ hp * wt, data = mtcars)
  slopes <- estimate_slopes(model, trend = "hp", by = "wt", length = 200)
  set.seed(123)
  vdiffr::expect_doppelganger(
    "plot-jonhson-neyman-1",
    plot(slopes)
  )
})
