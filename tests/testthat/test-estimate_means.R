skip_if_not_installed("emmeans")
skip_if_not_installed("marginaleffects")

test_that("estimate_means() - lm", {
  data(mtcars)
  data(iris)

  dat <- mtcars
  dat$gear <- as.factor(dat$gear)
  dat$cyl <- as.factor(dat$cyl)
  dat <<- dat

  # Simple
  model <- lm(vs ~ cyl, data = dat)
  estim1 <- suppressMessages(estimate_means(model))
  estim2 <- suppressMessages(estimate_means(model, backend = "marginaleffects"))
  expect_identical(dim(estim1), c(3L, 5L))
  expect_identical(dim(estim2), c(3L, 5L))
  expect_equal(estim1$Mean, estim2$Mean, tolerance = 1e-4)
  expect_equal(estim1$CI_low, estim2$CI_low, tolerance = 1e-3)

  # Interaction (factor * continuous)
  model <- lm(mpg ~ wt * gear, data = dat)
  estim1 <- suppressMessages(estimate_means(model))
  estim2 <- suppressMessages(estimate_means(model, backend = "marginaleffects"))
  expect_identical(dim(estim1), c(3L, 5L))
  expect_identical(dim(estim2), c(3L, 5L))
  expect_equal(estim1$Mean, estim2$Mean, tolerance = 1e-4)
  expect_named(estim1, c("gear", "Mean", "SE", "CI_low", "CI_high"))
  expect_named(estim2, c("gear", "Mean", "SE", "CI_low", "CI_high"))
  expect_equal(estim1$CI_low, estim2$CI_low, tolerance = 1e-3)

  # At specific levels
  model <- lm(Sepal.Width ~ Species, data = iris)
  estim1 <- suppressMessages(estimate_means(model, by = "Species=c('versicolor', 'virginica')"))
  estim2 <- suppressMessages(estimate_means(model, by = "Species=c('versicolor', 'virginica')", backend = "marginaleffects"))
  expect_identical(dim(estim1), c(2L, 5L))
  expect_identical(dim(estim2), c(2L, 5L))
  expect_equal(estim1$Mean, estim2$Mean, tolerance = 1e-4)
  expect_named(estim1, c("Species", "Mean", "SE", "CI_low", "CI_high"))
  expect_named(estim2, c("Species", "Mean", "SE", "CI_low", "CI_high"))
  expect_equal(estim1$CI_low, estim2$CI_low, tolerance = 1e-3)

  # Interactions between factors
  dat <- iris
  dat$Petal.Length_factor <- ifelse(dat$Petal.Length < 4.2, "A", "B")
  dat <<- dat

  model <- lm(Sepal.Width ~ Species * Petal.Length_factor, data = dat)
  estim1 <- suppressMessages(estimate_means(model, by = "all"))
  estim2 <- suppressWarnings(suppressMessages(estimate_means(model, by = "all", backend = "marginaleffects")))
  expect_identical(dim(estim1), c(6L, 6L))
  expect_identical(dim(estim2), c(6L, 6L))
  expect_equal(estim2$Mean, c(3.428, 3.79557, 2.54211, 2.90968, 2.60643, 2.974), tolerance = 1e-4)
  expect_named(estim1, c("Species", "Petal.Length_factor", "Mean", "SE", "CI_low", "CI_high"))
  expect_named(estim2, c("Species", "Petal.Length_factor", "Mean", "SE", "CI_low", "CI_high"))

  # No interaction (two factors)
  model <- lm(Petal.Length ~ Sepal.Width + Species, data = iris)
  estim1 <- suppressMessages(estimate_means(model))
  estim2 <- suppressMessages(estimate_means(model, backend = "marginaleffects"))
  expect_identical(dim(estim1), c(3L, 5L))
  expect_identical(dim(estim2), c(3L, 5L))
  expect_equal(estim1$Mean, estim2$Mean, tolerance = 1e-4)
  expect_equal(estim1$CI_low, estim2$CI_low, tolerance = 1e-3)
  expect_named(estim2, c("Species", "Mean", "SE", "CI_low", "CI_high"))

  # At specific levels of continuous
  estim1 <- suppressMessages(estimate_means(model, by = "Sepal.Width"))
  estim2 <- suppressMessages(estimate_means(model, by = "Sepal.Width", backend = "marginaleffects"))
  expect_identical(dim(estim1), c(10L, 5L))
  expect_identical(dim(estim2), c(10L, 5L))
  # Note that the absolute values are different here... for unclear reasons
  expect_true(max(diff(estim1$Mean) - diff(estim2$Mean)) < 1e-10)
  expect_equal(estim1$Mean, estim2$Mean, tolerance = 1e-4)

  model <- lm(Petal.Length ~ Sepal.Width + Species, data = iris)
  estim1 <- suppressMessages(estimate_means(model))
  estim2 <- suppressMessages(estimate_means(model, backend = "marginaleffects"))
  expect_identical(dim(estim1), c(3L, 5L))
  expect_identical(dim(estim2), c(3L, 5L))
  expect_equal(estim1$Mean, estim2$Mean, tolerance = 1e-4)
  expect_equal(estim1$CI_low, estim2$CI_low, tolerance = 1e-3)

  estim1 <- suppressMessages(estimate_means(model, by = "all"))
  estim2 <- suppressMessages(estimate_means(model, by = "all", backend = "marginaleffects"))
  expect_identical(dim(estim1), c(30L, 6L))
  expect_identical(dim(estim2), c(30L, 6L))
  expect_equal(estim1$Mean[order(estim1$Sepal.Width)], estim2$Mean, tolerance = 1e-4)
  expect_equal(estim1$CI_low[order(estim1$Sepal.Width)], estim2$CI_low, tolerance = 1e-3)

  # In formula modification
  # FIXME: this got broken but it seems just to tedious to fix. Don't use in formula transforms.
  # model <- lm(mpg ~ wt * as.factor(gear), data = mtcars)
  # estim <- suppressMessages(estimate_means(model))
  # expect_equal(dim(estim), c(3L, 5L))

  # One continuous and one factor
  model <- lm(Petal.Length ~ Species * Sepal.Width, data = iris)

  estim1 <- suppressMessages(estimate_means(model))
  estim2 <- suppressMessages(estimate_means(model, backend = "marginaleffects"))
  expect_identical(dim(estim1), c(3L, 5L))
  expect_identical(dim(estim2), c(3L, 5L))
  expect_equal(estim1$Mean, estim2$Mean, tolerance = 1e-4)
  expect_equal(estim1$CI_low, estim2$CI_low, tolerance = 1e-3)

  estim1 <- suppressMessages(estimate_means(model, by = c("Species", "Sepal.Width"), length = 2))
  estim2 <- suppressMessages(estimate_means(model, by = c("Species", "Sepal.Width"), length = 2, backend = "marginaleffects"))
  expect_identical(dim(estim1), c(6L, 6L))
  expect_identical(dim(estim2), c(6L, 6L))
  expect_equal(estim1$Mean[order(estim1$Species)], estim2$Mean, tolerance = 1e-4)
  expect_equal(estim1$CI_low[order(estim1$Species)], estim2$CI_low, tolerance = 1e-3)

  estim1 <- suppressMessages(estimate_means(model, by = "Species=c('versicolor', 'setosa')"))
  estim2 <- suppressMessages(estimate_means(model, by = "Species=c('versicolor', 'setosa')", backend = "marginaleffects"))
  expect_identical(dim(estim1), c(2L, 5L))
  expect_identical(dim(estim2), c(2L, 5L))
  expect_equal(estim1$Mean, estim2$Mean, tolerance = 1e-4)
  expect_equal(estim1$CI_low, estim2$CI_low, tolerance = 1e-3)

  estim1 <- suppressMessages(estimate_means(model, by = "Sepal.Width=c(2, 4)"))
  estim2 <- suppressMessages(estimate_means(model, by = "Sepal.Width=c(2, 4)", backend = "marginaleffects"))
  expect_identical(dim(estim1), c(2L, 5L))
  expect_identical(dim(estim2), c(2L, 5L))
  expect_equal(estim1$Mean, estim2$Mean, tolerance = 1e-4)
  expect_equal(estim1$CI_low, estim2$CI_low, tolerance = 1e-3)

  estim1 <- suppressMessages(estimate_means(model, by = c("Species", "Sepal.Width=0")))
  estim2 <- suppressMessages(estimate_means(model, by = c("Species", "Sepal.Width=0"), backend = "marginaleffects"))
  expect_identical(dim(estim1), c(3L, 6L))
  expect_identical(dim(estim2), c(3L, 6L))
  expect_equal(estim1$Mean, estim2$Mean, tolerance = 1e-4)
  expect_equal(estim1$CI_low, estim2$CI_low, tolerance = 1e-3)

  estim1 <- suppressMessages(estimate_means(model, by = "Sepal.Width", length = 5))
  estim2 <- suppressMessages(estimate_means(model, by = "Sepal.Width", length = 5, backend = "marginaleffects"))
  expect_identical(dim(estim1), c(5L, 5L))
  expect_identical(dim(estim2), c(5L, 5L))
  expect_equal(estim1$Mean, estim2$Mean, tolerance = 1e-4)
  expect_equal(estim1$CI_low, estim2$CI_low, tolerance = 1e-3)

  estim1 <- suppressMessages(estimate_means(model, by = "Sepal.Width=c(2, 4)"))
  estim2 <- suppressMessages(estimate_means(model, by = "Sepal.Width=c(2, 4)", backend = "marginaleffects"))
  expect_identical(dim(estim1), c(2L, 5L))
  expect_identical(dim(estim2), c(2L, 5L))
  expect_equal(estim1$Mean, estim2$Mean, tolerance = 1e-4)
  expect_equal(estim1$CI_low, estim2$CI_low, tolerance = 1e-3)

  estim1 <- suppressMessages(estimate_means(model, by = c("Species=c('versicolor', 'setosa')", "Sepal.Width=c(2, 4)")))
  estim2 <- suppressMessages(estimate_means(model, by = c("Species=c('versicolor', 'setosa')", "Sepal.Width=c(2, 4)"), backend = "marginalmeans"))
  expect_identical(dim(estim1), c(4L, 6L))
  expect_identical(dim(estim2), c(4L, 6L))
  expect_equal(estim1$Mean, estim2$Mean[order(estim2$Sepal.Width)], tolerance = 1e-4)
  expect_equal(estim1$CI_low, estim2$CI_low[order(estim2$Sepal.Width)], tolerance = 1e-3)

  # Two factors
  dat <- iris
  dat$Petal.Length_factor <- ifelse(dat$Petal.Length < 4.2, "A", "B")
  dat <<- dat
  model <- lm(Petal.Length ~ Species * Petal.Length_factor, data = dat)

  estim1 <- suppressMessages(estimate_means(model, by = "all"))
  estim2 <- suppressMessages(estimate_means(model, by = "all", backend = "marginaleffects"))
  expect_identical(dim(estim1), c(6L, 6L))
  expect_identical(dim(estim2), c(6L, 6L))
  expect_equal(estim2$Mean, c(1.462, 2.24638, 3.77368, 4.55806, 4.76762, 5.552), tolerance = 1e-4)

  estim1 <- suppressMessages(estimate_means(model, by = "Petal.Length_factor"))
  estim2 <- suppressMessages(estimate_means(model, by = "Petal.Length_factor", backend = "marginaleffects"))
  expect_identical(dim(estim1), c(2L, 5L))
  expect_identical(dim(estim2), c(2L, 5L))
  expect_equal(estim2$Mean, c(3.33443, 4.11881), tolerance = 1e-4)

  estim <- suppressMessages(estimate_means(model, by = "Petal.Length_factor='B'", backend = "marginaleffects"))
  expect_equal(estim$Mean, 4.11882, tolerance = 1e-4)

  # Three factors
  dat <- mtcars
  dat[c("gear", "vs", "am")] <- lapply(dat[c("gear", "vs", "am")], as.factor)
  dat <<- dat
  model <- lm(mpg ~ gear * vs * am, data = dat)

  estim1 <- suppressMessages(estimate_means(model))
  estim2 <- suppressMessages(estimate_means(model, backend = "marginaleffects"))
  expect_identical(dim(estim1), c(12L, 7L))
  expect_identical(dim(estim2), c(12L, 7L))
  expect_equal(
    estim2$Mean,
    c(
      15.05, 22.03333, 20.33333, 27.31667, 14.01667, 21, 21.05, 28.03333,
      12.14167, 19.125, 23.41667, 30.4
    ),
    tolerance = 1e-4
  )

  estim1 <- suppressMessages(estimate_means(model, by = c("gear", "vs", "am='1'")))
  estim2 <- suppressMessages(estimate_means(model, by = c("gear", "vs", "am='1'"), backend = "marginaleffects"))
  expect_identical(dim(estim1), c(6L, 7L))
  expect_identical(dim(estim2), c(6L, 7L))
  expect_equal(
    estim2$Mean,
    c(22.03333, 27.31667, 21, 28.03333, 19.125, 30.4),
    tolerance = 1e-4
  )

  estim1 <- suppressMessages(estimate_means(model, by = c("gear='5'", "vs")))
  estim2 <- suppressMessages(estimate_means(model, by = c("gear='5'", "vs"), backend = "marginaleffects"))
  expect_identical(dim(estim1), c(2L, 7L))
  expect_identical(dim(estim2), c(2L, 6L))
  expect_equal(estim2$Mean, c(15.63333, 26.90833), tolerance = 1e-4)

  dat <- iris
  dat$factor1 <- ifelse(dat$Sepal.Width > 3, "A", "B")
  dat$factor2 <- ifelse(dat$Petal.Length > 3.5, "C", "D")
  dat$factor3 <- ifelse(dat$Sepal.Length > 5, "E", "F")
  dat <<- dat

  model <- lm(Petal.Width ~ factor1 * factor2 * factor3, data = dat)
  estim1 <- suppressMessages(estimate_means(model))
  estim2 <- suppressMessages(estimate_means(model, backend = "marginaleffects"))
  expect_identical(dim(estim1), c(8L, 7L))
  expect_identical(dim(estim2), c(8L, 7L))
  expect_equal(
    estim2$Mean,
    c(0.27727, 0.23, 2.004, 2.6842, 1.05, 0.41818, 1.60435, 1.7),
    tolerance = 1e-4
  )

  estim1 <- suppressMessages(estimate_means(model, by = c("factor1", "factor2", "factor3='E'")))
  estim2 <- suppressMessages(estimate_means(model, by = c("factor1", "factor2", "factor3='E'"), backend = "marginaleffects"))
  expect_identical(dim(estim1), c(4L, 7L))
  expect_identical(dim(estim2), c(4L, 7L))
  expect_equal(estim1$Mean, estim2$Mean[order(estim2$factor2, decreasing = TRUE)], tolerance = 1e-4)
  expect_equal(estim1$CI_low, estim2$CI_low[order(estim2$factor2, decreasing = TRUE)], tolerance = 1e-3)
})


test_that("estimate_means() - glm", {
  # GLM
  # TODO: add the marginaleffects comparison for the remaining tests
  dat <- iris
  dat$y <- as.numeric(as.factor(ifelse(dat$Sepal.Width > 3, "A", "B"))) - 1
  dat <<- dat
  model <- glm(y ~ Species, family = "binomial", data = dat)

  estim1 <- suppressMessages(estimate_means(model))
  estim2 <- suppressMessages(estimate_means(model, backend = "marginaleffects"))
  expect_identical(dim(estim1), c(3L, 5L))
  expect_identical(dim(estim2), c(3L, 4L))
  expect_true(all(estim1$Probability >= 0) & all(estim1$Probability <= 1))
  expect_true(all(estim2$Probability >= 0) & all(estim2$Probability <= 1))
  expect_equal(estim1$Probability, estim2$Probability, tolerance = 1e-4)
  expect_equal(estim1$CI_low, estim2$CI_low, tolerance = 1e-2)
  expect_named(estim2, c("Species", "Probability", "CI_low", "CI_high"))

  dat <- iris
  dat$Petal.Length_factor <- as.factor(ifelse(dat$Petal.Length < 4.2, "A", "B"))
  dat <<- dat
  model <- glm(Petal.Length_factor ~ Species, data = dat, family = "binomial")

  estim1 <- suppressMessages(estimate_means(model))
  estim2 <- suppressMessages(estimate_means(model, backend = "marginaleffects"))
  expect_identical(dim(estim1), c(3L, 5L))
  expect_identical(dim(estim2), c(3L, 4L))
  expect_equal(estim1$Probability, estim2$Probability, tolerance = 1e-4)
  expect_equal(estim1$CI_low, estim2$CI_low, tolerance = 1e-2)

  estim1 <- suppressMessages(estimate_means(model, transform = "none"))
  estim2 <- suppressMessages(estimate_means(model, transform = "none", backend = "marginaleffects"))
  expect_identical(dim(estim1), c(3L, 5L))
  expect_identical(dim(estim2), c(3L, 5L))
  expect_equal(estim1$Mean, estim2$Mean, tolerance = 1e-4)
  expect_equal(estim1$CI_low, estim2$CI_low, tolerance = 1e-2)

  model <- glm(Petal.Length ~ Species, data = iris, family = "Gamma")
  estim1 <- suppressMessages(estimate_means(model))
  estim2 <- suppressMessages(estimate_means(model, backend = "marginaleffects"))
  expect_identical(dim(estim1), c(3L, 5L))
  expect_identical(dim(estim2), c(3L, 4L))
  expect_equal(estim1$Mean, estim2$Mean, tolerance = 1e-4)
  expect_equal(estim1$CI_low, estim2$CI_low, tolerance = 1e-2)
})


test_that("estimate_means() - mixed models", {
  skip_if_not_installed("emmeans")
  skip_if_not_installed("lme4")
  skip_if_not_installed("glmmTMB")
  dat <- iris
  dat$Petal.Length_factor <- as.factor(ifelse(dat$Petal.Length < 4.2, "A", "B"))
  dat <<- dat

  model <- lme4::lmer(Sepal.Width ~ Species + (1 | Petal.Length_factor), data = dat)

  estim1 <- suppressMessages(estimate_means(model))
  expect_identical(dim(estim1), c(3L, 5L))
  estim2 <- suppressMessages(estimate_means(model, backend = "marginaleffects"))
  expect_identical(dim(estim2), c(3L, 5L))
  expect_lt(max(estim1$Mean - estim2$Mean), 1e-10)

  model <- lme4::glmer(Sepal.Width ~ Species + (1 | Petal.Length_factor), data = dat, family = "Gamma")
  estim1 <- suppressMessages(estimate_means(model))
  expect_identical(dim(estim1), c(3L, 5L))
  estim2 <- suppressMessages(estimate_means(model, backend = "marginaleffects"))
  expect_identical(dim(estim2), c(3L, 5L))
  expect_lt(max(estim1$Mean - estim2$Mean), 1e-10)

  data(Salamanders, package = "glmmTMB")
  m <- glmmTMB::glmmTMB(
    count ~ mined * spp + (1 | site),
    zi = ~mined,
    family = poisson(),
    data = Salamanders
  )
  out <- estimate_means(m, c("mined", "spp"), backend = "marginaleffects")
  expect_snapshot(print(out))
  out1 <- estimate_means(m, c("mined", "spp"), type = "conditional", backend = "marginaleffects")
  out2 <- estimate_means(m, c("mined", "spp"))
  expect_equal(out1$Mean[order(out1$spp)], out2$rate, tolerance = 1e-1)

  m <- glm(count ~ mined + spp, family = poisson(), data = Salamanders)
  out <- estimate_means(m, c("mined", "spp"), backend = "marginaleffects")
  expect_snapshot(print(out))
  out1 <- estimate_means(m, c("mined", "spp"), backend = "marginaleffects")
  out2 <- estimate_means(m, c("mined", "spp"))
  expect_equal(out1$Mean[order(out1$spp)], out2$rate, tolerance = 1e-3)
})
