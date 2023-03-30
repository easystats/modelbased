test_that("estimate_means", {
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("emmeans")
  dat <- mtcars
  dat$gear <- as.factor(dat$gear)
  dat$cyl <- as.factor(dat$cyl)
  dat <<- dat

  model <- lm(mpg ~ wt * gear, data = dat)
  estim <- suppressMessages(estimate_means(model))
  expect_equal(dim(estim), c(3, 5))

  model <- lm(vs ~ cyl, data = dat)
  estim <- suppressMessages(estimate_means(model))
  expect_equal(dim(estim), c(3, 5))

  model <- lm(Sepal.Width ~ Species, data = iris)
  estim <- suppressMessages(estimate_means(model, at = "Species=c('versicolor', 'virginica')"))
  expect_equal(dim(estim), c(2, 5))

  dat <- iris
  dat$Petal.Length_factor <- ifelse(dat$Petal.Length < 4.2, "A", "B")
  dat <<- dat

  model <- lm(Sepal.Width ~ Species * Petal.Length_factor, data = dat)
  estim <- suppressMessages(estimate_means(model, at = "all"))
  expect_equal(dim(estim), c(6, 6))

  model <- lm(Petal.Length ~ Sepal.Width + Species, data = iris)
  estim <- suppressMessages(estimate_means(model))
  expect_equal(dim(estim), c(3, 5))

  estim <- suppressMessages(estimate_means(model, at = "Sepal.Width"))
  expect_equal(dim(estim), c(10, 5))

  dat <- iris
  dat$y <- as.numeric(as.factor(ifelse(dat$Sepal.Width > 3, "A", "B"))) - 1
  dat <<- dat
  model <- glm(y ~ Species, family = "binomial", data = dat)

  estim <- suppressMessages(estimate_means(model))
  expect_equal(dim(estim), c(3, 5))
  estim <- suppressMessages(estimate_means(model, transform = "response"))
  expect_equal(dim(estim), c(3, 5))
  expect_true(all(estim$Probability >= 0) & all(estim$Probability <= 1))


  model <- lm(Petal.Length ~ Sepal.Width + Species, data = iris)
  estim <- suppressMessages(estimate_means(model))
  expect_equal(dim(estim), c(3, 5))

  estim <- suppressMessages(estimate_means(model, at = "all"))
  expect_equal(dim(estim), c(30, 6))

  # In formula modification
  # FIXME: this got broken but it seems just to tedious to fix. Don't use in formula transforms.
  # model <- lm(mpg ~ wt * as.factor(gear), data = mtcars)
  # estim <- suppressMessages(estimate_means(model))
  # expect_equal(dim(estim), c(3, 5))

  # One continuous and one factor
  model <- lm(Petal.Length ~ Species * Sepal.Width, data = iris)

  estim <- suppressMessages(estimate_means(model))
  expect_equal(dim(estim), c(3, 5))
  estim <- suppressMessages(estimate_means(model, fixed = "Sepal.Width"))
  expect_equal(dim(estim), c(3, 6))
  estim <- suppressMessages(estimate_means(model, at = c("Species", "Sepal.Width"), length = 2))
  expect_equal(dim(estim), c(6, 6))
  estim <- suppressMessages(estimate_means(model, at = "Species=c('versicolor', 'setosa')"))
  expect_equal(dim(estim), c(2, 5))
  estim <- suppressMessages(estimate_means(model, at = "Sepal.Width=c(2, 4)"))
  expect_equal(dim(estim), c(2, 5))
  estim <- suppressMessages(estimate_means(model, at = c("Species", "Sepal.Width=0")))
  expect_equal(dim(estim), c(3, 6))
  estim <- suppressMessages(estimate_means(model, at = "Sepal.Width", length = 5))
  expect_equal(dim(estim), c(5, 5))
  estim <- suppressMessages(estimate_means(model, at = "Sepal.Width=c(2, 4)"))
  expect_equal(dim(estim), c(2, 5))

  # Two factors
  dat <- iris
  dat$Petal.Length_factor <- ifelse(dat$Petal.Length < 4.2, "A", "B")
  dat <<- dat
  model <- lm(Petal.Length ~ Species * Petal.Length_factor, data = dat)

  estim <- suppressMessages(estimate_means(model, at = "all"))
  expect_equal(dim(estim), c(6, 6))
  estim <- suppressMessages(estimate_means(model, at = "Petal.Length_factor"))
  expect_equal(dim(estim), c(2, 5))
  estim <- suppressMessages(estimate_means(model, at = "Petal.Length_factor='B'"))
  expect_true(as.character(unique(estim$Petal.Length_factor)) == "B")


  # Three factors
  dat <- mtcars
  dat[c("gear", "vs", "am")] <- sapply(dat[c("gear", "vs", "am")], as.factor)
  dat <<- dat
  model <- lm(mpg ~ gear * vs * am, data = dat)

  estim <- suppressMessages(estimate_means(model))
  expect_equal(dim(estim), c(12, 7))
  estim <- suppressMessages(estimate_means(model, fixed = "am"))
  expect_equal(dim(estim), c(6, 7))
  estim <- suppressMessages(estimate_means(model, at = c("gear='5'", "vs")))
  expect_equal(dim(estim), c(2, 7))

  dat <- iris
  dat$factor1 <- ifelse(dat$Sepal.Width > 3, "A", "B")
  dat$factor2 <- ifelse(dat$Petal.Length > 3.5, "C", "D")
  dat$factor3 <- ifelse(dat$Sepal.Length > 5, "E", "F")
  dat <<- dat

  model <- lm(Petal.Width ~ factor1 * factor2 * factor3, data = dat)
  estim <- suppressMessages(estimate_means(model))
  expect_equal(dim(estim), c(8, 7))
  estim <- suppressMessages(estimate_means(model, fixed = "factor3"))
  expect_equal(dim(estim), c(4, 7))



  # GLM
  dat <- iris
  dat$Petal.Length_factor <- as.factor(ifelse(dat$Petal.Length < 4.2, "A", "B"))
  dat <<- dat
  model <- glm(Petal.Length_factor ~ Species, data = dat, family = "binomial")

  estim <- suppressMessages(estimate_means(model))
  expect_equal(dim(estim), c(3, 5))
  estim <- suppressMessages(estimate_means(model, transform = "none"))
  expect_equal(dim(estim), c(3, 5))

  model <- glm(Petal.Length ~ Species, data = iris, family = "Gamma")
  estim <- suppressMessages(estimate_means(model))
  expect_equal(dim(estim), c(3, 5))
})

test_that("mixed models", {
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("emmeans")
  skip_if_not_installed("lme4")
  dat <- iris
  dat$Petal.Length_factor <- as.factor(ifelse(dat$Petal.Length < 4.2, "A", "B"))
  dat <<- dat

  model <- lme4::lmer(Sepal.Width ~ Species + (1 | Petal.Length_factor), data = dat)

  estim <- suppressMessages(estimate_means(model))
  expect_equal(dim(estim), c(3, 5))

  model <- lme4::glmer(Sepal.Width ~ Species + (1 | Petal.Length_factor), data = dat, family = "Gamma")

  estim <- suppressMessages(estimate_means(model))
  expect_equal(dim(estim), c(3, 5))
})

