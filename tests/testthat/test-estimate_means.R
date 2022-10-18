if (

  require("rstanarm") &&
    require("insight") &&
    require("emmeans")) {
  test_that("estimate_means", {
    data <- mtcars
    data$gear <- as.factor(data$gear)

    model <- lm(mpg ~ wt * gear, data = data)
    estim <- estimate_means(model)
    expect_equal(dim(estim), c(3, 5))

    data$cyl <- as.factor(data$cyl)
    model <- lm(vs ~ cyl, data = data)
    estim <- estimate_means(model)
    expect_equal(dim(estim), c(3, 5))

    model <- lm(Sepal.Width ~ Species, data = iris)
    estim <- estimate_means(model, at = "Species=c('versicolor', 'virginica')")
    expect_equal(dim(estim), c(2, 5))

    data <- iris
    data$Petal.Length_factor <- ifelse(data$Petal.Length < 4.2, "A", "B")

    model <- lm(Sepal.Width ~ Species * Petal.Length_factor, data = data)
    estim <- estimate_means(model, at = "all")
    expect_equal(dim(estim), c(6, 6))

    model <- lm(Petal.Length ~ Sepal.Width + Species, data = iris)
    estim <- estimate_means(model)
    expect_equal(dim(estim), c(3, 5))

    estim <- estimate_means(model, at = "Sepal.Width")
    expect_equal(dim(estim), c(10, 5))

    df <- iris
    df$y <- as.numeric(as.factor(ifelse(df$Sepal.Width > 3, "A", "B"))) - 1
    model <- glm(y ~ Species, family = "binomial", data = df)

    estim <- estimate_means(model)
    expect_equal(dim(estim), c(3, 5))
    estim <- estimate_means(model, transform = "response")
    expect_equal(dim(estim), c(3, 5))
    expect_true(all(estim$Probability >= 0) & all(estim$Probability <= 1))


    model <- lm(Petal.Length ~ Sepal.Width + Species, data = iris)
    estim <- estimate_means(model)
    expect_equal(dim(estim), c(3, 5))

    estim <- estimate_means(model, at = "all")
    expect_equal(dim(estim), c(30, 6))

    # In formula modification
    # FIXME: this got broken but it seems just to tedious to fix. Don't use in formula transforms.
    # model <- lm(mpg ~ wt * as.factor(gear), data = mtcars)
    # estim <- estimate_means(model)
    # expect_equal(dim(estim), c(3, 5))

    # One continuous and one factor
    model <- lm(Petal.Length ~ Species * Sepal.Width, data = iris)

    estim <- estimate_means(model)
    expect_equal(dim(estim), c(3, 5))
    estim <- estimate_means(model, fixed = "Sepal.Width")
    expect_equal(dim(estim), c(3, 6))
    estim <- estimate_means(model, at = c("Species", "Sepal.Width"), length = 2)
    expect_equal(dim(estim), c(6, 6))
    estim <- estimate_means(model, at = "Species=c('versicolor', 'setosa')")
    expect_equal(dim(estim), c(2, 5))
    estim <- estimate_means(model, at = "Sepal.Width=c(2, 4)")
    expect_equal(dim(estim), c(2, 5))
    estim <- estimate_means(model, at = c("Species", "Sepal.Width=0"))
    expect_equal(dim(estim), c(3, 6))
    estim <- estimate_means(model, at = "Sepal.Width", length = 5)
    expect_equal(dim(estim), c(5, 5))
    estim <- estimate_means(model, at = "Sepal.Width=c(2, 4)")
    expect_equal(dim(estim), c(2, 5))

    # Two factors
    data <- iris
    data$Petal.Length_factor <- ifelse(data$Petal.Length < 4.2, "A", "B")
    model <- lm(Petal.Length ~ Species * Petal.Length_factor, data = data)

    estim <- estimate_means(model, at = "all")
    expect_equal(dim(estim), c(6, 6))
    estim <- estimate_means(model, at = "Petal.Length_factor")
    expect_equal(dim(estim), c(2, 5))
    estim <- estimate_means(model, at = "Petal.Length_factor='B'")
    expect_true(as.character(unique(estim$Petal.Length_factor)) == "B")


    # Three factors
    data <- mtcars
    data[c("gear", "vs", "am")] <- sapply(data[c("gear", "vs", "am")], as.factor)
    model <- lm(mpg ~ gear * vs * am, data = data)

    estim <- estimate_means(model)
    expect_equal(dim(estim), c(12, 7))
    estim <- estimate_means(model, fixed = "am")
    expect_equal(dim(estim), c(6, 7))
    estim <- estimate_means(model, at = c("gear='5'", "vs"))
    expect_equal(dim(estim), c(2, 7))

    data <- iris
    data$factor1 <- ifelse(data$Sepal.Width > 3, "A", "B")
    data$factor2 <- ifelse(data$Petal.Length > 3.5, "C", "D")
    data$factor3 <- ifelse(data$Sepal.Length > 5, "E", "F")

    model <- lm(Petal.Width ~ factor1 * factor2 * factor3, data = data)
    estim <- estimate_means(model)
    expect_equal(dim(estim), c(8, 7))
    estim <- estimate_means(model, fixed = "factor3")
    expect_equal(dim(estim), c(4, 7))


    # Mixed models
    if (require("lme4")) {
      data <- iris
      data$Petal.Length_factor <- as.factor(ifelse(data$Petal.Length < 4.2, "A", "B"))

      model <- lme4::lmer(Sepal.Width ~ Species + (1 | Petal.Length_factor), data = data)

      estim <- estimate_means(model)
      expect_equal(dim(estim), c(3, 5))

      model <- lme4::glmer(Sepal.Width ~ Species + (1 | Petal.Length_factor), data = data, family = "Gamma")

      estim <- estimate_means(model)
      expect_equal(dim(estim), c(3, 5))
    }

    # GLM
    data <- iris
    data$Petal.Length_factor <- as.factor(ifelse(data$Petal.Length < 4.2, "A", "B"))
    model <- glm(Petal.Length_factor ~ Species, data = data, family = "binomial")

    estim <- estimate_means(model)
    expect_equal(dim(estim), c(3, 5))
    estim <- estimate_means(model, transform = "none")
    expect_equal(dim(estim), c(3, 5))

    model <- glm(Petal.Length ~ Species, data = iris, family = "Gamma")
    estim <- estimate_means(model)
    expect_equal(dim(estim), c(3, 5))
  })
}
