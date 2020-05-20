if (require("testthat") && require("modelbased") && require("rstanarm") && require("insight")) {

  test_that("estimate_means", {

    if(require("rstanarm")){
      data <- mtcars
      data$gear <- as.factor(data$gear)

      model <- stan_glm(mpg ~ wt * gear, data = data, refresh = 0, iter = 200, chains = 2)
      estim <- estimate_means(model)
      testthat::expect_equal(c(nrow(estim), ncol(estim)), c(3, 4))


      # model <- stan_glm(vs ~ as.factor(cyl), data = mtcars, refresh = 0, iter = 200, chains = 2)
      data$cyl <- as.factor(data$cyl)
      model <- stan_glm(vs ~ cyl, data = data, refresh = 0, iter = 200, chains = 2)
      estim <- estimate_means(model)
      testthat::expect_equal(c(nrow(estim), ncol(estim)), c(3, 4))



      data <- iris
      data$Petal.Length_factor <- ifelse(data$Petal.Length < 4.2, "A", "B")

      model <- stan_glm(Sepal.Width ~ Species * Petal.Length_factor, data = data, refresh = 0, iter = 200, chains = 2)
      estim <- estimate_means(model)
      testthat::expect_equal(c(nrow(estim), ncol(estim)), c(6, 5))

      model <- stan_glm(Petal.Length ~ Sepal.Width + Species, data = iris)
      estim <- estimate_means(model)
      testthat::expect_equal(c(nrow(estim), ncol(estim)), c(3, 4))

      estim <- estimate_means(model, modulate = "Sepal.Width")
      testthat::expect_equal(c(nrow(estim), ncol(estim)), c(30, 5))

    }


    model <- lm(Petal.Length ~ Sepal.Width + Species, data = iris)
    estim <- estimate_means(model)
    testthat::expect_equal(c(nrow(estim), ncol(estim)), c(3, 5))

    estim <- estimate_means(model, modulate = "Sepal.Width")
    testthat::expect_equal(c(nrow(estim), ncol(estim)), c(30, 6))

    # In formula modification
    model <- lm(mpg ~ wt * as.factor(gear), data = mtcars)
    estim <- estimate_means(model)
    testthat::expect_equal(c(nrow(estim), ncol(estim)), c(3, 5))



    # One continuous and one factor
    model <- lm(Petal.Length ~ Species * Sepal.Width, data = iris)

    estim <- estimate_means(model)
    testthat::expect_equal(c(nrow(estim), ncol(estim)), c(3, 5))
    estim <- estimate_means(model, fixed="Sepal.Width")
    testthat::expect_equal(c(nrow(estim), ncol(estim)), c(3, 6))
    estim <- estimate_means(model, levels = c("Species", "Sepal.Width"), length=2)
    testthat::expect_equal(c(nrow(estim), ncol(estim)), c(6, 6))
    estim <- estimate_means(model, levels = "Species=c('versicolor', 'setosa')")
    testthat::expect_equal(c(nrow(estim), ncol(estim)), c(2, 5))
    estim <- estimate_means(model, levels = "Sepal.Width=c(2, 4)")
    testthat::expect_equal(c(nrow(estim), ncol(estim)), c(2, 5))
    estim <- estimate_means(model, levels = c("Species", "Sepal.Width=0"))
    testthat::expect_equal(c(nrow(estim), ncol(estim)), c(3, 6))
    estim <- estimate_means(model, modulate = "Sepal.Width", length=5)
    testthat::expect_equal(c(nrow(estim), ncol(estim)), c(15, 6))
    estim <- estimate_means(model, modulate = "Sepal.Width=c(2, 4)")
    testthat::expect_equal(c(nrow(estim), ncol(estim)), c(6, 6))

    # Two factors
    data <- iris
    data$Petal.Length_factor <- ifelse(data$Petal.Length < 4.2, "A", "B")
    model <- lm(Petal.Length ~ Species * Petal.Length_factor, data = data)

    estim <- estimate_means(model)
    testthat::expect_equal(c(nrow(estim), ncol(estim)), c(6, 6))
    estim <- estimate_means(model, fixed="Petal.Length_factor")
    testthat::expect_equal(c(nrow(estim), ncol(estim)), c(3, 6))
    estim <- estimate_means(model, fixed="Petal.Length_factor='B'")
    testthat::expect_true(as.character(unique(estim$Petal.Length_factor)) == "B")
  })
}