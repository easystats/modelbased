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
  })
}