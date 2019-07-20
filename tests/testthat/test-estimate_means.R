context("estimate_means")



test_that("estimate_means", {
  library(insight)
  library(rstanarm)

  data <- iris
  data$Petal.Length_factor <- ifelse(data$Petal.Length < 4.2, "A", "B")

  estim <- estimate_means(insight::download_model("stanreg_lm_6"))
  testthat::expect_equal(c(nrow(estim), ncol(estim)), c(3, 4))

  model <- stan_glm(Sepal.Width ~ Species * Petal.Length_factor, data = data, refresh = 0, iter = 200, chains = 2)
  estim <- estimate_means(model)
  testthat::expect_equal(c(nrow(estim), ncol(estim)), c(6, 5))

  model <- stan_glm(vs ~ as.factor(cyl), data = mtcars, refresh = 0, iter = 200, chains = 2)
  estim <- estimate_means(model)
  testthat::expect_equal(c(nrow(estim), ncol(estim)), c(3, 4))

  model <- stan_glm(Petal.Length ~ Sepal.Width + Species, data = iris)
  estim <- estimate_means(model)
  testthat::expect_equal(c(nrow(estim), ncol(estim)), c(3, 4))

  estim <- estimate_means(model, modulate = "Sepal.Width")
  testthat::expect_equal(c(nrow(estim), ncol(estim)), c(30, 5))
})
