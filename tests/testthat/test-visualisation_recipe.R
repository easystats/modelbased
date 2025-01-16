test_that("visualization_recipe", {
  skip_if_not_installed("emmeans")
  skip_if_not_installed("marginaleffects")

  data <- iris
  data$fac <- rep_len(c("A", "B"), 150)
  data$fac2 <- rep_len(c("X", "X", "X", "Y", "Y", "Y"), 150)
  model <- lm(Sepal.Length ~ Species * fac * Sepal.Width * fac2, data = data)

  # Estimate means -------------------------------------
  x <- estimate_means(model, by = "Species")
  # plot(modelbased:::.visualization_recipe(x))
  aes <- modelbased:::.find_aes(x)$aes
  expect_identical(aes$y, "Mean")
  expect_identical(aes$x, "Species")

  x <- estimate_means(model, by = "Sepal.Width")
  # plot(modelbased:::.visualization_recipe(x))
  aes <- modelbased:::.find_aes(x)$aes
  expect_identical(aes$y, "Mean")
  expect_identical(aes$x, "Sepal.Width")

  x <- estimate_means(model, by = c("Sepal.Width", "Species"))
  # plot(modelbased:::.visualization_recipe(x))
  aes <- modelbased:::.find_aes(x)$aes
  expect_identical(aes$y, "Mean")
  expect_identical(aes$x, "Sepal.Width")

  x <- estimate_means(model, by = c("Species", "Sepal.Width"))
  # plot(modelbased:::.visualization_recipe(x))
  aes <- modelbased:::.find_aes(x)$aes
  expect_identical(aes$y, "Mean")
  expect_identical(aes$x, "Species")

  x <- estimate_means(model, by = c("Species", "fac", "Sepal.Width"))
  # plot(modelbased:::.visualization_recipe(x))
  aes <- modelbased:::.find_aes(x)$aes
  expect_identical(aes$y, "Mean")
  expect_identical(aes$x, "Species")
  expect_identical(aes$color, "fac")


  x <- estimate_means(model, by = c("Species", "Sepal.Width", "fac"))
  # plot(modelbased:::.visualization_recipe(x))
  aes <- modelbased:::.find_aes(x)$aes
  expect_identical(aes$y, "Mean")
  expect_identical(aes$x, "Species")
  expect_identical(aes$color, "Sepal.Width")

  # works with glm-Poisson
  set.seed(123)
  dat <- data.frame(y = rpois(100, 3), fa = gl(4, 20, 100))
  dat_glm <- glm(y ~ fa, data = dat, family = poisson(link = "log"))
  x <- estimate_means(dat_glm, "fa", backend = "emmeans")
  vr <- visualisation_recipe(x)
  expect_identical(vr$l1$aes, list(y = "y", x = "fa", color = NULL, alpha = NULL))
  expect_identical(vr$l2$aes, list(y = "Rate", x = "fa", color = NULL, group = 1, alpha = NULL))
  expect_identical(
    vr$l3$aes,
    list(
      y = "Rate", x = "fa", ymin = "CI_low", ymax = "CI_high",
      color = NULL, group = 1, alpha = NULL
    )
  )
  x <- estimate_means(dat_glm, "fa", backend = "marginaleffects")
  expect_identical(vr$l1$aes, list(y = "y", x = "fa", color = NULL, alpha = NULL))
  expect_identical(vr$l2$aes, list(y = "Rate", x = "fa", color = NULL, group = 1, alpha = NULL))
  expect_identical(
    vr$l3$aes,
    list(
      y = "Rate", x = "fa", ymin = "CI_low", ymax = "CI_high",
      color = NULL, group = 1, alpha = NULL
    )
  )


  # Estimate predictions ---------------------------
  x <- estimate_relation(model, by = "Sepal.Width")
  # plot(modelbased:::.visualization_recipe(x))
  aes <- modelbased:::.find_aes(x)$aes
  expect_identical(aes$y, "Predicted")
  expect_identical(aes$x, "Sepal.Width")
  expect_null(aes$color)

  x <- estimate_relation(model, by = "Species")
  # plot(modelbased:::.visualization_recipe(x))
  aes <- modelbased:::.find_aes(x)$aes
  expect_identical(aes$y, "Predicted")
  expect_identical(aes$x, "Species")
  expect_null(aes$color)

  x <- estimate_relation(model, by = c("Sepal.Width", "Species"))
  # plot(modelbased:::.visualization_recipe(x))
  aes <- modelbased:::.find_aes(x)$aes
  expect_identical(aes$y, "Predicted")
  expect_identical(aes$x, "Sepal.Width")
  expect_identical(aes$color, "Species")

  x <- estimate_relation(model, by = c("Species", "Sepal.Width"))
  # plot(modelbased:::.visualization_recipe(x))
  aes <- modelbased:::.find_aes(x)$aes
  expect_identical(aes$y, "Predicted")
  expect_identical(aes$x, "Species")
  expect_identical(aes$color, "Sepal.Width")

  x <- estimate_relation(model, by = c("Species", "fac", "Sepal.Width"))
  # plot(modelbased:::.visualization_recipe(x))
  aes <- modelbased:::.find_aes(x)$aes
  expect_identical(aes$y, "Predicted")
  expect_identical(aes$x, "Species")
  expect_identical(aes$color, "fac")
  expect_identical(aes$alpha, "Sepal.Width")

  x <- estimate_relation(model, by = c("Species", "Sepal.Width", "fac"))
  # plot(modelbased:::.visualization_recipe(x))
  aes <- modelbased:::.find_aes(x)$aes
  expect_identical(aes$y, "Predicted")
  expect_identical(aes$x, "Species")
  expect_identical(aes$color, "Sepal.Width")
  expect_identical(deparse(aes$facet), "~fac")

  x <- lm(Sepal.Length ~ Petal.Width * Species * Sepal.Width, data = iris) |>
    estimate_relation(by = c("Petal.Width", "Species", "Sepal.Width"))
  # plot(modelbased:::.visualization_recipe(x))
  aes <- modelbased:::.find_aes(x)$aes
  expect_identical(aes$y, "Predicted")
  expect_identical(aes$x, "Petal.Width")
  expect_identical(aes$color, "Species")
  expect_identical(aes$alpha, "Sepal.Width")

  x <- estimate_relation(model, by = c("Species", "Sepal.Width", "fac", "fac2"))
  # plot(modelbased:::.visualization_recipe(x))
  aes <- modelbased:::.find_aes(x)$aes
  expect_identical(aes$y, "Predicted")
  expect_identical(aes$x, "Species")
  expect_identical(aes$color, "Sepal.Width")
  expect_identical(deparse(aes$grid), "fac2 ~ fac")
  expect_null(aes$facet)

  # Estimate slopes --------------------------------
  x <- estimate_slopes(model, trend = "Sepal.Width")
  expect_error(modelbased:::.find_aes(x)$aes)

  x <- estimate_slopes(model, trend = "Sepal.Width", by = "Species")
  # plot(modelbased:::.visualization_recipe(x))
  aes <- modelbased:::.find_aes(x)$aes
  expect_identical(aes$y, "Slope")
  expect_identical(aes$x, "Species")
  expect_null(aes$color)

  x <- estimate_slopes(model, trend = "Species", by = "Sepal.Width", backend = "marginaleffects")
  aes <- modelbased:::.find_aes(x)$aes
  expect_identical(aes$y, "Slope")
  expect_identical(aes$x, "Species")
  expect_null(aes$color)
  # plot(modelbased:::.visualization_recipe(x))
})
