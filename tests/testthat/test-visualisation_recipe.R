test_that("visualization_recipe", {
  skip_if_not_installed("emmeans")
  skip_if_not_installed("marginaleffects")

  # library(see)
  # library(modelbased)

  data <- iris
  data$fac <- rep(c("A", "B"), length.out=150)
  data$fac2 <- rep(c("X", "X", "X", "Y", "Y", "Y"), length.out=150)
  model <- lm(Sepal.Length ~ Species * fac * Sepal.Width * fac2, data = data)

  # Estimate means -------------------------------------
  x <- estimate_means(model, by="Species")
  # plot(modelbased:::.visualization_recipe(x))
  aes <- modelbased:::.find_aes(x)$aes
  expect_equal(aes$y, "Mean")
  expect_equal(aes$x, "Species")

  x <- estimate_means(model, by="Sepal.Width")
  # plot(modelbased:::.visualization_recipe(x))
  aes <- modelbased:::.find_aes(x)$aes
  expect_equal(aes$y, "Mean")
  expect_equal(aes$x, "Sepal.Width")

  x <- estimate_means(model, by=c("Sepal.Width", "Species"))
  # plot(modelbased:::.visualization_recipe(x))
  aes <- modelbased:::.find_aes(x)$aes
  expect_equal(aes$y, "Mean")
  expect_equal(aes$x, "Sepal.Width")

  x <- estimate_means(model, by=c("Species", "Sepal.Width"))
  # plot(modelbased:::.visualization_recipe(x))
  aes <- modelbased:::.find_aes(x)$aes
  expect_equal(aes$y, "Mean")
  expect_equal(aes$x, "Species")

  x <- estimate_means(model, by=c("Species", "fac", "Sepal.Width"))
  # plot(modelbased:::.visualization_recipe(x))
  aes <- modelbased:::.find_aes(x)$aes
  expect_equal(aes$y, "Mean")
  expect_equal(aes$x, "Species")
  expect_equal(aes$color, "fac")


  # Estimate predictions ---------------------------
  x <- estimate_relation(model, by=c("Sepal.Width"))
  # plot(modelbased:::.visualization_recipe(x))
  aes <- modelbased:::.find_aes(x)$aes
  expect_equal(aes$y, "Predicted")
  expect_equal(aes$x, "Sepal.Width")
  expect_true(is.null(aes$color))

  x <- estimate_relation(model, by=c("Species"))
  # plot(modelbased:::.visualization_recipe(x))
  aes <- modelbased:::.find_aes(x)$aes
  expect_equal(aes$y, "Predicted")
  expect_equal(aes$x, "Species")
  expect_true(is.null(aes$color))

  x <- estimate_relation(model, by=c("Sepal.Width", "Species"))
  # plot(modelbased:::.visualization_recipe(x))
  aes <- modelbased:::.find_aes(x)$aes
  expect_equal(aes$y, "Predicted")
  expect_equal(aes$x, "Sepal.Width")
  expect_equal(aes$color, "Species")

  x <- estimate_relation(model, by=c("Species", "Sepal.Width"))
  # plot(modelbased:::.visualization_recipe(x))
  aes <- modelbased:::.find_aes(x)$aes
  expect_equal(aes$y, "Predicted")
  expect_equal(aes$x, "Species")
  expect_equal(aes$color, "Sepal.Width")

  x <- estimate_relation(model, by=c("Species", "fac", "Sepal.Width"))
  # plot(modelbased:::.visualization_recipe(x))
  aes <- modelbased:::.find_aes(x)$aes
  expect_equal(aes$y, "Predicted")
  expect_equal(aes$x, "Species")
  expect_equal(aes$color, "fac")
  expect_equal(aes$alpha, "Sepal.Width")

  x <- estimate_relation(model, by=c("Species", "Sepal.Width", "fac"))
  # plot(modelbased:::.visualization_recipe(x))
  aes <- modelbased:::.find_aes(x)$aes
  expect_equal(aes$y, "Predicted")
  expect_equal(aes$x, "Species")
  expect_equal(aes$color, "Sepal.Width")
  expect_equal(aes$alpha, "fac")

  x <- lm(Sepal.Length ~ Petal.Width * Species * Sepal.Width, data = iris) |>
    estimate_relation(by=c("Petal.Width", "Species", "Sepal.Width"))
  # plot(modelbased:::.visualization_recipe(x))
  aes <- modelbased:::.find_aes(x)$aes
  expect_equal(aes$y, "Predicted")
  expect_equal(aes$x, "Petal.Width")
  expect_equal(aes$color, "Species")
  expect_equal(aes$alpha, "Sepal.Width")

  x <- estimate_relation(model, by=c("Species", "Sepal.Width", "fac", "fac2"))
  # plot(modelbased:::.visualization_recipe(x))
  aes <- modelbased:::.find_aes(x)$aes
  expect_equal(aes$y, "Predicted")
  expect_equal(aes$x, "Species")
  expect_equal(aes$color, "Sepal.Width")
  expect_equal(aes$alpha, "fac")
  expect_true("fac2" %in% as.character(aes$facet))

  # Estimate slopes --------------------------------
  x <- estimate_slopes(model, trend="Sepal.Width")
  expect_error(modelbased:::.find_aes(x)$aes)

  x <- estimate_slopes(model, trend="Sepal.Width", by="Species")
  # plot(modelbased:::.visualization_recipe(x))
  aes <- modelbased:::.find_aes(x)$aes
  expect_equal(aes$y, "Coefficient")
  expect_equal(aes$x, "Species")
  expect_true(is.null(aes$color))

  # broken
  # x <- estimate_slopes(model, trend="Species", by="Sepal.Width")
  # plot(modelbased:::.visualization_recipe(x))

})