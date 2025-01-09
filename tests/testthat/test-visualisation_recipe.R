test_that("visualization_recipe - .find_aes", {
  skip_if_not_installed("emmeans")
  skip_if_not_installed("marginaleffects")

  model <- lm(Sepal.Length ~ Species * Sepal.Width, data = iris)

  # library(see)

  # Estimate means -------------------------------------
  x <- estimate_means(model, by="Species")
  plot(modelbased:::.visualization_recipe(x))
  # aes <- modelbased:::.find_aes(x)$aes
  # expect_equal(aes$y, "Mean")
  # expect_equal(aes$x, "Species")

  x <- estimate_means(model, by="Sepal.Width")
  plot(modelbased:::.visualization_recipe(x))
  # aes <- modelbased:::.find_aes(x)$aes
  # expect_equal(aes$y, "Mean")
  # expect_equal(aes$x, "Sepal.Width")

  x <- estimate_means(model, by=c("Sepal.Width", "Species"))
  plot(modelbased:::.visualization_recipe(x))
  # aes <- modelbased:::.find_aes(x)$aes
  # expect_equal(aes$y, "Mean")
  # expect_equal(aes$x, "Sepal.Width")

  x <- estimate_means(model, by=c("Species", "Sepal.Width"))
  plot(modelbased:::.visualization_recipe(x))
  # aes <- modelbased:::.find_aes(x)$aes
  # expect_equal(aes$y, "Mean")
  # expect_equal(aes$x, "Species")


  # Estimate predictions ---------------------------
  x <- estimate_relation(model, by=c("Sepal.Width"))
  plot(modelbased:::.visualization_recipe(x))
  # aes <- modelbased:::.find_aes(x)$aes
  # expect_equal(aes$y, "Predicted")
  # expect_equal(aes$x, "Sepal.Width")
  # expect_true(is.null(aes$color))

  x <- estimate_relation(model, by=c("Species"))
  plot(modelbased:::.visualization_recipe(x))
  # aes <- modelbased:::.find_aes(x)$aes
  # expect_equal(aes$y, "Predicted")
  # expect_equal(aes$x, "Species")
  # expect_true(is.null(aes$color))

  x <- estimate_relation(model, by=c("Sepal.Width", "Species"))
  plot(modelbased:::.visualization_recipe(x))
  # aes <- modelbased:::.find_aes(x)$aes
  # expect_equal(aes$y, "Predicted")
  # expect_equal(aes$x, "Sepal.Width")
  # expect_equal(aes$color, "Species")

  x <- estimate_relation(model, by=c("Species", "Sepal.Width"))
  plot(modelbased:::.visualization_recipe(x))
  # aes <- modelbased:::.find_aes(x)$aes
  # expect_equal(aes$y, "Predicted")
  # expect_equal(aes$x, "Species")
  # expect_equal(aes$color, "Sepal.Width")

  # Estimate slopes --------------------------------
  # x <- estimate_slopes(model, trend="Sepal.Width")
  # expect_error(modelbased:::.find_aes(x)$aes)

  x <- estimate_slopes(model, trend="Sepal.Width", by="Species")
  plot(modelbased:::.visualization_recipe(x))
  # aes <- modelbased:::.find_aes(x)$aes
  # expect_equal(aes$y, "Predicted")
  # expect_equal(aes$x, "Species")
  # expect_true(is.null(aes$color))

})

