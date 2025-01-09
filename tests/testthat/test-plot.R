skip_on_os(c("mac", "solaris"))
skip_if_not_installed("ggplot2")
skip_if_not_installed("see")
skip_if_not_installed("vdiffr")
skip_on_cran()

test_that("plots emmeans", {
  model <- lm(Sepal.Length ~ Species * Sepal.Width, data = iris)

  # Estimate means -------------------------------------
  x <- estimate_means(model, by = "Species")
  vdiffr::expect_doppelganger(
    "plot-means-1",
    plot(modelbased::visualisation_recipe(x, show_data = FALSE))
  )
  x <- estimate_means(model, by = "Sepal.Width")
  vdiffr::expect_doppelganger(
    "plot-means-2",
    plot(modelbased::visualisation_recipe(x, show_data = FALSE))
  )
  x <- estimate_means(model, by = c("Sepal.Width", "Species"))
  vdiffr::expect_doppelganger(
    "plot-means-3",
    plot(modelbased::visualisation_recipe(x, show_data = FALSE))
  )
  x <- estimate_means(model, by = c("Species", "Sepal.Width"))
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
