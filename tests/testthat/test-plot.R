skip_on_os(c("mac", "solaris"))
skip_if_not_installed("ggplot2")
skip_if_not_installed("see")
skip_if_not_installed("vdiffr")
skip_on_cran()

test_that("plots", {
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
