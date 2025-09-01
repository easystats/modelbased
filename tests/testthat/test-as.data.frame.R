skip_if_not_installed("marginaleffects", minimum_version = "0.29.0")

test_that("as.data.frame.estimate_contrasts()", {
  data(iris)
  model <- lm(Petal.Length ~ Species, data = iris)
  estim <- estimate_means(model, "Species")
  out <- as.data.frame(estim)
  expect_named(
    out,
    c("Species", "Coefficient", "SE", "CI_low", "CI_high", "t", "df")
  )
  out <- as.data.frame(estim, use_responsename = TRUE)
  expect_named(
    out,
    c("Species", "Petal.Length", "SE", "CI_low", "CI_high", "t", "df")
  )
})
