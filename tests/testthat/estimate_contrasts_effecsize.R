skip_on_cran()
skip_if_not_installed("emmeans")
skip_if_not_installed("marginaleffects")
skip_if_not_installed("bootES")
skip_on_os("mac")

test_that("estimate_contrasts - emmeans backend", {
  data(iris)

  library(modelbased)
  model <- lm(Sepal.Width ~ Species, data = iris)

  expect_snapshot(estimate_contrasts(model, backend = "emmeans"))
  expect_snapshot(estimate_contrasts(model, effectsize = "none", backend = "emmeans"))
  expect_snapshot(estimate_contrasts(model, effectsize = "emmeans", backend = "emmeans"))
  expect_snapshot(estimate_contrasts(model, effectsize = "marginal", backend = "emmeans"))
  set.seed(100)
  expect_snapshot(estimate_contrasts(model, effectsize = "bootES", backend = "emmeans"))
  set.seed(100)
  expect_snapshot(estimate_contrasts(model,
    effectsize = "bootES",
    bootES_type = "akp.robust.d",
    backend = "emmeans"
  ))
})

test_that("estimate_contrasts - marginaleffects backend", {
  data(iris)

  library(modelbased)
  model <- lm(Sepal.Width ~ Species, data = iris)

  expect_snapshot(estimate_contrasts(model, backend = "marginaleffects"))
  expect_snapshot(estimate_contrasts(model, effectsize = "none", backend = "marginaleffects"))
  expect_snapshot(estimate_contrasts(model, effectsize = "marginal", backend = "marginaleffects"))
  skip() # marginaleffects backend isn't working with those methods yet
  expect_snapshot(estimate_contrasts(model, effectsize = "emmeans", backend = "marginaleffects"))
  set.seed(100)
  expect_snapshot(estimate_contrasts(model, effectsize = "bootES", backend = "marginaleffects"))
  set.seed(100)
  expect_snapshot(estimate_contrasts(model,
    effectsize = "bootES",
    bootES_type = "akp.robust.d",
    backend = "marginaleffects"
  ))
})
