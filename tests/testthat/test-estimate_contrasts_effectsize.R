skip_on_cran()
skip_if_not_installed("emmeans")
skip_if_not_installed("marginaleffects")
skip_if_not_installed("bootES")
skip_if_not_installed("lme4")
skip_on_os("mac")

data(iris)
model <- lm(Sepal.Width ~ Species, data = iris)

test_that("estimate_contrasts - emmeans backend", {
  expect_snapshot(estimate_contrasts(model, backend = "emmeans"),
    variant = "windows"
  )
  expect_snapshot(estimate_contrasts(model, effectsize = "none", backend = "emmeans"),
    variant = "windows"
  )
  expect_snapshot(estimate_contrasts(model, effectsize = "emmeans", backend = "emmeans"),
    variant = "windows"
  )
  expect_snapshot(estimate_contrasts(model, effectsize = "marginal", backend = "emmeans"),
    variant = "windows"
  )
  set.seed(100)
  expect_snapshot(estimate_contrasts(model, effectsize = "boot", backend = "emmeans"),
    variant = "windows"
  )
  set.seed(100)
  expect_snapshot(estimate_contrasts(model,
    effectsize = "boot",
    boot_type = "akp.robust.d",
    backend = "emmeans"
  ), variant = "windows")
})

test_that("estimate_contrasts - marginaleffects backend", {
  expect_snapshot(estimate_contrasts(model, backend = "marginaleffects"), variant = "windows")
  expect_snapshot(estimate_contrasts(model, effectsize = "none", backend = "marginaleffects"),
    variant = "windows"
  )
  expect_error(
    estimate_contrasts(model, effectsize = "emmeans", backend = "marginaleffects"),
    "`effectsize = emmeans` only possible with `backend = emmeans`"
  )
  expect_snapshot(estimate_contrasts(model, effectsize = "marginal", backend = "marginaleffects"),
    variant = "windows"
  )
  skip() # marginaleffects backend isn't working with bootES yet
  set.seed(100)
  expect_snapshot(estimate_contrasts(model, effectsize = "boot", backend = "marginaleffects"),
    variant = "windows"
  )
  set.seed(100)
  expect_snapshot(estimate_contrasts(model,
    effectsize = "boot",
    boot_type = "akp.robust.d",
    backend = "marginaleffects"
  ), variant = "windows")
})

test_that("estimate_contrasts - random effects", {
  sleepstudy <- lme4::sleepstudy
  sleepstudy$Days_factor <- cut(sleepstudy$Days, breaks = 3, labels = c("Low", "Medium", "High"))
  model_random_effects <- lme4::lmer(Reaction ~ Days_factor + (1 | Subject), data = sleepstudy)

  expect_error(
    estimate_contrasts(model_random_effects, effectsize = "emmeans", backend = "emmeans"),
    "We strongly recommend not using"
  )
})
