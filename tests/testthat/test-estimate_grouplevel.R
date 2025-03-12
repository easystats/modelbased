skip_if(utils::packageVersion("insight") <= "1.1.0")
skip_if(utils::packageVersion("parameters") <= "0.24.1")

test_that("estimate_grouplevel - lme4", {
  skip_if_not_installed("lme4")
  set.seed(333)
  data <- lme4::sleepstudy

  # Random intercept
  model <- lme4::lmer(Reaction ~ Days + (1 | Subject), data = data)
  random <- estimate_grouplevel(model)
  expect_equal(nrow(random), length(unique(data$Subject)))
  expect_equal(nrow(reshape_grouplevel(random)), nrow(data))

  # 2 random intercepts
  model <- lme4::lmer(mpg ~ wt + (1 | gear) + (1 | carb), data = mtcars)
  random <- estimate_grouplevel(model)
  expect_equal(nrow(random), length(c(unique(mtcars$gear), unique(mtcars$carb))))
  expect_equal(nrow(reshape_grouplevel(random)), nrow(mtcars))

  # Random slope and intercept
  model <- lme4::lmer(Reaction ~ Days + (1 + Days | Subject), data = data)
  random <- estimate_grouplevel(model)
  expect_equal(nrow(random), 2 * length(unique(data$Subject)))
  expect_equal(nrow(reshape_grouplevel(random)), nrow(data))

  # Nested random factors
  set.seed(33)
  data$grp <- sample(letters[1:5], size = 180, replace = TRUE)
  data$subgrp <- NA
  for (i in letters[1:5]) {
    filter_group <- data$grp == i
    data$subgrp[filter_group] <- sample(LETTERS, size = sum(filter_group), replace = TRUE)
  }
  model <- lme4::lmer(Reaction ~ Days + (1 | grp / subgrp) + (1 | Subject), data = data)
  random <- estimate_grouplevel(model)
  expect_equal(nrow(random), sum(sapply(coef(model), nrow)))

  reshaped <- reshape_grouplevel(random)
  expect_equal(nrow(reshaped), nrow(data))
  ref <- insight::get_data(model, verbose = FALSE)[insight::find_random(model, split_nested = TRUE, flatten = TRUE)]
  all(reshaped$Subject == ref$Subject)
  all(reshaped$grp == ref$grp)
  all(reshaped$subgrp == ref$subgrp)
})

test_that("estimate_grouplevel - lme4", {
  skip_on_cran()
  skip_if_not_installed("lme4")
  data(iris)
  d <- iris
  d$Group <- as.factor(rep(c("G1", "G2", "G3"), each = 50))

  m <- lme4::lmer(Sepal.Width ~ Petal.Width + (Petal.Width | Group), data = d)

  out <- estimate_grouplevel(m)
  expect_identical(dim(out), c(6L, 8L))
  expect_named(out, c("Group", "Level", "Parameter", "Coefficient", "SE", "CI", "CI_low", "CI_high"))

  out <- estimate_grouplevel(m, type = "total")
  expect_identical(dim(out), c(6L, 4L))
  expect_named(out, c("Group", "Level", "Parameter", "Coefficient"))
})

test_that("estimate_grouplevel - glmmTMB", {
  skip_on_cran()
  skip_if_not_installed("glmmTMB")
  data(iris)
  d <- iris
  d$Group <- as.factor(rep(c("G1", "G2", "G3"), each = 50))

  m1 <- glmmTMB::glmmTMB(
    Sepal.Width ~ Petal.Width + (Petal.Width | Group),
    data = d
  )

  out <- estimate_grouplevel(m1)
  expect_identical(dim(out), c(6L, 8L))
  expect_named(out, c("Group", "Level", "Parameter", "Coefficient", "SE", "CI", "CI_low", "CI_high"))

  out <- estimate_grouplevel(m1, type = "total")
  expect_identical(dim(out), c(6L, 4L))
  expect_named(out, c("Group", "Level", "Parameter", "Coefficient"))
})

test_that("estimate_grouplevel - Bayesian", {
  skip_on_cran()
  skip_if_not_installed("curl")
  skip_if_offline()
  skip_if_not_installed("httr2")
  skip_if_not_installed("brms")

  m <- insight::download_model("brms_mixed_10")
  skip_if(is.null(m))

  out <- estimate_grouplevel(m)
  expect_identical(dim(out), c(6L, 8L))
  expect_named(out, c("Group", "Level", "Parameter", "Median", "MAD", "CI", "CI_low", "CI_high"))

  out <- estimate_grouplevel(m, type = "total", dispersion = FALSE)
  expect_identical(dim(out), c(6L, 7L))
  expect_named(out, c("Group", "Level", "Parameter", "Median", "CI", "CI_low", "CI_high"))

  m <- insight::download_model("brms_sigma_3")
  skip_if(is.null(m))

  out <- estimate_grouplevel(m)
  expect_identical(dim(out), c(12L, 9L))
  expect_named(out, c("Component", "Group", "Level", "Parameter", "Median", "MAD", "CI", "CI_low", "CI_high"))

  out <- estimate_grouplevel(m, type = "total")
  expect_identical(dim(out), c(12L, 9L))
  expect_named(out, c("Component", "Group", "Level", "Parameter", "Median", "MAD", "CI", "CI_low", "CI_high"))
})
