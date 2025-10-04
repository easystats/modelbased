skip_if(utils::packageVersion("insight") <= "1.1.0")
skip_if(utils::packageVersion("parameters") <= "0.24.1")

test_that("estimate_grouplevel - errors", {
  m <- lm(mpg ~ gear, data = mtcars)
  expect_error(estimate_grouplevel(m), regex = "Model must be a mixed")
})

test_that("estimate_grouplevel - lme4", {
  skip_if_not_installed("lme4")
  set.seed(333)
  data <- lme4::sleepstudy

  # Random intercept
  model <- lme4::lmer(Reaction ~ Days + (1 | Subject), data = data)
  random <- estimate_grouplevel(model)
  expect_equal(nrow(random), length(unique(data$Subject)))
  expect_equal(nrow(reshape_grouplevel(random)), length(unique(data$Subject)))

  # 2 random intercepts
  model <- lme4::lmer(mpg ~ wt + (1 | gear) + (1 | carb), data = mtcars)
  random <- estimate_grouplevel(model)
  expect_equal(nrow(random), length(c(unique(mtcars$gear), unique(mtcars$carb))))
  expect_equal(nrow(reshape_grouplevel(random, group = "gear")), length(unique(mtcars$gear)))
  expect_equal(nrow(reshape_grouplevel(random, group = "carb")), length(unique(mtcars$carb)))

  # Random slope and intercept
  model <- lme4::lmer(Reaction ~ Days + (1 + Days | Subject), data = data)
  random <- estimate_grouplevel(model)
  expect_equal(nrow(random), 2 * length(unique(data$Subject)))
  expect_equal(nrow(reshape_grouplevel(random)), length(unique(data$Subject)))

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

  reshaped <- reshape_grouplevel(random, group = "grp")
  expect_equal(nrow(reshaped), length(unique(data$grp)))
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

test_that("estimate_grouplevel - Bayesian, brms", {
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

test_that("estimate_grouplevel - Bayesian, rstanarm", {
  skip_on_cran()
  skip_if_not_installed("curl")
  skip_if_offline()
  skip_if_not_installed("httr2")
  skip_if_not_installed("rstanarm")

  m <- insight::download_model("stanreg_merMod_1")
  skip_if(is.null(m))

  out <- estimate_grouplevel(m)
  expect_identical(dim(out), c(3L, 8L))
  expect_named(out, c("Group", "Level", "Parameter", "Median", "MAD", "CI", "CI_low", "CI_high"))

  out <- estimate_grouplevel(m, dispersion = FALSE)
  expect_identical(dim(out), c(3L, 7L))

  out <- estimate_grouplevel(m, type = "total", dispersion = FALSE)
  expect_identical(dim(out), c(3L, 4L))
  expect_named(out, c("Group", "Level", "Parameter", "Coefficient"))
})


skip_if_not_installed("withr")

withr::with_environment(
  new.env(),
  test_that("estimate_grouplevel, coxme", {
    skip_on_cran()
    skip_if_not_installed("coxme")
    skip_if_not_installed("survival")

    Surv <- survival::Surv
    rats <- survival::rats
    lung <- survival::lung

    set.seed(1234)
    rats$grp <- sample(letters[1:3], nrow(rats), replace = TRUE)

    data(eortc, package = "coxme")
    d <<- coxme::eortc
    d2 <<- rats
    d3 <<- lung

    m1 <- coxme::coxme(Surv(y, uncens) ~ trt + (1 | center), data = d)
    m2 <- coxme::coxme(Surv(time, status) ~ ph.ecog + age + (1 | inst), d3)
    m3 <- coxme::coxme(Surv(time, status) ~ rx + (1 + rx | litter) + (1 | grp), d2)

    out <- estimate_grouplevel(m1)
    expect_identical(dim(out), c(37L, 5L))
    out <- estimate_grouplevel(m2)
    expect_identical(dim(out), c(18L, 5L))
    out <- estimate_grouplevel(m3)
    expect_identical(dim(out), c(203L, 5L))
  })
)

test_that("estimate_grouplevel type='marginal'", {
  skip_on_cran()
  skip_if_not_installed("lme4")
  data(mtcars)

  model <- lme4::lmer(mpg ~ hp + (1 | carb), data = mtcars)
  gl1 <- estimate_grouplevel(model, type = "random")
  gl3 <- estimate_grouplevel(model, type = "marginal")
  expect_s3_class(gl3, "estimate_grouplevel")
  expect_equal(dim(gl3), c(6, 6))
  expect_equal(
    colnames(gl3),
    c("Group", "Level", "Parameter", "Coefficient", "CI_low", "CI_high")
  )
  expect_gt(as.numeric(cor.test(gl1$Coefficient, gl3$Coefficient)$estimate), 0.99)

  model <- lme4::lmer(mpg ~ hp + (1 + hp | carb), data = mtcars)
  gl1 <- estimate_grouplevel(model, type = "random")
  gl3 <- estimate_grouplevel(model, type = "marginal")
  expect_s3_class(gl3, "estimate_grouplevel")
  expect_equal(dim(gl3), c(12, 6))
  expect_equal(
    colnames(gl3),
    c("Group", "Level", "Parameter", "Coefficient", "CI_low", "CI_high")
  )
  r <- cor.test(
    gl1[gl1$Parameter == "(Intercept)", "Coefficient"],
    gl3[gl3$Parameter == "(Intercept)", "Coefficient"]
  )
  expect_gt(r$estimate, 0.99)
  r <- cor.test(gl1[gl1$Parameter == "hp", "Coefficient"], gl3[gl3$Parameter == "hp", "Coefficient"])
  expect_gt(r$estimate, 0.99)

  model <- lme4::lmer(mpg ~ hp + (1 + hp | carb) + (1 | gear), data = mtcars)
  gl1 <- estimate_grouplevel(model, type = "random")
  gl3 <- estimate_grouplevel(model, type = "marginal")
  expect_s3_class(gl3, "estimate_grouplevel")
  expect_equal(dim(gl3), c(15, 6))
  expect_equal(
    colnames(gl3),
    c("Group", "Level", "Parameter", "Coefficient", "CI_low", "CI_high")
  )
  r <- cor.test(gl1[gl1$Parameter == "(Intercept)" & gl1$Group == "carb", "Coefficient"], gl3[gl3$Parameter == "(Intercept)" & gl3$Group == "carb", "Coefficient"])
  expect_gt(r$estimate, 0.99)
  r <- cor.test(gl1[gl1$Parameter == "(Intercept)" & gl1$Group == "gear", "Coefficient"], gl3[gl3$Parameter == "(Intercept)" & gl3$Group == "gear", "Coefficient"])
  r <- cor.test(gl1[gl1$Parameter == "hp", "Coefficient"], gl3[gl3$Parameter == "hp", "Coefficient"])
  expect_gt(r$estimate, 0.99)
})

test_that("estimate_grouplevel type='marginal' correlations", {
  skip_on_cran()
  skip_if_not_installed("lme4")
  data(mtcars)

  model <- lme4::lmer(mpg ~ hp + (1 + hp | carb) + (1 | gear), data = mtcars)
  m1 <- estimate_grouplevel(model, type = "random")
  m2 <- estimate_grouplevel(model, type = "total")
  m3 <- estimate_grouplevel(model, type = "marginal")

  # merge m1 and m3 to compare
  m1_intercept <- m1[m1$Parameter == "(Intercept)", ]
  m3_intercept <- m3[m3$Parameter == "(Intercept)", ]
  merged_intercepts <- merge(m1_intercept, m3_intercept, by = c("Group", "Level"))
  expect_gt(cor(merged_intercepts$Coefficient.x, merged_intercepts$Coefficient.y), 0.89)

  m1_hp <- m1[m1$Parameter == "hp", ]
  m3_hp <- m3[m3$Parameter == "hp", ]
  merged_hp <- merge(m1_hp, m3_hp, by = c("Group", "Level"))
  expect_gt(cor(merged_hp$Coefficient.x, merged_hp$Coefficient.y), 0.99)

  # merge m2 and m3 to compare
  m2_intercept <- m2[m2$Parameter == "(Intercept)", ]
  m3_intercept <- m3[m3$Parameter == "(Intercept)", ]
  m2_intercept$Level <- as.character(m2_intercept$Level)
  m3_intercept$Level <- as.character(m3_intercept$Level)
  merged_intercepts <- merge(m2_intercept, m3_intercept, by = c("Group", "Level"))
  expect_gt(cor(merged_intercepts$Coefficient.x, merged_intercepts$Coefficient.y), 0.89)

  m2_hp <- m2[m2$Parameter == "hp", ]
  m3_hp <- m3[m3$Parameter == "hp", ]
  m2_hp$Level <- as.character(m2_hp$Level)
  m3_hp$Level <- as.character(m3_hp$Level)
  merged_hp <- merge(m2_hp, m3_hp, by = c("Group", "Level"))
  expect_gt(cor(merged_hp$Coefficient.x, merged_hp$Coefficient.y), 0.99)
})
