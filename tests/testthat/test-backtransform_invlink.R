skip_on_cran()
skip_if_not_installed("marginaleffects", minimum_version = "0.29.0")
skip_if_not_installed("glmmTMB")

test_that("estimate_means correct inverse link for glmmTMB", {
  data(mtcars)
  d <- mtcars
  d$count <- rep_len(c(0, 0, 0, 0, 1, 2, 4), nrow(mtcars))
  m <- glmmTMB::glmmTMB(
    count ~ cyl,
    data = datawizard::data_modify(d, cyl = as.factor(cyl)),
    family = "poisson"
  )
  out1 <- estimate_means(m, backend = "marginaleffects", predict = "inverse_link")
  out2 <- estimate_means(m, backend = "emmeans")
  expect_equal(out1$Mean, out2$Rate, tolerance = 1e-4)
  expect_equal(out1$CI_low, out2$CI_low, tolerance = 1e-4)
  expect_equal(out1$CI_high, out2$CI_high, tolerance = 1e-4)
})


test_that("estimate_means correct inverse link for glm", {
  set.seed(5)
  data <- data.frame(
    outcome = rbinom(100, 1, 0.5),
    var1 = rbinom(100, 1, 0.1),
    var2 = rnorm(100, 10, 7)
  )
  m <- glm(
    outcome ~ var1 * var2,
    data = data,
    family = binomial(link = "logit")
  )

  out <- estimate_relation(m, by = c("var1", "var2"))
  expect_true(all(out$CI_low >= 0 & out$CI_low <= 1))
  expect_true(all(out$CI_high >= 0 & out$CI_high <= 1))

  out <- estimate_means(m, by = c("var1", "var2"))
  expect_true(all(out$CI_low >= 0 & out$CI_low <= 1))
  expect_true(all(out$CI_high >= 0 & out$CI_high <= 1))

  out <- estimate_means(m, by = c("var1", "var2"), estimate = "population")
  expect_true(all(out$CI_low >= 0 & out$CI_low <= 1))
  expect_true(all(out$CI_high >= 0 & out$CI_high <= 1))
})


test_that("estimate_means correct inverse link for glmer", {
  data(efc, package = "modelbased")

  x <- which(efc$negc7d == 1 & efc$c172code == 3)
  efc$negc7d[x[sample.int(length(x), round(length(x) / 1.1))]] <- 0
  efc$c172code <- as.factor(efc$c172code)
  fit <- lme4::glmer(
    negc7d ~ c12hour + e42dep + c161sex + c172code + (1 | grp),
    data = efc,
    family = binomial(link = "logit")
  )
  out1 <- estimate_means(fit, "c172code", predict = "inverse_link")
  out2 <- estimate_relation(fit, by = "c172code", verbose = FALSE)
  expect_equal(out1$Probability, out2$Predicted, tolerance = 1e-2)
  expect_equal(out1$CI_low, out2$CI_low, tolerance = 1e-2)
  expect_true(all(out1$CI_low >= 0 & out1$CI_low <= 1))
  expect_true(all(out1$CI_high >= 0 & out1$CI_high <= 1))
})


test_that("estimate_means correct inverse link for stan-glm", {
  skip_if_not_installed("curl")
  skip_if_offline()
  skip_if_not_installed("httr2")
  skip_if_not_installed("rstanarm")

  m <- insight::download_model("stanreg_glm_1")
  skip_if(is.null(m))

  out <- estimate_means(m, "wt = [sd]")
  expect_equal(out$Median, c(0.81144, 0.38844, 0.08599), tolerance = 1e-4)
  expect_equal(out$CI_low, c(0.54837, 0.2029, 0.01342), tolerance = 1e-4)
})
