skip_on_cran()
skip_if_not_installed("marginaleffects")
skip_if_not_installed("emmeans")
skip_if_not_installed("lme4")

test_that("estimate_means bias_correction", {
  set.seed(123)
  dat <- data.frame(
    outcome = rbinom(n = 100, size = 1, prob = 0.35),
    var_binom = as.factor(rbinom(n = 100, size = 1, prob = 0.7)),
    var_cont = rnorm(n = 100, mean = 10, sd = 7),
    grp = as.factor(sample(letters[1:4], size = 100, replace = TRUE))
  )
  m1 <- lme4::glmer(
    outcome ~ var_binom + var_cont + (1 | grp),
    data = dat,
    family = binomial(link = "logit")
  )
  set.seed(123)
  out <- estimate_means(m1, "var_binom", predict = "inverse_link")
  expect_equal(out$Probability, c(0.38508, 0.36696), tolerance = 1e-4)
  out <- estimate_means(m1, "var_binom", bias_correction = TRUE)
  expect_equal(out$Probability, c(0.4746, 0.46863), tolerance = 1e-4)
  out <- estimate_means(m1, "var_binom", bias_correction = TRUE, sigma = 2.5)
  expect_equal(out$Probability, c(0.55516, 0.56012), tolerance = 1e-4)
  out2 <- as.data.frame(emmeans::emmeans(m1, "var_binom", bias.adj = TRUE, type = "response", sigma = 2.5))
  expect_equal(out$Probability, out2$prob, tolerance = 1e-2)
})
