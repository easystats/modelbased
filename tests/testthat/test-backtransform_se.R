skip_on_cran()
skip_if_not_installed("marginaleffects", minimum_version = "0.29.0")
skip_if_not_installed("emmeans")

test_that("backtransform_predictions uses link derivative for SE across GLM links", {
  # Binomial logit
  m_logit <- glm(am ~ factor(cyl) + wt, data = mtcars, family = binomial("logit"))
  out_mb <- estimate_means(m_logit, by = "cyl", predict = "inverse_link")
  out_em <- as.data.frame(emmeans::emmeans(m_logit, ~cyl, type = "response"))
  expect_equal(out_mb$Probability, out_em$prob, tolerance = 1e-4)
  expect_equal(out_mb$SE, out_em$SE, tolerance = 1e-4)
  expect_equal(out_mb$CI_low, out_em$asymp.LCL, tolerance = 1e-4)
  expect_equal(out_mb$CI_high, out_em$asymp.UCL, tolerance = 1e-4)

  # Binomial probit
  m_probit <- glm(am ~ factor(cyl), data = mtcars, family = binomial("probit"))
  out_mb <- estimate_means(m_probit, by = "cyl", predict = "inverse_link")
  out_em <- as.data.frame(emmeans::emmeans(m_probit, ~cyl, type = "response"))
  expect_equal(out_mb$Probability, out_em$prob, tolerance = 1e-4)
  expect_equal(out_mb$SE, out_em$SE, tolerance = 1e-4)

  # Poisson sqrt link
  m_sqrt <- glm(breaks ~ wool + tension, data = warpbreaks, family = poisson("sqrt"))
  out_mb <- estimate_means(m_sqrt, by = "tension", predict = "inverse_link")
  out_em <- as.data.frame(emmeans::emmeans(m_sqrt, ~tension, type = "response"))
  expect_equal(out_mb$Mean, out_em$rate, tolerance = 1e-4)
  expect_equal(out_mb$SE, out_em$SE, tolerance = 1e-4)

  # Gamma inverse link (decreasing inverse link: SE positive and CI_low <= CI_high)
  m_gamma <- glm(mpg ~ factor(cyl), data = mtcars, family = Gamma("inverse"))
  out_mb <- estimate_means(m_gamma, by = "cyl", predict = "inverse_link")
  out_em <- as.data.frame(emmeans::emmeans(m_gamma, ~cyl, type = "response"))
  expect_equal(out_mb$Mean, out_em$response, tolerance = 1e-4)
  expect_equal(out_mb$SE, out_em$SE, tolerance = 1e-4)
  expect_true(all(out_mb$CI_low < out_mb$CI_high))
  expect_equal(out_mb$CI_low, out_em$lower.CL, tolerance = 1e-4)
  expect_equal(out_mb$CI_high, out_em$upper.CL, tolerance = 1e-4)
})

test_that("backtransform_predictions uses bias-adjusted derivative for SE with bias_correction", {
  skip_if_not_installed("lme4")
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
    family = binomial("logit")
  )
  out_mb <- estimate_means(
    m1,
    "var_binom",
    bias_correction = TRUE,
    sigma = 2.5,
    re.form = NA
  )
  out_em <- as.data.frame(emmeans::emmeans(
    m1,
    "var_binom",
    type = "response",
    bias.adjust = TRUE,
    sigma = 2.5
  ))
  expect_equal(out_mb$Probability, out_em$prob, tolerance = 1e-4)
  expect_equal(out_mb$SE, out_em$SE, tolerance = 1e-4)
})
