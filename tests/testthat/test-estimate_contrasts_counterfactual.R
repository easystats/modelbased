skip_on_cran()
skip_if_not_installed("marginaleffects", minimum_version = "0.29.0")
skip_on_os("mac")
skip_if_not_installed("MatchIt")
skip_if_not_installed("sandwich")

test_that("estimate_contrast, counterfactual", {
  data("lalonde", package = "MatchIt")
  m <- glm(treat ~ age + educ + race + re74, data = lalonde, family = binomial)

  # IPW
  tmp <- marginaleffects::predictions(m, newdata = lalonde)
  lalonde$wts <- ifelse(tmp$treat == 1, 1 / tmp$estimate, 1 / (1 - tmp$estimate))

  mod <- lm(re78 ~ treat * (age + educ + race + re74), data = lalonde, weights = wts)

  out1 <- marginaleffects::avg_comparisons(mod, variables = "treat", wts = "wts", vcov = "HC3")

  out2 <- estimate_contrasts(
    mod,
    contrast = "treat",
    estimate = "population",
    weights = "wts",
    vcov = "HC3"
  )
  expect_equal(out1$estimate, out2$Difference, tolerance = 1e-4)
  expect_named(
    out2,
    c("Level1", "Level2", "Difference", "SE", "CI_low", "CI_high", "t", "df", "p")
  )
  expect_identical(
    attributes(out2)$table_title,
    c("Counterfactual Contrasts Analysis (G-computation)", "blue")
  )

  out1 <- marginaleffects::avg_comparisons(
    mod,
    variables = "treat",
    by = "treat",
    wts = "wts",
    vcov = "HC3"
  )

  out2 <- estimate_contrasts(
    mod,
    contrast = "treat",
    by = "treat",
    estimate = "population",
    weights = "wts",
    vcov = "HC3"
  )

  expect_equal(out1$estimate, out2$Difference, tolerance = 1e-4)
  expect_named(
    out2,
    c("Level1", "Level2", "treat", "Difference", "SE", "CI_low", "CI_high", "t", "df", "p")
  )

  # transformed response
  mod <- lm(log1p(re78) ~ treat * (age + educ + race + re74), data = lalonde, weights = wts)

  out1 <- marginaleffects::avg_comparisons(
    mod,
    variables = "treat",
    wts = "wts",
    vcov = "HC3",
    transform = expm1
  )

  out2 <- estimate_contrasts(
    mod,
    contrast = "treat",
    estimate = "population",
    weights = "wts",
    vcov = "HC3",
    transform = TRUE
  )
  expect_equal(out1$estimate, out2$Difference, tolerance = 1e-4)
})


test_that("estimate_contrast, counterfactual, custom hypothesis", {
  skip_if_not_installed("parameters")
  skip_if_not_installed("glmmTMB")
  skip_if_not_installed("datawizard")
  data(qol_cancer, package = "parameters")

  # sort and group data by patient ID, then assign each patient either to
  # the treatment or control condition, with higher educated patients having
  # a higher chance belonging to the treatment group
  set.seed(12345)

  d <- datawizard::data_arrange(qol_cancer, "ID")
  d <- datawizard::data_group(d, "ID")
  d <- datawizard::data_modify(
    d,
    treatment = rbinom(1, 1, ifelse(education == "high", 0.72, 0.3))
  )
  d <- datawizard::data_ungroup(d)

  # create a treatment effect that increased over time
  # with more improvements for higher educated patients
  d$QoL <- d$QoL +
    rnorm(nrow(d), (d$treatment * d$time * 5) + ifelse(d$education == "high", 5, 0), sd = 2)

  # convert to factors
  d <- datawizard::to_factor(d, c("treatment", "time"))

  m_ipw <- glm(
    treatment ~ time + hospital + phq4 + education + age,
    data = d,
    family = binomial()
  )

  # add predictions, i.e. the probability of belonging to treatment
  # or control for each patient in the sample (propensity score)
  d$propensity_score <- predict(m_ipw, newdata = d, type = "response")

  # calculating the IPW
  d$ipw <- ifelse(
    d$treatment == 1,
    1 / d$propensity_score, # IPW for treatment group
    1 / (1 - d$propensity_score) # IPW for control group
  )

  model <- glmmTMB::glmmTMB(
    QoL ~ treatment * time + education + hospital + age + phq4 + (1 | ID),
    weights = ipw,
    data = d
  )

  out <- estimate_contrasts(
    model,
    "treatment",
    by = "time",
    estimate = "population",
    weights = "ipw",
    comparison = "b2 = b1"
  )

  expect_identical(dim(out), c(1L, 7L))
  expect_identical(out$Parameter, "b2=b1")
  expect_equal(out$Difference, 4.591834, tolerance = 1e-3)
})


test_that("estimate_contrast, counterfactual, snapshots, Level-columns", {
  skip_if_not_installed("parameters")
  skip_if_not_installed("glmmTMB")
  skip_if_not_installed("datawizard")
  data(qol_cancer, package = "parameters")

  # sort and group data by patient ID, then assign each patient either to
  # the treatment or control condition, with higher educated patients having
  # a higher chance belonging to the treatment group
  set.seed(12345)

  d <- datawizard::data_arrange(qol_cancer, "ID")
  d <- datawizard::data_group(d, "ID")
  d <- datawizard::data_modify(
    d,
    treatment = rbinom(1, 1, ifelse(education == "high", 0.72, 0.3))
  )
  d <- datawizard::data_ungroup(d)

  # create a treatment effect that increased over time
  # with more improvements for higher educated patients
  d$QoL <- d$QoL +
    rnorm(nrow(d), (d$treatment * d$time * 5) + ifelse(d$education == "high", 5, 0), sd = 2)

  # convert to factors
  d <- datawizard::to_factor(d, c("treatment", "time"))
  levels(d$time) <- c("pre", "post", "end")
  levels(d$treatment) <- c("control", "tx")

  m_ipw <- glm(
    treatment ~ time + hospital + phq4 + education + age,
    data = d,
    family = binomial()
  )

  # add predictions, i.e. the probability of belonging to treatment
  # or control for each patient in the sample (propensity score)
  d$propensity_score <- predict(m_ipw, newdata = d, type = "response")

  # calculating the IPW
  d$ipw <- ifelse(
    d$treatment == 1,
    1 / d$propensity_score, # IPW for treatment group
    1 / (1 - d$propensity_score) # IPW for control group
  )

  model <- glmmTMB::glmmTMB(
    QoL ~ treatment * time + education + hospital + age + phq4 + (1 | ID),
    weights = ipw,
    data = d
  )

  out <- estimate_contrasts(model, "treatment", estimate = "population", weights = "ipw")
  expect_snapshot(print(out, table_width = Inf))

  out <- estimate_contrasts(
    model,
    "treatment",
    by = "time",
    estimate = "population",
    weights = "ipw"
  )
  expect_snapshot(print(out, table_width = Inf))

  out <- estimate_contrasts(
    model,
    "time",
    by = "treatment",
    estimate = "population",
    weights = "ipw"
  )
  expect_snapshot(print(out, table_width = Inf))
})
