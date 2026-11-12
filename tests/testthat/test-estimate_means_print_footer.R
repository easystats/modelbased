skip_on_cran()

test_that("estimate_means - print footer for estimate types and re.form", {
  skip_if_not_installed("lme4")
  skip_if_not_installed("marginaleffects")
  skip_if(getRversion() < "4.5.0")

  data(penguins)
  set.seed(123)
  d <- penguins
  d$weights <- abs(rnorm(nrow(d), 1, 0.2))
  model <- lm(body_mass ~ species + sex + bill_len, data = d, weights = weights)

  expect_snapshot(print(estimate_means(model, "sex", estimate = "specific")))
  expect_snapshot(print(estimate_means(model, "sex", estimate = "typical")))
  expect_snapshot(print(estimate_means(model, "sex", estimate = "average")))
  expect_snapshot(print(estimate_means(model, "sex", estimate = "population")))

  data(cbpp, package = "lme4")
  m <- lme4::glmer(
    cbind(incidence, size - incidence) ~ period + (1 | herd),
    data = cbpp,
    family = binomial
  )

  expect_snapshot(print(estimate_means(m, by = "period", estimate = "typical")))
  expect_snapshot(print(estimate_means(
    m,
    by = "period",
    estimate = "typical",
    re.form = NA
  )))
  expect_snapshot(print(estimate_means(m, by = "period", estimate = "average")))
  expect_snapshot(print(estimate_means(
    m,
    by = "period",
    estimate = "average",
    re.form = NA
  )))
})
