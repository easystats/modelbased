skip_on_cran()

test_that("estimate_means - print footer for estimate types and re.form", {
  skip_if_not_installed("lme4")
  skip_if_not_installed("marginaleffects")
  skip_if(getRversion() < "4.5.0")

  data(penguins)

  # one categorical
  model <- lm(body_mass ~ species + sex + bill_len, data = penguins)
  expect_snapshot(print(estimate_means(model, "sex", estimate = "specific")))
  expect_snapshot(print(estimate_means(model, "sex", estimate = "typical")))
  expect_snapshot(print(estimate_means(model, "sex", estimate = "average")))
  expect_snapshot(print(estimate_means(model, "sex", estimate = "population")))

  # warnings
  expect_no_warning(estimate_means(model, "sex", estimate = "average"))
  expect_no_warning(estimate_means(model, "sex", estimate = "population"))

  # one categorical, one numeric
  model <- lm(body_mass ~ species + bill_dep * sex + bill_len, data = penguins)
  expect_snapshot(print(estimate_means(
    model,
    c("sex", "bill_dep"),
    estimate = "specific"
  )))
  expect_snapshot(print(estimate_means(
    model,
    c("sex", "bill_dep"),
    estimate = "typical"
  )))
  expect_snapshot(print(estimate_means(
    model,
    c("sex", "bill_dep"),
    estimate = "average"
  )))
  expect_snapshot(print(estimate_means(
    model,
    c("sex", "bill_dep"),
    estimate = "population"
  )))

  # check warnings
  expect_warning(estimate_means(model, c("sex", "bill_dep"), estimate = "average"))
  expect_no_warning(estimate_means(
    model,
    c("sex", "bill_dep"),
    estimate = "average",
    verbose = FALSE
  ))
  # warnings
  expect_no_warning(estimate_means(model, c("sex", "bill_dep"), estimate = "population"))

  # one numeric
  model <- lm(body_mass ~ species + bill_dep + sex + bill_len, data = penguins)
  expect_snapshot(print(estimate_means(model, "bill_dep", estimate = "specific")))
  expect_snapshot(print(estimate_means(model, "bill_dep", estimate = "typical")))
  expect_snapshot(print(estimate_means(
    model,
    "bill_dep",
    estimate = "average",
    verbose = FALSE
  )))
  expect_snapshot(print(estimate_means(model, "bill_dep", estimate = "population")))

  # check warnings
  expect_warning(estimate_means(model, "bill_dep", estimate = "average"))
  expect_no_warning(estimate_means(
    model,
    "bill_dep",
    estimate = "average",
    verbose = FALSE
  ))

  # two categorical
  model <- lm(body_mass ~ species * sex + bill_dep + bill_len, data = penguins)
  expect_snapshot(print(estimate_means(
    model,
    c("sex", "species"),
    estimate = "specific"
  )))
  expect_snapshot(print(estimate_means(model, c("sex", "species"), estimate = "typical")))
  expect_snapshot(print(estimate_means(model, c("sex", "species"), estimate = "average")))
  expect_snapshot(print(estimate_means(
    model,
    c("sex", "species"),
    estimate = "population"
  )))

  # check warnings
  expect_no_warning(estimate_means(model, c("sex", "species"), estimate = "average"))

  # mixed models
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

  # inverse link
  skip_if_not_installed("datawizard")
  data(penguins)
  penguins <- datawizard::categorize(penguins, "bill_dep")
  m <- glm(
    as.factor(bill_dep) ~ sex + species + body_mass,
    data = penguins,
    family = binomial()
  )

  # default is predict = "inverse_link"
  expect_snapshot(print(estimate_means(m, "sex")))

  # default is predict = "response"
  expect_snapshot(print(estimate_means(m, "sex", estimate = "average")))
})
