skip_on_cran()
skip_if_not_installed("marginaleffects", minimum_version = "0.29.0")
skip_on_os("mac")
skip_if(getRversion() < "4.5.0")
skip_if_not_installed("datawizard")

test_that("estimate_contrast, context effects, linear", {
  data(penguins, package = "datasets")
  d <- datawizard::demean(penguins, "bill_len", by = "species")
  m <- lm(bill_dep ~ bill_len_between + bill_len_within, data = d)

  b <- coef(summary(m))[2:3, 1]
  se <- coef(summary(m))[2:3, 2]

  out <- estimate_contrasts(m, c("bill_len_between", "bill_len_within"))
  expect_equal(out$Difference, b[2] - b[1], tolerance = 1e-4, ignore_attr = TRUE)
  expect_equal(out$SE, sqrt((se[1]^2 + se[2]^2)), tolerance = 1e-4, ignore_attr = TRUE)
  expect_true(!is.null(out$p))

  output <- capture.output(out)
  expect_identical(output[3], "Difference |   SE |       95% CI |     z |      p")

  expect_message(
    estimate_contrasts(
      m,
      c("bill_len_between", "bill_len_within"),
      comparison = "difference"
    ),
    regex = "The `comparison` argument will be set"
  )

  m <- lm(bill_dep ~ year * (bill_len_between + bill_len_within), data = d)
  out <- estimate_contrasts(m, c("bill_len_between", "bill_len_within"), by = "year")
  expect_named(out, c("year", "Difference", "SE", "CI_low", "CI_high", "z", "p"))
  x1 <- estimate_slopes(m, "bill_len_within", by = "year")
  x2 <- estimate_slopes(m, "bill_len_between", by = "year")
  expect_equal(out$Difference, x1$Slope - x2$Slope, tolerance = 1e-4)

  out <- estimate_contrasts(m, c("bill_len_between", "bill_len_within", "year"))
  expect_named(
    out,
    c("Level1", "Level2", "Parameter", "Difference", "SE", "CI_low", "CI_high", "z", "p")
  )
  expect_equal(
    out$Difference,
    c(-0.04317, -0.08635, -0.04317),
    tolerance = 1e-4,
    ignore_attr = TRUE
  )

  skip_on_os(c("mac", "linux"))
  m <- lm(bill_dep ~ sex * year * (bill_len_between + bill_len_within), data = d)
  out <- estimate_contrasts(
    m,
    c("bill_len_between", "bill_len_within"),
    by = c("sex", "year")
  )
  expect_identical(
    capture.output(out),
    c(
      "Marginal Contrasts Analysis",
      "",
      "sex    | year | Difference |   SE |        95% CI |    z |      p",
      "-----------------------------------------------------------------",
      "female | 2007 |       0.28 | 0.08 | [ 0.12, 0.45] | 3.41 | < .001",
      "female | 2008 |       0.26 | 0.06 | [ 0.15, 0.37] | 4.47 | < .001",
      "female | 2009 |       0.24 | 0.10 | [ 0.05, 0.42] | 2.48 |  0.013",
      "male   | 2007 |       0.46 | 0.10 | [ 0.26, 0.65] | 4.60 | < .001",
      "male   | 2008 |       0.28 | 0.06 | [ 0.15, 0.40] | 4.41 | < .001",
      "male   | 2009 |       0.10 | 0.10 | [-0.11, 0.30] | 0.93 |  0.355",
      "",
      "Variable predicted: bill_dep",
      "Predictors contrasted: bill_len_between, bill_len_within",
      "p-values are uncorrected."
    )
  )
  out <- estimate_contrasts(
    m,
    c("bill_len_between", "bill_len_within", "sex"),
    by = "year"
  )
  expect_identical(
    capture.output(out),
    c(
      "Marginal Contrasts Analysis",
      "",
      "Level1 | Level2 | year | Difference |   SE |        95% CI |     z |     p",
      "--------------------------------------------------------------------------",
      "male   | female | 2007 |       0.17 | 0.13 | [-0.08, 0.43] |  1.33 | 0.183",
      "male   | female | 2008 |       0.02 | 0.09 | [-0.15, 0.18] |  0.18 | 0.853",
      "male   | female | 2009 |      -0.14 | 0.14 | [-0.42, 0.14] | -0.99 | 0.320",
      "",
      "Variable predicted: bill_dep",
      "Predictors contrasted: bill_len_between, bill_len_within",
      "p-values are uncorrected."
    )
  )
})

test_that("estimate_contrast, context effects, glm", {
  data(penguins, package = "datasets")
  d <- datawizard::demean(penguins, "bill_len", by = "species")
  d$out <- datawizard::categorize(d$flipper_len) - 1
  m <- glm(out ~ bill_len_between + bill_len_within, data = d, family = binomial())

  b <- coef(summary(m))[2:3, 1]
  se <- coef(summary(m))[2:3, 2]

  out <- estimate_contrasts(m, c("bill_len_between", "bill_len_within"))
  expect_equal(out$Odds_Ratio, exp(b[2] - b[1]), tolerance = 1e-4, ignore_attr = TRUE)
  expect_true(!is.null(out$p))

  output <- capture.output(out)
  expect_identical(output[3], "Odds_Ratio |       95% CI |     p")

  out <- estimate_contrasts(
    m,
    c("bill_len_between", "bill_len_within"),
    predict = "response"
  )
  expect_equal(out$Probability, -0.01784138, tolerance = 1e-4, ignore_attr = TRUE)
})
