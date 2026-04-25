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

  out <- estimate_contrasts(
    m,
    c("bill_len_between", "bill_len_within"),
    comparison = "context"
  )
  expect_equal(out$Difference, b[2] - b[1], tolerance = 1e-4, ignore_attr = TRUE)
  expect_equal(out$SE, sqrt((se[1]^2 + se[2]^2)), tolerance = 1e-4, ignore_attr = TRUE)
  expect_true(!is.null(out$p))

  output <- capture.output(out)
  expect_identical(output[3], "Difference |   SE |       95% CI |     z |      p")

  out2 <- estimate_contrasts(m, c("bill_len_between", "bill_len_within"))
  expect_equal(out, out2, ignore_attr = TRUE, tolerance = 1e-4)

  m <- lm(bill_dep ~ year * (bill_len_between + bill_len_within), data = d)
  out <- estimate_contrasts(
    m,
    c("bill_len_between", "bill_len_within"),
    by = "year",
    comparison = "context"
  )
  expect_named(out, c("year", "Difference", "SE", "CI_low", "CI_high", "z", "p"))
  x1 <- estimate_slopes(m, "bill_len_within", by = "year")
  x2 <- estimate_slopes(m, "bill_len_between", by = "year")
  expect_equal(out$Difference, x1$Slope - x2$Slope, tolerance = 1e-4)

  out2 <- estimate_contrasts(m, c("bill_len_between", "bill_len_within"), by = "year")
  expect_equal(out, out2, ignore_attr = TRUE, tolerance = 1e-4)

  out <- estimate_contrasts(
    m,
    c("bill_len_between", "bill_len_within"),
    by = "year",
    comparison = "context_pairwise"
  )
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

  out2 <- estimate_contrasts(m, c("bill_len_between", "bill_len_within", "year"))
  expect_equal(out, out2, ignore_attr = TRUE, tolerance = 1e-4)
})

test_that("estimate_contrast, context effects, glm", {
  data(penguins, package = "datasets")
  d <- datawizard::demean(penguins, "bill_len", by = "species")
  d$out <- datawizard::categorize(d$flipper_len) - 1
  m <- glm(out ~ bill_len_between + bill_len_within, data = d, family = binomial())

  b <- coef(summary(m))[2:3, 1]
  se <- coef(summary(m))[2:3, 2]

  out <- estimate_contrasts(
    m,
    c("bill_len_between", "bill_len_within"),
    comparison = "context"
  )
  expect_equal(out$Odds_Ratio, exp(b[2] - b[1]), tolerance = 1e-4, ignore_attr = TRUE)
  expect_true(!is.null(out$p))

  output <- capture.output(out)
  expect_identical(output[3], "Odds_Ratio |       95% CI |     p")

  out <- estimate_contrasts(
    m,
    c("bill_len_between", "bill_len_within"),
    comparison = "slope"
  )
  expect_equal(out$Odds_Ratio, exp(b[2] - b[1]), tolerance = 1e-4, ignore_attr = TRUE)
  expect_true(!is.null(out$p))

  out <- estimate_contrasts(
    m,
    c("bill_len_between", "bill_len_within"),
    comparison = "context",
    predict = "response"
  )
  expect_equal(out$Probability, -0.01784138, tolerance = 1e-4, ignore_attr = TRUE)
})
