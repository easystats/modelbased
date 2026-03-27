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

  out <- modelbased::estimate_contrasts(
    m,
    c("bill_len_between", "bill_len_within"),
    comparison = "context"
  )
  expect_equal(out$Estimate, b[1] - b[2], tolerance = 1e-4, ignore_attr = TRUE)
  expect_equal(out$SE, sqrt((se[1]^2 + se[2]^2)), tolerance = 1e-4, ignore_attr = TRUE)
})

test_that("estimate_contrast, context effects, glm", {
  data(penguins, package = "datasets")
  d <- datawizard::demean(penguins, "bill_len", by = "species")
  d$out <- datawizard::categorize(d$flipper_len) - 1
  m <- glm(out ~ bill_len_between + bill_len_within, data = d, family = binomial())

  b <- coef(summary(m))[2:3, 1]
  se <- coef(summary(m))[2:3, 2]

  out <- modelbased::estimate_contrasts(
    m,
    c("bill_len_between", "bill_len_within"),
    comparison = "context"
  )
  expect_equal(out$Estimate, exp(b[1] - b[2]), tolerance = 1e-4, ignore_attr = TRUE)
})
