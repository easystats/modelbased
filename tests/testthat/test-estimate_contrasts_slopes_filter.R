skip_if_not_installed("emmeans")
skip_if_not_installed("marginaleffects", minimum_version = "0.29.0")

mt <- mtcars
mt$g <- factor(mt$cyl)
mt$a <- factor(mt$am)
m2 <- lm(mpg ~ wt * g, data = mt)
m3 <- lm(mpg ~ wt * g * a, data = mt)

# modelbased orders and orients the pairs differently than emmeans
expect_slope_contrasts <- function(out, at) {
  ref <- as.data.frame(pairs(
    emmeans::emtrends(m3, ~ g * a, "wt", at = at),
    adjust = "none"
  ))
  expect_equal(sort(abs(out$Difference)), sort(abs(ref$estimate)), tolerance = 1e-5)
  expect_equal(sort(out$p), sort(ref$p.value), tolerance = 1e-5)
}


test_that("estimate_contrasts, slopes, by-filters", {
  by <- c("g='4'", "a")
  expect_slope_contrasts(
    estimate_contrasts(m3, "wt", by = by, estimate = "average"),
    list(g = "4")
  )
  expect_slope_contrasts(
    estimate_contrasts(m3, "wt", by = by, backend = "emmeans"),
    list(g = "4")
  )

  by <- c("g=c('4','6')", "a='1'")
  at <- list(g = c("4", "6"), a = "1")
  expect_slope_contrasts(estimate_contrasts(m3, "wt", by = by, estimate = "average"), at)
  expect_slope_contrasts(estimate_contrasts(m3, "wt", by = by, backend = "emmeans"), at)

  by <- c("g=c('4','5')", "a")
  expect_slope_contrasts(
    estimate_contrasts(m3, "wt", by = by, estimate = "average"),
    list(g = "4")
  )
  expect_slope_contrasts(
    estimate_contrasts(m3, "wt", by = by, backend = "emmeans"),
    list(g = "4")
  )
})


test_that("estimate_contrasts, slopes, by-filters leave fewer than two slopes", {
  expect_error(
    estimate_contrasts(m2, "wt", by = "g='4'", estimate = "average"),
    regexp = "at least two"
  )
  expect_error(
    estimate_contrasts(m2, "wt", by = "g='4'", backend = "emmeans"),
    regexp = "at least two"
  )
  # spaces around "=", and `estimate = "specific"`
  expect_error(
    estimate_contrasts(m3, "wt", by = c("g = '4'", "a = '1'"), estimate = "specific"),
    regexp = "at least two"
  )
  # with `estimate = "average"`, only slopes with data count: for 5 gears,
  # all cars have am = 1
  mt$gear <- factor(mt$gear)
  m7 <- lm(mpg ~ wt * a + gear, data = mt)
  expect_error(
    estimate_contrasts(m7, "wt", by = c("gear='5'", "a"), estimate = "average"),
    regexp = "at least two"
  )
})


test_that("estimate_contrasts, slopes, emmeans, numeric by or factors from the formula", {
  m_factor <- lm(mpg ~ wt * factor(cyl), data = mtcars)
  out <- estimate_contrasts(m_factor, "wt", by = "cyl=c(4,6)", backend = "emmeans")
  expect_identical(nrow(out), 3L)
  m_numeric <- lm(mpg ~ wt * hp * g, data = mt)
  out <- estimate_contrasts(m_numeric, "wt", by = c("g='4'", "hp"), backend = "emmeans")
  expect_identical(nrow(out), 3L)
})
