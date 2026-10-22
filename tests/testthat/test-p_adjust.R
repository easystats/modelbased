skip_on_cran()
skip_if_not_installed("emmeans")
skip_if_not_installed("marginaleffects")

set.seed(12)
n <- 200
d_adjust <- data.frame(
  g = factor(rep(letters[1:4], each = n / 4)),
  h = factor(rep(c("u", "v"), n / 2)),
  x = rnorm(n)
)
d_adjust$y <- 1 +
  as.integer(d_adjust$g) * 0.3 * d_adjust$x +
  0.4 * as.integer(d_adjust$g) +
  rnorm(n)
m1 <- lm(y ~ g * h, data = d_adjust)
m2 <- lm(y ~ x * g, data = d_adjust)

# p-values of emmeans; modelbased orders and orients the pairs differently
emmeans_p <- function(em) sort(as.data.frame(summary(em))$p.value)

# the message can wrap over lines
sidak_message <- "(?s)(?=.*Tukey)(?=.*Sidak)"


test_that("p_adjust tukey and sidak, contrasts, marginaleffects", {
  em <- suppressMessages(emmeans::emmeans(m1, "g"))
  out <- estimate_contrasts(m1, "g", p_adjust = "tukey")
  expect_equal(sort(out$p), emmeans_p(pairs(em, adjust = "tukey")), tolerance = 1e-5)
  out <- estimate_contrasts(m1, "g", p_adjust = "sidak")
  expect_equal(sort(out$p), emmeans_p(pairs(em, adjust = "sidak")), tolerance = 1e-5)

  em <- suppressMessages(emmeans::emmeans(m1, "g", by = "h"))
  out <- estimate_contrasts(m1, "g", by = "h", p_adjust = "tukey")
  expect_equal(sort(out$p), emmeans_p(pairs(em, adjust = "tukey")), tolerance = 1e-5)

  expect_message(
    {
      out <- estimate_contrasts(m1, "g", comparison = "reference", p_adjust = "tukey")
    },
    regexp = sidak_message,
    perl = TRUE
  )
  em <- suppressMessages(emmeans::emmeans(m1, "g"))
  em_p <- as.data.frame(summary(emmeans::contrast(
    em,
    "trt.vs.ctrl",
    adjust = "sidak"
  )))$p.value
  expect_equal(out$p, em_p, tolerance = 1e-5)
  expect_identical(attributes(out)$p_adjust, "sidak")
})


test_that("p_adjust tukey, compared slopes, marginaleffects", {
  em_p <- emmeans_p(pairs(emmeans::emtrends(m2, "g", var = "x"), adjust = "tukey"))
  out <- estimate_slopes(m2, "x", by = "g", comparison = ~pairwise, p_adjust = "tukey")
  expect_equal(sort(out$p), em_p, tolerance = 1e-5)
  out <- estimate_contrasts(m2, contrast = "x", by = "g", p_adjust = "tukey")
  expect_equal(sort(out$p), em_p, tolerance = 1e-5)
  out <- estimate_slopes(m2, "x", by = "g", hypo = ~pairwise, p_adjust = "tukey")
  expect_equal(sort(out$p), em_p, tolerance = 1e-5)

  # unadjusted p-values are based on z, so we only check the number of slopes
  out <- suppressMessages(estimate_slopes(
    m2,
    "x",
    by = "g",
    post_process = ~pairwise,
    p_adjust = "tukey"
  ))
  z <- intersect(c("z", "t", "statistic"), colnames(out))[1]
  expect_equal(
    as.vector(out$p),
    stats::ptukey(sqrt(2) * abs(as.vector(out[[z]])), 4, stats::df.residual(m2), lower.tail = FALSE),
    tolerance = 1e-8
  )

  m3 <- lm(y ~ x * g * h, data = d_adjust)
  em_p <- emmeans_p(pairs(emmeans::emtrends(m3, ~ g | h, var = "x"), adjust = "tukey"))
  out <- estimate_slopes(
    m3,
    "x",
    by = c("g", "h"),
    comparison = ~ pairwise | h,
    p_adjust = "tukey"
  )
  expect_equal(sort(out$p), em_p, tolerance = 1e-5)
  out <- estimate_contrasts(
    m3,
    contrast = "x",
    by = c("g", "h"),
    comparison = ~ pairwise | h,
    p_adjust = "tukey"
  )
  expect_equal(sort(out$p), em_p, tolerance = 1e-5)
})


test_that("p_adjust tukey, slopes tested against a value, marginaleffects", {
  expect_message(
    {
      out <- estimate_slopes(m2, "x", by = "g", hypothesis = 1, p_adjust = "tukey")
    },
    regexp = sidak_message,
    perl = TRUE
  )
  em_p <- as.data.frame(summary(emmeans::test(
    emmeans::emtrends(m2, "g", var = "x"),
    null = 1,
    adjust = "sidak"
  )))$p.value
  expect_equal(out$p, em_p, tolerance = 1e-5)
})


test_that("p_adjust, contrasts of estimate_relation() output", {
  out <- estimate_contrasts(
    estimate_relation(m1, by = c("g", "h")),
    contrast = "g",
    by = "h",
    p_adjust = "sidak"
  )
  em <- suppressMessages(emmeans::emmeans(m1, "g", by = "h"))
  expect_equal(sort(out$p), emmeans_p(pairs(em, adjust = "sidak")), tolerance = 1e-5)

  # missing values of a `by` variable form their own family
  expect_identical(
    modelbased:::.p_adjust_family_size(data.frame(h = c("a", NA, "a", "b")), by = "h"),
    c(2L, 1L, 2L, 1L)
  )
})


test_that("p_adjust, no Sidak message when nothing is adjusted", {
  # one contrast or one slope: Sidak does not change the p-value
  expect_no_message(
    estimate_contrasts(m1, "g", comparison = "b1 = b2", p_adjust = "tukey"),
    message = "Sidak"
  )
  expect_no_message(
    estimate_slopes(lm(mpg ~ wt, data = mtcars), "wt", hypothesis = 1, p_adjust = "tukey"),
    message = "Sidak"
  )
  # a matrix hypothesis is not a test against a value
  expect_message(
    estimate_slopes(
      m2,
      "x",
      by = "g",
      hypothesis = matrix(c(-1, 1, 0, 0, 0, -1, 1, 0), 4),
      p_adjust = "tukey"
    ),
    regexp = "not compared pairwise"
  )
})


test_that("p_adjust, slopes, emmeans backend", {
  mt <- mtcars
  mt$cyl <- factor(mt$cyl)
  m4 <- lm(mpg ~ wt * cyl, data = mt)
  em <- emmeans::emtrends(m4, "cyl", var = "wt")
  out <- suppressMessages(estimate_slopes(
    m4,
    "wt",
    by = "cyl",
    backend = "emmeans",
    p_adjust = "tukey"
  ))
  em_p <- suppressMessages(
    as.data.frame(summary(emmeans::test(em, adjust = "tukey")))$p.value
  )
  expect_equal(out$p, em_p, tolerance = 1e-5)
  expect_identical(attributes(out)$p_adjust, "sidak")

  out <- estimate_slopes(m4, "wt", by = "cyl", backend = "emmeans", p_adjust = "bh")
  em_p <- as.data.frame(summary(emmeans::test(em, adjust = "BH")))$p.value
  expect_equal(out$p, em_p, tolerance = 1e-5)
  expect_identical(attributes(out)$p_adjust, "BH")
})
