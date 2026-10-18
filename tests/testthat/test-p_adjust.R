skip_on_cran()
skip_if_not_installed("emmeans")
skip_if_not_installed("marginaleffects")

# data and models for the p-value adjustment tests
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

# p-values of emmeans, in the row order of modelbased. modelbased compares
# "b - a" where emmeans compares "a - b", so rows are matched by the sorted
# pair of levels, and by the levels of `by`, if any
emmeans_p <- function(out, em, by = NULL) {
  em <- as.data.frame(summary(em))
  pairs_em <- strsplit(as.character(em$contrast), " - ", fixed = TRUE)
  key_em <- vapply(pairs_em, function(i) paste(sort(gsub("[()]", "", i)), collapse = "|"), "")
  key_out <- vapply(
    seq_len(nrow(out)),
    function(i) paste(sort(c(as.character(out$Level1[i]), as.character(out$Level2[i]))), collapse = "|"),
    ""
  )
  if (!is.null(by)) {
    key_em <- paste(key_em, em[[by]])
    key_out <- paste(key_out, out[[by]])
  }
  em$p.value[match(key_out, key_em)]
}

# the message can wrap over lines
sidak_message <- "(?s)(?=.*Tukey)(?=.*Sidak)"


test_that("p_adjust tukey, pairwise contrasts, marginaleffects", {
  out <- estimate_contrasts(m1, "g", p_adjust = "tukey")
  em <- suppressMessages(emmeans::emmeans(m1, "g"))
  expect_equal(out$p, emmeans_p(out, pairs(em, adjust = "tukey")), tolerance = 1e-5)
  expect_identical(attributes(out)$p_adjust, "tukey")

  out <- estimate_contrasts(m1, "g", by = "h", p_adjust = "tukey")
  em <- suppressMessages(emmeans::emmeans(m1, "g", by = "h"))
  expect_equal(out$p, emmeans_p(out, pairs(em, adjust = "tukey"), by = "h"), tolerance = 1e-5)
})


test_that("p_adjust tukey, non-pairwise contrasts use sidak, marginaleffects", {
  em <- suppressMessages(emmeans::emmeans(m1, "g"))
  comparisons <- list("reference", ~reference, "sequential", "meandev")
  methods <- c("trt.vs.ctrl", "trt.vs.ctrl", "consec", "eff")
  for (i in seq_along(comparisons)) {
    expect_message(
      {
        out <- estimate_contrasts(
          m1,
          "g",
          comparison = comparisons[[i]],
          p_adjust = "tukey"
        )
      },
      regexp = sidak_message,
      perl = TRUE
    )
    em_p <- as.data.frame(summary(emmeans::contrast(em, methods[i], adjust = "sidak")))$p.value
    expect_equal(out$p, em_p, tolerance = 1e-5)
    expect_identical(attributes(out)$p_adjust, "sidak")
    expect_no_message(
      estimate_contrasts(
        m1,
        "g",
        comparison = comparisons[[i]],
        p_adjust = "tukey",
        verbose = FALSE
      ),
      message = "Tukey|Sidak"
    )
  }

  # the example of the issue
  mt <- mtcars
  mt$gear <- factor(mt$gear)
  m5 <- lm(mpg ~ gear, data = mt)
  out <- suppressMessages(estimate_contrasts(
    m5,
    "gear",
    comparison = "reference",
    p_adjust = "tukey"
  ))
  em_p <- as.data.frame(summary(emmeans::contrast(
    emmeans::emmeans(m5, "gear"),
    "trt.vs.ctrl",
    adjust = "sidak"
  )))$p.value
  expect_equal(out$p, em_p, tolerance = 1e-5)
})


test_that("p_adjust sidak, contrasts, family size, marginaleffects", {
  out <- estimate_contrasts(m1, "g", p_adjust = "sidak")
  em <- suppressMessages(emmeans::emmeans(m1, "g"))
  expect_equal(out$p, emmeans_p(out, pairs(em, adjust = "sidak")), tolerance = 1e-5)

  out <- estimate_contrasts(m1, "g", by = "h", p_adjust = "sidak")
  em <- suppressMessages(emmeans::emmeans(m1, "g", by = "h"))
  expect_equal(out$p, emmeans_p(out, pairs(em, adjust = "sidak"), by = "h"), tolerance = 1e-5)

  em <- suppressMessages(emmeans::emmeans(m1, "g"))
  comparisons <- list("reference", ~reference, "sequential", "meandev")
  methods <- c("trt.vs.ctrl", "trt.vs.ctrl", "consec", "eff")
  for (i in seq_along(comparisons)) {
    expect_no_message(
      {
        out <- estimate_contrasts(
          m1,
          "g",
          comparison = comparisons[[i]],
          p_adjust = "sidak"
        )
      },
      message = "Sidak"
    )
    em_p <- as.data.frame(summary(emmeans::contrast(em, methods[i], adjust = "sidak")))$p.value
    expect_equal(out$p, em_p, tolerance = 1e-5)
  }
})
