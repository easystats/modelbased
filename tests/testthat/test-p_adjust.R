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
  if (all(c("Level1", "Level2") %in% colnames(out))) {
    key_out <- vapply(
      seq_len(nrow(out)),
      function(i) paste(sort(c(as.character(out$Level1[i]), as.character(out$Level2[i]))), collapse = "|"),
      ""
    )
  } else {
    # compared slopes, labelled like "(b) - (a)"
    pairs_out <- strsplit(as.character(out$Parameter), " - ", fixed = TRUE)
    key_out <- vapply(pairs_out, function(i) paste(sort(gsub("[()]", "", i)), collapse = "|"), "")
  }
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


test_that("p_adjust tukey and sidak, compared slopes, marginaleffects", {
  em <- emmeans::emtrends(m2, "g", var = "x")
  for (p_adjust in c("tukey", "sidak")) {
    em_pairs <- pairs(em, adjust = p_adjust)
    expect_no_message(
      {
        out <- estimate_slopes(m2, "x", by = "g", comparison = ~pairwise, p_adjust = p_adjust)
      },
      message = "Sidak"
    )
    expect_equal(out$p, emmeans_p(out, em_pairs), tolerance = 1e-5)
    expect_no_message(
      {
        out <- estimate_contrasts(m2, contrast = "x", by = "g", p_adjust = p_adjust)
      },
      message = "Sidak"
    )
    expect_equal(out$p, emmeans_p(out, em_pairs), tolerance = 1e-5)
  }

  # post-processing: unadjusted p-values are based on z, so we only check the
  # number of slopes in the Tukey adjustment, which uses the model's df
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
  expect_identical(attributes(out)$p_adjust, "sidak")

  # partly matched `hypothesis` argument
  em_pairs <- pairs(emmeans::emtrends(m2, "g", var = "x"), adjust = "tukey")
  expect_no_message(
    {
      out <- estimate_slopes(m2, "x", by = "g", hypo = ~pairwise, p_adjust = "tukey")
    },
    message = "Sidak"
  )
  expect_equal(out$p, emmeans_p(out, em_pairs), tolerance = 1e-5)
})


test_that("p_adjust, slopes, emmeans backend", {
  mt <- mtcars
  mt$cyl <- factor(mt$cyl)
  m4 <- lm(mpg ~ wt * cyl, data = mt)
  em <- emmeans::emtrends(m4, "cyl", var = "wt")
  for (p_adjust in c("holm", "bonferroni", "sidak", "tukey", "none")) {
    out <- estimate_slopes(m4, "wt", by = "cyl", backend = "emmeans", p_adjust = p_adjust)
    em_p <- suppressMessages(as.data.frame(summary(emmeans::test(em, adjust = p_adjust)))$p.value)
    expect_equal(out$p, em_p, tolerance = 1e-5)
  }
  p_none <- estimate_slopes(m4, "wt", by = "cyl", backend = "emmeans", p_adjust = "none")$p
  p_holm <- estimate_slopes(m4, "wt", by = "cyl", backend = "emmeans", p_adjust = "holm")$p
  expect_false(isTRUE(all.equal(p_none, p_holm)))
})


test_that("p_adjust tukey and sidak, slopes compared pairwise within groups", {
  m3 <- lm(y ~ x * g * h, data = d_adjust)
  em <- emmeans::emtrends(m3, ~ g | h, var = "x")
  for (p_adjust in c("tukey", "sidak")) {
    expect_no_message(
      {
        out <- estimate_slopes(
          m3,
          "x",
          by = c("g", "h"),
          comparison = ~ pairwise | h,
          p_adjust = p_adjust
        )
      },
      message = "Sidak"
    )
    expect_equal(out$p, emmeans_p(out, pairs(em, adjust = p_adjust), by = "h"), tolerance = 1e-5)
  }
})


test_that("p_adjust, families of grouped contrasts and of contrasts of predictions", {
  # contrasts of slopes with a group term, Tukey counts the slopes in each group
  m3 <- lm(y ~ x * g * h, data = d_adjust)
  out <- estimate_contrasts(
    m3,
    contrast = "x",
    by = c("g", "h"),
    comparison = ~ pairwise | h,
    p_adjust = "tukey"
  )
  em <- emmeans::emtrends(m3, "g", by = "h", var = "x")
  expect_equal(out$p, emmeans_p(out, pairs(em, adjust = "tukey"), by = "h"), tolerance = 1e-5)

  # contrasts of `estimate_relation()` output, Sidak counts within `by` groups
  out <- estimate_contrasts(
    estimate_relation(m1, by = c("g", "h")),
    contrast = "g",
    by = "h",
    p_adjust = "sidak"
  )
  em <- suppressMessages(emmeans::emmeans(m1, "g", by = "h"))
  expect_equal(out$p, emmeans_p(out, pairs(em, adjust = "sidak"), by = "h"), tolerance = 1e-5)

  # missing values of a `by` variable form their own family
  expect_identical(
    modelbased:::.p_adjust_family_size(data.frame(h = c("a", NA, "a", "b")), by = "h"),
    c(2L, 1L, 2L, 1L)
  )
})


test_that("p_adjust, no Sidak message when nothing is adjusted, and method names", {
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

  # emmeans backend: method names in any case, and the `p_adjust` attribute
  mt <- mtcars
  mt$cyl <- factor(mt$cyl)
  m4 <- lm(mpg ~ wt * cyl, data = mt)
  em <- emmeans::emtrends(m4, "cyl", var = "wt")
  out <- estimate_slopes(m4, "wt", by = "cyl", backend = "emmeans", p_adjust = "bh")
  expect_equal(
    out$p,
    as.data.frame(summary(emmeans::test(em, adjust = "BH")))$p.value,
    tolerance = 1e-5
  )
  expect_identical(attributes(out)$p_adjust, "BH")
  out <- suppressMessages(estimate_slopes(
    m4,
    "wt",
    by = "cyl",
    backend = "emmeans",
    p_adjust = "tukey"
  ))
  expect_identical(attributes(out)$p_adjust, "sidak")
})
