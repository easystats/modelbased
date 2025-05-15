skip_on_cran()
skip_if_not_installed("marginaleffects")
skip_on_os("mac")

test_that("estimate_contrasts - book examples 1", {
  data(contrast_example, package = "modelbased")

  # set contrasts.
  treat_vs_none <- c(-2 / 3, 1 / 3, 1 / 3)
  short_vs_long <- c(0, -1 / 2, 1 / 2)

  contrast_example$tx_ori <- contrast_example$tx
  contrasts(contrast_example$tx) <- cbind(treat_vs_none, short_vs_long)

  contrast_example$treat_vs_none <- as.factor(
    ifelse(contrast_example$tx == "no treatment", "no treatment", "Puppies")
  )

  contrast_example$short_vs_long <- factor(
    short_vs_long,
    levels = c("short", "long", "no treatmen")
  )

  contrast_example$score <- contrast_example$score - mean(contrast_example$score)

  # fit model
  m1 <- lm(outcome ~ score + tx + tx:score, data = contrast_example)
  m2 <- lm(outcome ~ score * tx_ori, data = contrast_example)

  expect_equal(
    coef(m1)["txtreat_vs_none"],
    estimate_contrasts(m2, "tx_ori", comparison = "((b2+b3)/2) = b1")$Difference,
    tolerance = 1e-4,
    ignore_attr = TRUE
  )
  expect_equal(
    coef(m1)["txshort_vs_long"],
    estimate_contrasts(m2, "tx_ori", comparison = "b3 = b2")$Difference,
    tolerance = 1e-4,
    ignore_attr = TRUE
  )
})


test_that("estimate_contrasts - book examples 2", {
  data(contrast_example, package = "modelbased")
  cond_tx <- cbind("no treatment" = c(1, 0, 0), "treatment" = c(0, 0.5, 0.5))

  m1 <- lm(outcome ~ score * tx, data = contrast_example)

  out1 <- marginaleffects::avg_slopes(m1, variables = "score", by = "tx", hypothesis = cond_tx)
  out2 <- estimate_slopes(m1, "score", by = "tx", hypothesis = cond_tx)
  expect_equal(out1$estimate, out2$Slope, tolerance = 1e-4)
  # we donb't officially have this argument for slopes, but we simply pass
  # it to the "hypothesis"
  out3 <- estimate_slopes(m1, "score", by = "tx", comparison = cond_tx)
  expect_equal(out3$Slope, out2$Slope, tolerance = 1e-4)
})


skip_if_not_installed("withr")

withr::with_environment(
  new.env(),
  test_that("estimate_contrasts - book examples 3", {
    data(contrast_example, package = "modelbased")
    cond_tx_foo <<- function(x) {
      drop(x %*% cbind("no treatment" = c(1, 0, 0), "treatment" = c(0, 0.5, 0.5)))
    }
    m1 <- lm(outcome ~ score * tx, data = contrast_example)

    out1 <- marginaleffects::avg_predictions(
      m1,
      variables = c("score", "tx"),
      hypothesis = ~ I(cond_tx_foo(x)) | score
    )

    out2 <- estimate_contrasts(
      m1,
      c("score=c(0, 1, 2.5, 4, 7)"),
      by = "tx",
      comparison = ~ I(cond_tx_foo(x)) | score
    )

    expect_equal(out1$estimate, out2$Difference, tolerance = 1e-4)
  })
)


withr::with_environment(
  new.env(),
  test_that("estimate_contrasts - custom function in 'comparison'", {
    dat <- expand.grid(
      treatment = 0:1,
      week = 1:52
    )
    set.seed(123)
    dat$y <- rpois(nrow(dat), 5)
    mod <- glm(y ~ treatment * week, data = dat, family = poisson)
    hyp <<- function(x) {
      sum(x$estimate[x$treatment == 1]) - sum(x$estimate[x$treatment == 0])
    }
    out1 <- marginaleffects::predictions(mod, type = "response", hypothesis = hyp)
    # we need to set `estimate = "average"`, because the function "hyp()"
    # required all predicted values, no data grid
    out2 <- estimate_contrasts(
      mod,
      c("treatment", "week"),
      comparison = hyp,
      estimate = "average"
    )
    expect_equal(out1$estimate, out2$Difference, tolerance = 1e-4)
  })
)
