skip_on_cran()
skip_if_not_installed("marginaleffects", minimum_version = "0.28.0.22")
skip_on_os("mac")

test_that("estimate_contrasts - book examples 1", {
  data(puppy_love, package = "modelbased")

  # set contrasts.
  treat_vs_none <- c(-2 / 3, 1 / 3, 1 / 3)
  short_vs_long <- c(0, -1 / 2, 1 / 2)

  puppy_love$dose_original <- puppy_love$dose
  contrasts(puppy_love$dose) <- cbind(treat_vs_none, short_vs_long)

  puppy_love$treat_vs_none <- as.factor(
    ifelse(puppy_love$dose == "no treatment", "no treatment", "Puppies")
  )

  puppy_love$short_vs_long <- factor(
    short_vs_long,
    levels = c("short", "long", "no treatment")
  )

  puppy_love$puppy_love <- puppy_love$puppy_love - mean(puppy_love$puppy_love)

  # fit model
  m1 <- lm(happiness ~ puppy_love * dose, data = puppy_love)
  m2 <- lm(happiness ~ puppy_love * dose_original, data = puppy_love)

  expect_equal(
    coef(m1)["dosetreat_vs_none"],
    estimate_contrasts(m2, "dose_original", comparison = "((b2+b3)/2) = b1")$Difference,
    tolerance = 1e-4,
    ignore_attr = TRUE
  )
  expect_equal(
    coef(m1)["doseshort_vs_long"],
    estimate_contrasts(m2, "dose_original", comparison = "b3 = b2")$Difference,
    tolerance = 1e-4,
    ignore_attr = TRUE
  )
})


test_that("estimate_contrasts - book examples 2", {
  data(puppy_love, package = "modelbased")
  cond_tx <- cbind("no treatment" = c(1, 0, 0), "treatment" = c(0, 0.5, 0.5))

  m1 <- lm(happiness ~ puppy_love * dose, data = puppy_love)

  out1 <- marginaleffects::avg_slopes(m1, variables = "puppy_love", by = "dose", hypothesis = cond_tx)
  out2 <- estimate_slopes(m1, "puppy_love", by = "dose", hypothesis = cond_tx)
  expect_equal(out1$estimate, out2$Slope, tolerance = 1e-4)
  # we donb't officially have this argument for slopes, but we simply pass
  # it to the "hypothesis"
  out3 <- estimate_slopes(m1, "puppy_love", by = "dose", comparison = cond_tx)
  expect_equal(out3$Slope, out2$Slope, tolerance = 1e-4)
})


skip_if_not_installed("withr")

withr::with_environment(
  new.env(),
  test_that("estimate_contrasts - book examples 3", {
    data(puppy_love, package = "modelbased")
    cond_tx_foo <<- function(x) {
      drop(x %*% cbind("no treatment" = c(1, 0, 0), "treatment" = c(0, 0.5, 0.5)))
    }
    m1 <- lm(happiness ~ puppy_love * dose, data = puppy_love)

    out1 <- marginaleffects::avg_predictions(
      m1,
      variables = c("puppy_love", "dose"),
      hypothesis = ~ I(cond_tx_foo(x)) | puppy_love
    )

    out2 <- estimate_contrasts(
      m1,
      c("puppy_love=c(0, 1, 2.5, 4, 7)"),
      by = "dose",
      comparison = ~ I(cond_tx_foo(x)) | puppy_love
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
