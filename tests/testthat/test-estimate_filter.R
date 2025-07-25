skip_on_cran()
skip_if_not_installed("marginaleffects")
skip_on_os("mac")

test_that("filtering for by and contrast works for different estimate options", {
  data(efc, package = "modelbased")
  efc <- datawizard::to_factor(efc, c("c172code", "c161sex", "e16sex", "e42dep"))
  levels(efc$c172code) <- c("low", "mid", "high")
  m <- lm(barthtot ~ c161sex * c172code + neg_c_7, data = efc)

  out <- estimate_means(m, c("c172code=c('low','mid')", "c161sex"), estimate = "specific")
  expect_identical(dim(out), c(4L, 8L))
  expect_identical(as.character(out$c172code), c("low", "low", "mid", "mid"))
  out <- estimate_means(m, c("c172code=c('low','mid')", "c161sex"), estimate = "typical")
  expect_identical(dim(out), c(4L, 8L))
  expect_identical(as.character(out$c172code), c("low", "low", "mid", "mid"))
  out <- estimate_means(m, c("c172code=c('low','mid')", "c161sex"), estimate = "average")
  expect_identical(dim(out), c(4L, 8L))
  expect_identical(as.character(out$c172code), c("low", "low", "mid", "mid"))
  out <- estimate_means(m, c("c172code=c('low','mid')", "c161sex"), estimate = "population")
  expect_identical(dim(out), c(4L, 8L))
  expect_identical(as.character(out$c172code), c("low", "low", "mid", "mid"))

  out <- estimate_contrasts(m, "c172code=c('low','mid')", by = "c161sex", estimate = "specific")
  expect_identical(dim(out), c(2L, 10L))
  expect_identical(as.character(out$Level1), c("mid", "mid"))
  out <- estimate_contrasts(m, "c172code=c('low','mid')", by = "c161sex", estimate = "typical")
  expect_identical(dim(out), c(2L, 10L))
  expect_identical(as.character(out$Level1), c("mid", "mid"))
  out <- estimate_contrasts(m, "c172code=c('low','mid')", by = "c161sex", estimate = "average")
  expect_identical(dim(out), c(2L, 10L))
  expect_identical(as.character(out$Level1), c("mid", "mid"))
  out <- estimate_contrasts(m, "c172code=c('low','mid')", by = "c161sex", estimate = "population")
  expect_identical(dim(out), c(2L, 10L))
  expect_identical(as.character(out$Level1), c("mid", "mid"))
})


test_that("special filtering for by and contrast works", {
  data(iris)
  model <- lm(Sepal.Width ~ Species * Petal.Width, data = iris)
  out <- estimate_contrasts(model, contrast = c("Species", "Petal.Width=c(1, 2)"))
  expect_identical(dim(out), c(15L, 9L))
  expect_identical(
    as.character(out$Level1),
    c(
      "setosa, 2", "versicolor, 1", "versicolor, 2", "virginica, 1",
      "virginica, 2", "versicolor, 1", "versicolor, 2", "virginica, 1",
      "virginica, 2", "versicolor, 2", "virginica, 1", "virginica, 2",
      "virginica, 1", "virginica, 2", "virginica, 2"
    )
  )

  ## FIXME: not working yet

  # out <- estimate_contrasts(model, contrast = c("Species", "Petal.Width=c(1, 2)"), estimate = "average")
  # expect_identical(dim(out), c(15L, 9L))
  # expect_identical(
  #   as.character(out$Level1),
  #   c(
  #     "setosa, 2", "versicolor, 1", "versicolor, 2", "virginica, 1",
  #     "virginica, 2", "versicolor, 1", "versicolor, 2", "virginica, 1",
  #     "virginica, 2", "versicolor, 2", "virginica, 1", "virginica, 2",
  #     "virginica, 1", "virginica, 2", "virginica, 2"
  #   )
  # )
})


test_that("special filtering for by and contrast works", {
  set.seed(1234)
  n <- 365
  event_start <- 200
  time <- seq_len(n)
  event <- c(rep_len(0, event_start), rep_len(1, n - event_start))
  outcome <- 10 + # 1. Pre-intervention intercept
    15 * time + # 2. Pre-intervention slope (trend)
    20 * event + # 3. Level change (a jump of +20)
    5 * event * time + # 4. Slope change (slope becomes 15 + 5 = 20)
    rnorm(n, mean = 0, sd = 100) # Add some random noise

  dat <- data.frame(outcome, time, event)
  mod <- lm(outcome ~ time * event, data = dat)
  expect_error(
    estimate_contrasts(mod, contrast = "event", by = "time=200", estimate = "average"),
    regex = "No rows returned from marginal means"
  )
  expect_silent(estimate_contrasts(mod, contrast = "event", by = "time=200"))
})
