skip_on_cran()
skip_if_not_installed("marginaleffects", minimum_version = "0.29.0")

# focal variables named like columns of insight, marginaleffects or modelbased

focal_data <- function(name, numeric = FALSE) {
  d <- datasets::mtcars
  d$am_f <- factor(d$am)
  d[[name]] <- if (numeric) d$wt else factor(d$cyl)
  d
}

# the formula environment is the calling function, so that the data `d` is
# found there and not in the global environment
focal_formula <- function(rhs, env = parent.frame()) {
  stats::as.formula(paste("mpg ~", rhs), env = env)
}

focal_calls <- list(
  means = function(name) {
    d <- focal_data(name)
    m <- stats::lm(focal_formula(paste0("`", name, "` + hp")), data = d)
    estimate_means(m, name)
  },
  slopes_by = function(name) {
    d <- focal_data(name)
    m <- stats::lm(focal_formula(paste0("`", name, "` * wt")), data = d)
    estimate_slopes(m, "wt", by = name)
  },
  contrasts_by = function(name, ...) {
    d <- focal_data(name)
    m <- stats::lm(focal_formula(paste0("`", name, "` * am_f")), data = d)
    estimate_contrasts(m, "am_f", by = name, ...)
  },
  contrasts = function(name, ...) {
    d <- focal_data(name)
    m <- stats::lm(focal_formula(paste0("`", name, "` * am_f")), data = d)
    estimate_contrasts(m, name, by = "am_f", ...)
  }
)

# `out` must equal the output for "safe_var", with that name replaced by
# `new_name`
expect_focal_output <- function(call, name, new_name = name, ...) {
  ref <- as.data.frame(call("safe_var", ...))
  out <- as.data.frame(call(name, ...))
  expect_identical(colnames(out), gsub("safe_var", new_name, colnames(ref), fixed = TRUE))
  expect_equal(out, ref, ignore_attr = TRUE)
}


test_that("focal variables keep names that insight renames", {
  expect_focal_output(focal_calls$means, "type")
  expect_focal_output(focal_calls$slopes_by, "type")
  expect_focal_output(focal_calls$contrasts_by, "type")
  expect_focal_output(focal_calls$contrasts_by, "hypothesis")

  call <- function(name) {
    d <- focal_data(name, numeric = TRUE)
    m <- stats::lm(focal_formula(paste0("`", name, "` + hp")), data = d)
    newdata <- data.frame(x = c(2.123, 4.567), hp = 150)
    colnames(newdata)[1] <- name
    estimate_means(m, by = name, newdata = newdata)
  }
  expect_focal_output(call, "type")
})


test_that("focal variables named like output columns get the suffix '.1'", {
  expect_focal_output(focal_calls$means, "t", "t.1")
  expect_focal_output(focal_calls$slopes_by, "Slope", "Slope.1")
  expect_focal_output(focal_calls$contrasts_by, "p", "p.1")
  expect_focal_output(focal_calls$contrasts_by, "Level1", "Level1.1")
})


test_that("estimate_contrasts() filters suffixed by-variables", {
  call <- function(name, estimate) {
    d <- focal_data(name)
    m <- stats::lm(focal_formula(paste0("`", name, "` * am_f")), data = d)
    estimate_contrasts(m, "am_f", by = paste0(name, "=c('4', '6')"), estimate = estimate)
  }
  expect_focal_output(call, "p", "p.1", estimate = "typical")
  expect_focal_output(call, "p", "p.1", estimate = "average")
})


test_that("the suffix skips names that are already taken", {
  d <- datasets::mtcars
  d$p <- factor(d$cyl)
  d$p.1 <- factor(d$am)
  out <- estimate_slopes(
    stats::lm(mpg ~ wt * p + p.1, data = d),
    "wt",
    by = c("p", "p.1")
  )
  expect_identical(colnames(out)[1:2], c("p.2", "p.1"))
})


test_that("print() and plot() use the suffixed focal column", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("see")
  d <- focal_data("t")
  attr(d$t, "label") <- "Cylinders"
  out <- estimate_means(stats::lm(mpg ~ t + hp, data = d), "t")
  expect_no_error(utils::capture.output(print(out)))
  p <- plot(out)
  expect_identical(ggplot2::as_label(p$layers[[1]]$mapping$x), "t.1")
  expect_identical(p$labels$x, "Cylinders")
})


test_that("names that marginaleffects overwrites give an informative error", {
  d <- focal_data("SE")
  m <- stats::lm(mpg ~ SE * am_f, data = d)
  expect_error(estimate_means(m, c("am_f", "SE")), "Please rename", fixed = TRUE)
  d <- focal_data("df", numeric = TRUE)
  m <- stats::lm(mpg ~ am_f + df, data = d)
  expect_error(estimate_means(m, "am_f"), "Please rename", fixed = TRUE)
  d <- focal_data("predicted_hi")
  m <- stats::lm(mpg ~ predicted_hi * wt, data = d)
  expect_error(
    estimate_slopes(m, "wt", by = "predicted_hi"),
    "Please rename",
    fixed = TRUE
  )
})


test_that("output columns of contrasts keep their values for focal names like output columns", {
  expect_focal_output(focal_calls$contrasts, "Difference")
  expect_focal_output(focal_calls$contrasts_by, "Slope")
  expect_focal_output(focal_calls$contrasts_by, "Ratio", "Ratio.1", comparison = "ratio")
  expect_focal_output(
    focal_calls$contrasts_by,
    "Ratio",
    "Ratio.1",
    comparison = ratio ~ pairwise
  )
})


test_that("p_adjust in contrasts of estimate_relation() adjusts the p-values", {
  d <- focal_data("p.value")
  m <- stats::lm(mpg ~ am_f * p.value, data = d)
  call <- function(p_adjust) {
    estimate_contrasts(
      estimate_relation(m, by = c("am_f", "p.value")),
      "am_f",
      by = "p.value",
      p_adjust = p_adjust
    )
  }
  out0 <- call("none")
  out <- call("holm")
  expect_identical(as.character(out$p.value), c("4", "6", "8"))
  expect_equal(out$p, stats::p.adjust(out0$p, "holm"), tolerance = 1e-10)
})
