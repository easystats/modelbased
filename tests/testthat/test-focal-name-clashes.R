skip_on_cran()
skip_if_not_installed("marginaleffects", minimum_version = "0.29.0")

# Focal variables can have names that insight, parameters or modelbased use
# for their own columns. Each test renames the focal variable and compares the
# output with the output for a neutral name (`safe_var`).

# names that `insight::standardize_names()` changes, or that modelbased drops
# by name
focal_reported_names <- c(
  "type", "p", "CI", "S", "s.value", "predicted", "conf.level", "df_residual",
  "df_error", "response", "n", "value", "component", "effect", "weight",
  "weights", "sumsq", "meansq", "std.dev", "y.level", "hypothesis", "Median",
  "Mean", "MAP", "Dxy", "rho", "r", "tau", "Difference", "Estimate", "F",
  "Chi2", "chisq", "Chisq", "W"
)

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
  means_two = function(name) {
    d <- focal_data(name)
    m <- stats::lm(focal_formula(paste0("`", name, "` * am_f")), data = d)
    estimate_means(m, c("am_f", name))
  },
  slopes_by = function(name) {
    d <- focal_data(name)
    m <- stats::lm(focal_formula(paste0("`", name, "` * wt")), data = d)
    estimate_slopes(m, "wt", by = name)
  },
  contrasts_by = function(name) {
    d <- focal_data(name)
    m <- stats::lm(focal_formula(paste0("`", name, "` * am_f")), data = d)
    estimate_contrasts(m, "am_f", by = name)
  },
  contrasts_by_filter = function(name) {
    d <- focal_data(name)
    m <- stats::lm(focal_formula(paste0("`", name, "` * am_f")), data = d)
    estimate_contrasts(m, "am_f", by = paste0(name, "=c('4', '6')"))
  },
  contrasts_by_filter_average = function(name) {
    d <- focal_data(name)
    m <- stats::lm(focal_formula(paste0("`", name, "` * am_f")), data = d)
    estimate_contrasts(
      m,
      "am_f",
      by = paste0(name, "='4'"),
      estimate = "average"
    )
  },
  contrasts = function(name) {
    d <- focal_data(name)
    m <- stats::lm(focal_formula(paste0("`", name, "` + hp")), data = d)
    estimate_contrasts(m, name)
  },
  means_covariate = function(name) {
    d <- focal_data(name, numeric = TRUE)
    m <- stats::lm(focal_formula(paste0("am_f + `", name, "`")), data = d)
    estimate_means(m, "am_f")
  },
  slopes_covariate = function(name) {
    d <- focal_data(name, numeric = TRUE)
    d$hp_s <- d$hp / 100
    m <- stats::lm(focal_formula(paste0("hp_s * am_f + `", name, "`")), data = d)
    estimate_slopes(m, "hp_s", by = "am_f")
  }
)

# Names that marginaleffects overwrites or uses itself, for each call. These
# calls give an error that asks users to rename the variable.
focal_error_cells <- list(
  means_two = c("Predicted", "SE", "CI_low", "CI_high", "df", "rowid", "s.value"),
  means_covariate = c("SE", "CI_low", "CI_high", "df"),
  contrasts_by = c("Predicted", "SE", "CI_low", "CI_high", "df", "rowid", "s.value"),
  contrasts = c("SE", "CI_low", "CI_high", "df", "rowid"),
  slopes_by = c("df", "rowid", "s.value", "predicted", "predicted_hi", "predicted_lo")
)

# Returns NULL when `out` equals `ref` with "safe_var" replaced by `name`,
# otherwise a short description of the first difference.
focal_difference <- function(ref, out, name) {
  if (inherits(out, "error")) {
    return(paste("error:", conditionMessage(out)))
  }
  swap <- function(x) {
    if (is.character(x)) gsub("safe_var", name, x, fixed = TRUE) else x
  }
  ref_names <- swap(colnames(ref))
  if (!identical(ref_names, colnames(out))) {
    return(paste0(
      "columns [", toString(colnames(out)), "], expected [",
      toString(ref_names), "]"
    ))
  }
  for (i in seq_along(ref)) {
    r <- ref[[i]]
    o <- out[[i]]
    if (!identical(class(r), class(o))) {
      return(paste0("column ", i, " has class ", class(o)[1]))
    }
    if (is.factor(r)) {
      levels(r) <- swap(levels(r))
    }
    r <- swap(r)
    if (!isTRUE(all.equal(r, o, check.attributes = FALSE))) {
      return(paste0("column ", i, " has other values"))
    }
  }
  for (a in c("focal_terms", "by", "trend", "contrast")) {
    if (!identical(swap(attr(ref, a, exact = TRUE)), attr(out, a, exact = TRUE))) {
      return(paste0("attribute ", a, " differs"))
    }
  }
  ref_grid <- swap(colnames(attr(ref, "datagrid", exact = TRUE)))
  out_grid <- colnames(attr(out, "datagrid", exact = TRUE))
  if (!identical(ref_grid, out_grid)) {
    return(paste0("datagrid columns [", toString(out_grid), "]"))
  }
  NULL
}

expect_focal_names_match <- function(call_name, names, suffix = "") {
  call <- focal_calls[[call_name]]
  ref <- call("safe_var")
  for (name in names) {
    out <- tryCatch(suppressWarnings(call(name)), error = function(e) e)
    diff <- focal_difference(ref, out, paste0(name, suffix))
    expect_null(diff, label = paste0(call_name, "(\"", name, "\"): ", diff))
  }
}


test_that("names that marginaleffects overwrites give an informative error", {
  n_cells <- 0
  for (call_name in names(focal_error_cells)) {
    for (name in focal_error_cells[[call_name]]) {
      n_cells <- n_cells + 1
      out <- tryCatch(suppressWarnings(focal_calls[[call_name]](name)), error = function(e) e)
      label <- paste0(call_name, "(\"", name, "\")")
      expect_s3_class(out, "error")
      msg <- if (inherits(out, "error")) conditionMessage(out) else ""
      expect_true(grepl(paste0("`", name, "`"), msg, fixed = TRUE), label = label)
      expect_true(grepl("rename", msg, fixed = TRUE), label = label)
      expect_false(grepl("Sorry, calculating", msg, fixed = TRUE), label = label)
    }
  }
  expect_identical(n_cells, 29)
})


test_that("estimate_means() keeps focal variables named like renamed columns", {
  names <- setdiff(focal_reported_names, c("Mean", "s.value"))
  expect_focal_names_match("means", names)
  expect_focal_names_match("means_two", names)
})


test_that("estimate_slopes() keeps by-variables named like renamed columns", {
  names <- setdiff(focal_reported_names, c("p", "s.value", "predicted"))
  expect_focal_names_match("slopes_by", names)
})


test_that("estimate_contrasts() keeps by-variables named like renamed columns", {
  names <- setdiff(
    focal_reported_names,
    c("p", "s.value", "hypothesis", "Difference")
  )
  expect_focal_names_match("contrasts_by", names)
})


# A by-variable with the name of an output column gets the suffix ".1", and the
# output column keeps its name. The expected output is the output for
# "safe_var", with "safe_var" replaced by "<name>.1".

# marginaleffects renames columns named "Predicted", "SE", "CI_low" and
# "CI_high" for means and contrasts, so these names are not tested there.

test_that("estimate_means() suffixes focal variables named like output columns", {
  names <- c("Mean", "t")
  expect_focal_names_match("means", names, suffix = ".1")
  expect_focal_names_match("means_two", names, suffix = ".1")
})


test_that("estimate_slopes() suffixes by-variables named like output columns", {
  names <- c("Slope", "SE", "CI_low", "CI_high", "t", "p")
  expect_focal_names_match("slopes_by", names, suffix = ".1")
})


test_that("estimate_contrasts() suffixes by-variables named like output columns", {
  names <- c("Difference", "t", "p")
  expect_focal_names_match("contrasts_by", names, suffix = ".1")
})


test_that("estimate_contrasts() filters suffixed by-variables", {
  names <- c("Difference", "t", "p")
  expect_focal_names_match("contrasts_by_filter", names, suffix = ".1")
  expect_focal_names_match("contrasts_by_filter_average", names, suffix = ".1")
})


test_that("print() and plot() use the suffixed focal column", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("see")
  for (name in c("Mean", "t")) {
    out <- focal_calls$means(name)
    expect_no_error(utils::capture.output(print(out)))
    p <- plot(out)
    expect_no_error(ggplot2::ggplot_build(p))
    x_mappings <- unlist(lapply(p$layers, function(l) {
      if (is.null(l$mapping$x)) NULL else ggplot2::as_label(l$mapping$x)
    }))
    expect_true(
      length(x_mappings) > 0 && all(x_mappings == paste0(name, ".1")),
      label = paste0("x mappings for \"", name, "\": ", toString(x_mappings))
    )
  }
})


test_that("plot() uses the variable label of a suffixed focal variable", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("see")
  for (name in c("safe_var", "Mean", "t")) {
    d <- focal_data(name)
    attr(d[[name]], "label") <- "Cylinders"
    m <- stats::lm(focal_formula(paste0("`", name, "` + hp")), data = d)
    p <- plot(estimate_means(m, name))
    expect_identical(p$labels$x, "Cylinders", label = paste0("x label for \"", name, "\""))
  }
})


test_that("focal variable names that worked before still work", {
  # names that matched on upstream main b964ea75, computed once from a list
  # of 99 candidate names (the reported names and other column, argument and
  # output names)
  matched_before <- list(
    means = c(
      "z", "rowidcf", "wts", "predicted_lo", "predicted_hi", "emmean",
      "prob", "rate", "lower.CL", "upper.CL", "asymp.LCL", "asymp.UCL",
      "Level1", "Level2", "Parameter", "Comparison", "Response", "Threshold",
      "Row", "Residuals", "Group", "Level", "Slope", "Coefficient",
      "Probability", "Contrast", "Component", "Effects", "Type", "Ratio",
      "iter_1", "x", "model", "data", "at", "ci", "trend", "predict",
      "comparison", "length", "range", "fun", "i", "out", "means", "grid"
    ),
    means_two = c(
      "z", "rowidcf", "wts", "predicted_lo", "predicted_hi", "emmean",
      "prob", "rate", "lower.CL", "upper.CL", "asymp.LCL", "asymp.UCL",
      "Level1", "Level2", "Parameter", "Comparison", "Response", "Threshold",
      "Row", "Residuals", "Group", "Level", "Slope", "Coefficient",
      "Probability", "Contrast", "Component", "Effects", "Type", "Ratio",
      "iter_1", "x", "model", "data", "at", "ci", "trend", "predict",
      "comparison", "length", "range", "fun", "i", "out", "means", "grid"
    ),
    slopes_by = c(
      "rowidcf", "wts", "emmean", "prob", "rate", "lower.CL", "upper.CL",
      "asymp.LCL", "asymp.UCL", "Level1", "Level2", "Response", "Threshold",
      "Row", "Residuals", "Group", "Level", "Probability", "Contrast",
      "Component", "Effects", "Type", "Ratio", "iter_1", "x", "model",
      "data", "at", "ci", "trend", "predict", "comparison", "length",
      "range", "fun", "i", "out", "means", "grid"
    ),
    contrasts_by = c(
      "z", "rowidcf", "wts", "predicted_lo", "predicted_hi", "emmean",
      "prob", "rate", "lower.CL", "upper.CL", "asymp.LCL", "asymp.UCL",
      "Level1", "Level2", "Comparison", "Response", "Threshold", "Row",
      "Residuals", "Group", "Level", "Probability", "Contrast", "Component",
      "Effects", "Type", "Ratio", "iter_1", "x", "model", "data", "at", "ci",
      "trend", "predict", "comparison", "length", "range", "fun", "i", "out",
      "means", "grid"
    ),
    contrasts = c(
      "z", "F", "Chi2", "chisq", "Chisq", "W", "Median", "Mean", "MAP",
      "Dxy", "rho", "r", "tau", "df_residual", "df_error", "std.dev",
      "s.value", "component", "effect", "predicted", "response", "y.level",
      "conf.level", "n", "type", "weight", "weights", "hypothesis", "value",
      "sumsq", "meansq", "Estimate", "rowidcf", "wts", "predicted_lo",
      "predicted_hi", "emmean", "prob", "rate", "lower.CL", "upper.CL",
      "asymp.LCL", "asymp.UCL", "Predicted", "CI", "Level1", "Level2",
      "Comparison", "Response", "Threshold", "Row", "Residuals", "Group",
      "Level", "Slope", "Coefficient", "Probability", "Contrast",
      "Component", "Effects", "Type", "Ratio", "S", "iter_1", "x", "model",
      "data", "at", "ci", "trend", "predict", "comparison", "length",
      "range", "fun", "i", "out", "means", "grid"
    )
  )
  for (call_name in names(matched_before)) {
    expect_focal_names_match(call_name, matched_before[[call_name]])
  }
})


test_that("estimate_means() keeps numeric focal variables for glm", {
  names <- setdiff(focal_reported_names, c("Mean", "s.value"))
  call <- function(name) {
    d <- focal_data(name, numeric = TRUE)
    d$vs <- datasets::mtcars$vs
    m <- stats::glm(
      stats::as.formula(paste0("vs ~ am_f * `", name, "`")),
      family = stats::binomial(),
      data = d
    )
    estimate_means(m, c("am_f", name))
  }
  ref <- suppressWarnings(call("safe_var"))
  for (name in names) {
    out <- tryCatch(suppressWarnings(call(name)), error = function(e) e)
    diff <- focal_difference(ref, out, name)
    expect_null(diff, label = paste0("glm(\"", name, "\"): ", diff))
  }
})


test_that("estimate_means() keeps focal variables with custom newdata", {
  # the values in `newdata` are not part of the generated data grid
  names <- setdiff(focal_reported_names, c("Mean", "s.value"))
  call <- function(name) {
    d <- focal_data(name, numeric = TRUE)
    m <- stats::lm(focal_formula(paste0("`", name, "` + hp")), data = d)
    newdata <- data.frame(x = c(2.123, 4.567), hp = 150)
    colnames(newdata)[1] <- name
    estimate_means(m, by = name, newdata = newdata)
  }
  ref <- call("safe_var")
  for (name in names) {
    out <- tryCatch(suppressWarnings(call(name)), error = function(e) e)
    diff <- focal_difference(ref, out, name)
    expect_null(diff, label = paste0("newdata(\"", name, "\"): ", diff))
  }
})
