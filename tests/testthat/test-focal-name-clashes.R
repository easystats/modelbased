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
  d <- mtcars
  d$am_f <- factor(d$am)
  d[[name]] <- if (numeric) d$wt else factor(d$cyl)
  d
}

focal_formula <- function(rhs) {
  stats::as.formula(paste("mpg ~", rhs))
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
  contrasts = function(name) {
    d <- focal_data(name)
    m <- stats::lm(focal_formula(paste0("`", name, "` + hp")), data = d)
    estimate_contrasts(m, name)
  }
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
  NULL
}

expect_focal_names_match <- function(call_name, names) {
  call <- focal_calls[[call_name]]
  ref <- call("safe_var")
  for (name in names) {
    out <- tryCatch(suppressWarnings(call(name)), error = function(e) e)
    diff <- focal_difference(ref, out, name)
    expect_null(diff, label = paste0(call_name, "(\"", name, "\"): ", diff))
  }
}


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


test_that("focal variable names that worked before still work", {
  # names that matched on upstream main b964ea75, computed once with the
  # 99-name probe list
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
    d$vs <- mtcars$vs
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
