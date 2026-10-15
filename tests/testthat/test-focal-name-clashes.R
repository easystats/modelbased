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
  contrasts_covariate = function(name) {
    d <- focal_data(name, numeric = TRUE)
    m <- stats::lm(focal_formula(paste0("am_f + `", name, "`")), data = d)
    estimate_contrasts(m, "am_f")
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
  contrasts_covariate = c("SE", "CI_low", "CI_high", "df"),
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
      expect_true(inherits(out, "error"), label = label)
      msg <- if (inherits(out, "error")) conditionMessage(out) else ""
      expect_true(grepl(paste0("`", name, "`"), msg, fixed = TRUE), label = label)
      expect_true(grepl("rename", msg, fixed = TRUE), label = label)
      expect_false(grepl("Sorry, calculating", msg, fixed = TRUE), label = label)
    }
  }
  expect_identical(n_cells, 33)
})


test_that("names outside the error cells give the same output as safe names", {
  grid_names <- c(
    "Predicted", "SE", "CI_low", "CI_high", "df", "rowid", "s.value",
    "predicted", "predicted_hi", "predicted_lo", "Parameter", "Level1",
    "Level2", "hypothesis"
  )
  grid_calls <- c(
    "means_two", "means_covariate", "contrasts_by", "contrasts", "slopes_by",
    "slopes_covariate"
  )
  # a covariate named "rowid" is not part of this check
  excluded <- list(means_covariate = "rowid", slopes_covariate = "rowid")
  n_combinations <- 0
  for (call_name in grid_calls) {
    call <- focal_calls[[call_name]]
    ref <- call("safe_var")
    skip_names <- c(focal_error_cells[[call_name]], excluded[[call_name]])
    for (name in setdiff(grid_names, skip_names)) {
      n_combinations <- n_combinations + 1
      out <- tryCatch(suppressWarnings(call(name)), error = function(e) e)
      diff <- focal_difference(ref, out, name)
      # the variable may also get the suffix ".1"
      if (!is.null(diff)) {
        diff <- focal_difference(ref, out, paste0(name, ".1"))
      }
      expect_null(diff, label = paste0(call_name, "(\"", name, "\"): ", diff))
    }
  }
  expect_identical(n_combinations, 53)
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


test_that("estimate_contrasts() suffixes by-variables named Level1 or Level2", {
  names <- c("Level1", "Level2")
  expect_focal_names_match("contrasts_by", names, suffix = ".1")
  for (name in names) {
    out <- focal_calls$contrasts_by(name)
    expect_false(anyDuplicated(colnames(out)) > 0, label = name)
  }
})


test_that("estimate_contrasts() keeps by-variables named hypothesis", {
  expect_focal_names_match("contrasts_by", "hypothesis")
  out <- focal_calls$contrasts_by("hypothesis")
  expect_true(all(c("Level1", "Level2", "hypothesis") %in% colnames(out)))
})


test_that("estimate_contrasts() works for a contrast variable named Parameter", {
  expect_focal_names_match("contrasts", "Parameter")
})


test_that("the suffix skips names that are already taken", {
  two_focal <- function(names, call) {
    d <- datasets::mtcars
    d[[names[1]]] <- factor(d$cyl)
    d[[names[2]]] <- factor(d$am)
    rhs <- paste0("wt * `", names[1], "` + `", names[2], "`")
    m <- stats::lm(focal_formula(rhs), data = d)
    call(m, names)
  }
  slopes <- function(m, names) estimate_slopes(m, "wt", by = names)
  means <- function(m, names) estimate_means(m, names)

  ref <- two_focal(c("s1", "s2"), slopes)
  out <- two_focal(c("p", "p.1"), slopes)
  expect_identical(colnames(out)[1:2], c("p.2", "p.1"))
  expect_identical(as.character(out$p.2), as.character(ref$s1))
  expect_identical(as.character(out$p.1), as.character(ref$s2))
  expect_equal(out$Slope, ref$Slope, tolerance = 1e-10)
  expect_equal(out$p, ref$p, tolerance = 1e-10)

  ref <- two_focal(c("s1", "s2"), means)
  out <- two_focal(c("t", "t.1"), means)
  expect_identical(colnames(out)[1:2], c("t.2", "t.1"))
  expect_identical(as.character(out$t.2), as.character(ref$s1))
  expect_identical(as.character(out$t.1), as.character(ref$s2))
  expect_equal(out$Mean, ref$Mean, tolerance = 1e-10)
  expect_equal(out$t, ref$t, tolerance = 1e-10)
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
      "Comparison", "Response", "Threshold", "Row",
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


# Contrasts, ratios, custom hypotheses and slope contrasts, with focal
# variables named like output columns. The output column with that name must
# keep its values and its position.

clash_names <- c(
  "Difference", "Ratio", "Slope", "Mean", "SE", "CI_low", "CI_high", "t", "z",
  "df", "p", "Predicted", "Parameter", "Level1", "Statistic", "s.value",
  "hypothesis"
)

clash_data <- function(name) {
  d <- datasets::mtcars
  d$am_f <- factor(d$am, levels = 0:1, labels = c("auto", "manual"))
  d[[name]] <- factor(d$cyl)
  d
}

clash_model <- function(name, rhs) {
  d <- clash_data(name)
  stats::lm(focal_formula(paste0(rhs, " * `", name, "`")), data = d)
}

clash_calls <- list(
  contrast = function(name) {
    estimate_contrasts(clash_model(name, "am_f"), name, by = "am_f")
  },
  contrasts_by = function(name) {
    estimate_contrasts(clash_model(name, "am_f"), "am_f", by = name)
  },
  contrasts_by_ratio = function(name) {
    estimate_contrasts(
      clash_model(name, "am_f"),
      "am_f",
      by = name,
      comparison = "ratio"
    )
  },
  contrasts_by_hypothesis = function(name) {
    estimate_contrasts(
      clash_model(name, "am_f"),
      "am_f",
      by = name,
      comparison = "b2=b1"
    )
  },
  slope_contrasts_by = function(name) {
    estimate_contrasts(clash_model(name, "wt"), "wt", by = name)
  },
  slopes_by_hypothesis = function(name) {
    estimate_slopes(clash_model(name, "wt"), "wt", by = name, comparison = "b2=b1")
  }
)

# names that give the error of `.check_marginaleffects_names()`
clash_error_cells <- list(
  contrast = c("SE", "CI_low", "CI_high", "df"),
  contrasts_by = c("Predicted", "SE", "CI_low", "CI_high", "df", "s.value"),
  contrasts_by_ratio = c("Predicted", "SE", "CI_low", "CI_high", "df", "s.value"),
  contrasts_by_hypothesis = c("Predicted", "SE", "CI_low", "CI_high", "df", "s.value"),
  slope_contrasts_by = c("df", "s.value"),
  slopes_by_hypothesis = c("df", "s.value")
)

# Returns NULL when `out` equals `ref` with "safe_var" replaced by `name`,
# otherwise a short description of the difference. Label columns (factors and
# character vectors) are compared as character.
clash_difference <- function(ref, out, name) {
  if (inherits(out, "error")) {
    return(paste("error:", conditionMessage(out)))
  }
  if (anyDuplicated(colnames(out))) {
    return(paste0("duplicated columns [", toString(colnames(out)), "]"))
  }
  expected <- gsub("safe_var", name, colnames(ref), fixed = TRUE)
  if (!identical(expected, colnames(out))) {
    return(paste0(
      "columns [", toString(colnames(out)), "], expected [",
      toString(expected), "]"
    ))
  }
  for (i in seq_along(ref)) {
    r <- ref[[i]]
    o <- out[[i]]
    if (is.factor(r) || is.character(r)) {
      r <- gsub("safe_var", name, as.character(r), fixed = TRUE)
      o <- if (is.factor(o) || is.character(o)) as.character(o) else o
    }
    if (!identical(class(r), class(o))) {
      return(paste0("column ", colnames(out)[i], " has class ", class(o)[1]))
    }
    if (!isTRUE(all.equal(r, o, check.attributes = FALSE))) {
      return(paste0("column ", colnames(out)[i], " has other values"))
    }
  }
  NULL
}

test_that("output columns keep their values for focal names like output columns", {
  n_errors <- 0
  n_compared <- 0
  for (call_name in names(clash_calls)) {
    call <- clash_calls[[call_name]]
    ref <- call("safe_var")
    for (name in clash_names) {
      out <- tryCatch(suppressWarnings(call(name)), error = function(e) e)
      label <- paste0(call_name, "(\"", name, "\")")
      if (name %in% clash_error_cells[[call_name]]) {
        n_errors <- n_errors + 1
        msg <- if (inherits(out, "error")) conditionMessage(out) else "no error"
        msg <- gsub("\\s+", " ", msg)
        expect_match(
          msg,
          paste0("Variable `", name, "` has a name that the marginaleffects package uses"),
          fixed = TRUE,
          label = label
        )
        next
      }
      n_compared <- n_compared + 1
      # the variable has its own name, or a suffix ".1", ".2", ...
      for (new_name in c(name, paste0(name, ".", 1:3))) {
        diff <- clash_difference(ref, out, new_name)
        if (is.null(diff)) {
          break
        }
      }
      if (!is.null(diff)) {
        diff <- clash_difference(ref, out, name)
      }
      expect_null(diff, label = paste0(label, ": ", diff))
    }
  }
  expect_identical(n_errors, 26)
  expect_identical(n_compared, 76)
})
