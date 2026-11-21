# warn about custom contrasts combined with filtering and `estimate = "average"`
#
# When `estimate = "average"`, predictions are based on the empirical data rather
# than a balanced reference grid. If the user simultaneously filters levels or
# values in `contrast` or `by` (e.g., `contrast = "x = c('a', 'b')"`) and passes
# a custom `comparison` hypothesis using positional row references (such as
# `"b2 - b1"`), the internal row re-ordering can mismatch the filtered rows and
# silently yield wrong contrast estimates.
#
# This helper detects that combination and issues a warning advising the user
# to pass bare variable names to `contrast` and `by` and inspect the row order
# via `estimate_means()` first.
#
# Arguments:
# - estimate: The marginalization type string (e.g., `"average"`).
# - by: Character vector of focal variables (possibly including filter specs).
# - original_contrast: The user-supplied `contrast` argument before cleaning.
# - comparison: The custom hypothesis specification passed to `comparison`.
#
# Returns: `NULL` (called for its warning side-effect).
#
#' @keywords internal
#' @noRd
.check_custom_contrasts_and_filter <- function(
  estimate,
  by,
  original_contrast,
  comparison
) {
  # setup message to tell user that results must be cross-checked
  if (estimate == "average" && !all(.grep_cleaned_by_vars(by) == by)) {
    # first, extract contrast with filtering, which doesn't work
    wrong_contrast <- setdiff(original_contrast, .grep_cleaned_by_vars(original_contrast))
    # clean contrast and by, used to show correct example
    original_contrast <- .grep_cleaned_by_vars(original_contrast)
    original_by <- setdiff(.grep_cleaned_by_vars(by), original_contrast)
    msg1 <- paste0(
      "Selecting specific levels or values in the `contrast` or `by` arguments ",
      if (length(wrong_contrast)) {
        paste0(
          "(e.g., ",
          paste0(
            "`contrast = c(",
            paste0("\"", wrong_contrast, "\"", collapse = ", "),
            ")`) "
          )
        )
      },
      "is error-prone for custom contrasts like ",
      paste0("`comparison = \"", comparison, "\"`"),
      " in combination with `estimate = \"average\"`. This can yield incorrect results,",
      " even if the output suggests the correct comparisons. It is strongly recommended",
      " to use only bare variable names in `contrast` and `by`, e.g.\n\n"
    )
    msg2 <- insight::color_text(
      paste0(
        "  estimate_contrasts(\n",
        "    contrast = c(",
        paste0("\"", original_contrast, "\"", collapse = ", "),
        "),\n",
        if (length(original_by)) {
          paste0("    by = c(", paste0("\"", original_by, "\"", collapse = ", "), "),\n")
        },
        "    estimate = \"average\",\n    comparison = ...\n  )"
      ),
      color = "green"
    )
    msg3 <- "\n\n  and update your `comparison` argument accordingly.\n  Run\n\n"
    msg4 <- insight::color_text(
      paste0(
        "  estimate_means(\n",
        "    c(",
        paste0("\"", c(original_contrast, original_by), "\"", collapse = ", "),
        "),\n    estimate = \"average\"\n  )"
      ),
      color = "green"
    )
    msg5 <- "\n\n  first to find out the correct rows to specify the `b`-coefficients for the `comparison` argument.\n"
    warning(
      insight::format_message(msg1),
      msg2,
      msg3,
      msg4,
      insight::format_message(msg5),
      call. = FALSE
    )
  }
}


# check for missing (all-NA) standard errors ----------------------------------
#
# In some models - particularly complex mixed-effects models - the underlying
# backend (`{marginaleffects}` or `{emmeans}`) may fail to compute the
# variance-covariance matrix for contrasts, resulting in `SE` values that are
# entirely `NA`.
#
# When `verbose = TRUE` and all standard errors are `NA`, this helper informs
# the user and - if `by` or `contrast` variables are available - constructs a
# ready-to-run two-step workaround using `estimate_relation()` followed by
# `estimate_contrasts()`.
#
# Arguments:
# - out: Formatted data frame of estimated means, contrasts, or slopes.
# - by: Optional character vector of `by` variable names.
# - contrast: Optional character vector of `contrast` variable names.
# - model: The fitted model object (currently unused, kept for consistency).
# - model_name: Deparsed name of the model object for the example code snippet.
# - verbose: Logical, whether to print the informative message.
# - ...: Additional arguments (currently unused).
#
# Returns: `NULL` (called for its message side-effect).
#
#' @keywords internal
#' @noRd
.check_standard_errors <- function(
  out,
  by = NULL,
  contrast = NULL,
  model = NULL,
  model_name = "model",
  verbose = TRUE,
  ...
) {
  if (!verbose || is.null(out$SE)) {
    return(NULL)
  }

  if (all(is.na(out$SE))) {
    # we show an example code how to resolve the problem. this example
    # code only works when we have at least `by` or `contrast`. If both
    # are NULL, we ignore the example code (see below)
    code_snippet <- paste0("\n\nestim <- estimate_relation(\n  ", model_name)
    by_vars <- c(by, contrast)
    if (!is.null(by_vars)) {
      code_snippet <- paste0(
        code_snippet,
        ",\n  by = ",
        ifelse(length(by_vars) > 1, "c(", ""),
        paste0("\"", by_vars, "\"", collapse = ", "),
        ifelse(length(by_vars) > 1, ")", "")
      )
    }
    code_snippet <- paste0(code_snippet, "\n)\nestimate_contrasts(\n  estim")
    if (!is.null(contrast)) {
      code_snippet <- paste0(
        code_snippet,
        ",\n  contrast = ",
        ifelse(length(contrast) > 1, "c(", ""),
        paste0("\"", contrast, "\"", collapse = ", "),
        ifelse(length(contrast) > 1, ")", "")
      )
    }
    code_snippet <- paste0(code_snippet, "\n)")
    # setup message
    msg <- insight::format_message(
      "Could not calculate standard errors for contrasts. This can happen when random effects are involved."
    )
    # add example code, if valid
    if (!is.null(by_vars)) {
      msg <- c(
        paste(msg, "You may try following:"),
        insight::color_text(code_snippet, "green"),
        "\n"
      )
    }
    message(msg)

    # disable message for now, see
    # https://github.com/easystats/modelbased/issues/526
    # } else if (length(out$SE) > 1 && isTRUE(all(out$SE == out$SE[1])) && insight::is_mixed_model(model)) {
    #   msg <- "Standard errors are probably not reliable. This can happen when random effects are involved. You may try `estimate_relation()` instead."
    #   if (!inherits(model, "glmmTMB")) {
    #     msg <- paste(msg, "You may also try package {.pkg glmmTMB} to produce valid standard errors.")
    #   }
    #   insight::format_alert(msg)
  }
}


# inform user about how model offset terms are handled ------------------------
#
# When a regression model includes an `offset()` term and the user has not
# explicitly supplied a fixed `offset` value, passed the offset in `newdata`, or
# included it as a focal predictor in `by`, predictions depend on the chosen
# `estimate` method:
# - `"specific"` / `"typical"` fix the offset at its sample mean.
# - `"average"` / `"population"` average predictions across the empirical
#   distribution of the offset.
#
# This helper alerts the user to this behavior and additionally clarifies that
# for log-transformed offsets (e.g., `offset(log(exposure))`), the `offset`
# argument expects values on the original (untransformed) scale.
#
# Arguments:
# - model: The fitted model object.
# - estimate: The marginalization type string (`"specific"`, `"typical"`,
#   `"average"`, or `"population"`).
# - offset: The user-supplied `offset` value (if any).
# - my_args: List of parsed focal arguments (containing `$by`).
# - dots: List of additional arguments passed via `...` (checked for `newdata`
#   and `offset`).
# - verbose: Logical, whether to display the alert.
#
# Returns: `NULL` (called for its alert side-effect).
#
#' @keywords internal
#' @noRd
.check_offset <- function(
  model,
  estimate,
  offset = NULL,
  my_args = NULL,
  dots = NULL,
  verbose = TRUE
) {
  # check if model has an offset at all
  model_offset <- insight::find_offset(model)

  # if we have no offset at all, or offset is explicitly specified, we don't
  # need a warning here...
  if (is.null(model_offset) || !is.null(dots$offset)) {
    return(NULL)
  }

  # check if a) we have "newdata" and b) offset was specified in "newdata".
  # if not, "offset_miss_1" flags that offset is not specified or provided,
  # we need a warning
  offset_miss_1 <- !is.null(dots$newdata) && !model_offset %in% colnames(dots$newdata)

  # if no newdata provided, offset_miss_1 is FALSE, indicating no problems yet.
  # We only need to start the 2nd check if we have no newdata
  offset_miss_2 <- is.null(dots$newdata) &&
    !is.null(my_args$by) &&
    !any(startsWith(my_args$by, model_offset))

  if (verbose && is.null(offset) && (offset_miss_1 || offset_miss_2)) {
    msg <- NULL
    # if no offset argument was specified, tell user what this means
    msg <- switch(
      estimate,
      specific = ,
      typical = paste(
        "Model contains an offset-term, which is set to its mean value.",
        "If you want to average predictions over the distribution of the offset",
        "(if appropriate), use `estimate = \"average\"` or `estimate = \"population\"`.",
        "If you want to fix the offset to a specific value, for instance `1`,",
        "use `offset = 1`."
      ),
      average = ,
      population = paste(
        "Model contains an offset-term and you average predictions over the",
        "distribution of that offset. If you want to fix the offset to a",
        "specific value, for instance `1`, use `offset = 1`."
      )
    )
    # if offset term is log-transformed, tell user. offset should be fixed then
    log_offset <- insight::find_transformation(insight::find_offset(
      model,
      as_term = TRUE
    ))
    if (!is.null(log_offset) && startsWith(log_offset, "log")) {
      msg <- c(
        msg,
        paste(
          "\nWe also found that the model has a log-transformed offset term.",
          "If you use the `offset` argument, the log-transformation will",
          "automatically be applied to the provided offset-value. I.e., consider",
          "using, for instance, `offset = 10` and not `offset = log(10)`."
        )
      )
    }
    if (!is.null(msg)) {
      insight::format_alert(msg)
    }
  }
}


# prevent variable names that collide with `{marginaleffects}` output columns -
#
# `{marginaleffects}` reserves specific column names in its returned data frames
# (such as `"rowid"`, `"df"`, `"s.value"`, or `"predicted"`). If a predictor in
# the user's model shares one of these names, `{marginaleffects}` overwrites or
# renames the column, leading to silent miscalculations or downstream failures.
#
# This helper strips filter specifications from focal variables and throws an
# informative error if any predictor name collides with the reserved names for
# the requested estimation type (`"means"` vs. slopes/contrasts). Bayesian
# models are skipped because their output is tidied via `describe_posterior()`.
#
# Arguments:
# - model: The fitted model object.
# - model_info: Model metadata list from `insight::model_info()`.
# - focal: Character vector of all focal predictor names.
# - contrast: Optional character vector of `contrast` predictor names.
# - trend: Optional character vector of `trend` predictor names.
# - type: Estimation context (`"means"` or other, e.g., `"slopes"`), which
#   determines the exact set of reserved column names checked.
#
# Returns: `NULL` invisibly (throws an error if a collision is detected).
#
#' @keywords internal
#' @noRd
.check_marginaleffects_names <- function(
  model,
  model_info,
  focal = NULL,
  contrast = NULL,
  trend = NULL,
  type = "means"
) {
  # Bayesian models are not checked yet: describe_posterior() tidies their
  # output, and which names break it has not been worked out
  if (isTRUE(model_info$is_bayesian)) {
    return(invisible(NULL))
  }
  # remove filters such as "p='4'" or "x=[sd]" from names
  clean <- function(x) {
    if (is.null(x)) {
      return(NULL)
    }
    insight::trim_ws(sub("\\s*[=\\[].*$", "", x))
  }
  focal <- clean(focal)
  by_vars <- setdiff(focal, clean(c(contrast, trend)))

  # marginaleffects renames, overwrites or uses columns with these names, so
  # that the output for variables with these names is wrong
  if (identical(type, "means")) {
    covariates <- setdiff(
      insight::find_predictors(model, effects = "all", component = "all", flatten = TRUE),
      focal
    )
    bad <- c(
      intersect(c("SE", "CI_low", "CI_high", "df"), c(focal, covariates)),
      intersect("rowid", focal),
      intersect(c("Predicted", "s.value"), by_vars)
    )
  } else {
    bad <- intersect(
      c("df", "rowid", "s.value", "predicted", "predicted_hi", "predicted_lo"),
      by_vars
    )
  }
  bad <- unique(bad)

  if (length(bad)) {
    names_text <- datawizard::text_concatenate(bad, enclose = "`")
    insight::format_error(paste0(
      if (length(bad) > 1) "Variables " else "Variable ",
      names_text,
      if (length(bad) > 1) " have names " else " has a name ",
      "that the marginaleffects package uses for its own columns, so the results",
      " would be wrong or the call would fail. Please rename ",
      names_text,
      " in your data and fit the model again."
    ))
  }
  invisible(NULL)
}


# harmonize `data` and `newdata` arguments passed via `...` -------------------
#
# Across the `{easystats}` ecosystem, `data` is supported as a consistent alias
# for `{marginaleffects}`'s `newdata` argument. This helper moves `dots$data`
# into `dots$newdata`, removes `dots$data`, and alerts the user if both were
# supplied simultaneously.
#
# Arguments:
# - dots: Named list of extra arguments captured from `...`.
# - verbose: Logical, whether to alert the user when both `data` and `newdata`
#   are present.
#
# Returns: The updated `dots` list with `data` normalized to `newdata`.
#
#' @keywords internal
#' @noRd
.check_dots_data <- function(dots, verbose) {
  if (!is.null(dots$data)) {
    if (!is.null(dots$newdata) && verbose) {
      insight::format_alert(
        "Both 'data' and 'newdata' were provided. Please specify only one. Ignoring 'newdata' and using 'data' instead."
      )
    }
    dots$newdata <- dots$data
    dots$data <- NULL
  }
  dots
}


# normalize formula-based inequality comparisons to canonical strings ---------
#
# Users can request inequality / concentration metrics (such as Gini or Atkinson
# indices via `estimate_contrasts()`) either as character keywords (e.g.,
# `"inequality"`) or as formulas (e.g., `~inequality` or `ratio ~ inequality`).
#
# This helper converts unstratified inequality formulas into their canonical
# string identifiers so they can be dispatched uniformly, while leaving
# stratified formulas (e.g., `~ inequality | grp1 + grp2`) untouched for later
# processing by `.process_inequality_formula()`.
#
# Arguments:
# - comparison: The `comparison` argument (character string, formula, or
#   function).
#
# Returns: A canonical inequality string if `comparison` is an unstratified
#   inequality formula; otherwise, the unchanged `comparison` object.
#
#' @keywords internal
#' @noRd
.check_for_inequality_comparison <- function(comparison) {
  # check whether we have a formula definition of inequality comparisons,
  # and convert it to a string
  #
  # the default formulas are converted to a string:
  # ~inequality -> "inequality"
  # inequality ~ pairwise -> "inequality_pairwise"
  # ratio ~ inequality -> "inequality_ratio"
  # ratio ~ inequality + pairwise` -> "inequality_ratio_pairwise"
  #
  # we may have other formulas that control grouping and averaging, like
  # `~ inequality | grp1 + grp2`. In this case, the formula is returned as is
  # and processed later in ".process_inequality_formula()"
  if (inherits(comparison, "formula")) {
    # parse variables into a string
    out <- paste(all.vars(comparison), collapse = "_")
    # handle special cases
    out <- switch(
      out,
      ratio_inequality = "inequality_ratio",
      ratio_inequality_pairwise = "inequality_ratio_pairwise",
      out
    )
    if (.is_inequality_comparison(out)) {
      return(out)
    }
  }
  comparison
}


# determine HTML table formatting backend -------------------------------------
#
# Resolves which table-rendering backend should be used for HTML export methods
# (`print_html()`): `"tt"` when `backend = "tt"` (`{tinytable}`) is explicitly
# requested via `...`, and `"html"` (`{gt}`) otherwise.
#
# Arguments:
# - ...: Additional arguments passed to `print_html()`, checked for `backend`.
#
# Returns: Character string (`"tt"` or `"html"`).
#
#' @keywords internal
#' @noRd
.check_format_backend <- function(...) {
  # we allow exporting HTML format based on "gt" or "tinytable"
  dots <- list(...)
  if (identical(dots$backend, "tt")) {
    "tt"
  } else {
    "html"
  }
}


# validate the `predict` argument after matching ------------------------------
#
# Argument validation helpers return `NA` when a user-supplied `predict` string
# does not match any supported prediction scale or type. This helper catches
# that `NA` sentinel and throws a formatted error listing all valid options
# (always including `"response"` and `"link"`).
#
# Arguments:
# - predict: The matched `predict` value (or `NA` if matching failed).
# - valid_types: Character vector of model-specific valid prediction types.
# - error_arg: Name of the argument to display in the error message (usually
#   `"predict"`).
#
# Returns: `NULL` (throws an error if `predict` is `NA`).
#
#' @keywords internal
#' @noRd
.check_predict_arg <- function(predict, valid_types, error_arg) {
  if (isTRUE(is.na(predict))) {
    # add modelbased-options to valid types
    valid_types <- unique(c("response", "link", valid_types))
    insight::format_error(paste0(
      "The option provided in the `",
      error_arg,
      "` argument is not recognized.",
      " Valid options are: ",
      datawizard::text_concatenate(valid_types, enclose = "`"),
      "."
    ))
  }
}


# handle errors from marginaleffects -----------------------------------------
#
# This helper function processes errors that occur during calls to the
# {marginaleffects} package. It creates more informative and user-friendly
# error messages by inspecting the original error and suggesting potential
# solutions for common problems, such as using a different `estimate` option
# or switching to the `emmeans` backend.
#
# Arguments:
# - out: The error object returned from the `tryCatch` block.
# - fun_args: A list of arguments that were passed to the failing
#   {marginaleffects} function.
#
# returns: A character vector containing the formatted, user-friendly error
#   message, which is then passed to `insight::format_error()`.
#
#' @keywords internal
#' @noRd
.check_marginaleffects_errors <- function(out, fun_args) {
  # what was requested?
  if (is.null(fun_args$hypothesis)) {
    fun <- "marginal means"
  } else {
    fun <- "marginal contrasts"
  }
  # clean original error message
  out$message <- gsub("\\s+", " ", gsub("\n", "", out$message, fixed = TRUE))
  # setup clear error message
  msg <- c(
    paste0("Sorry, calculating ", fun, " failed with following error:"),
    insight::color_text(gsub("\n", "", out$message, fixed = TRUE), "red")
  )
  # handle exceptions ------------------------------------------------------
  # we get this error when we should use counterfactuals
  if (grepl("not found in column names", out$message, fixed = TRUE)) {
    msg <- c(
      msg,
      "\nIt seems that not all required levels of the focal terms are available in the provided data. If you want predictions extrapolated to a hypothetical target population, try setting `estimate=\"population\"."
    )
  }
  # we get this error for models with complex random effects structures in glmmTMB,
  # or when the data grid is too large
  if (
    grepl("map factor length must equal", out$message, fixed = TRUE) ||
      grepl("cannot allocate", out$message, fixed = TRUE)
  ) {
    msg <- c(
      msg,
      paste0(
        "\nYou may try using the `emmeans` backend, e.g. `estimate_means(model, by = c(",
        toString(paste0("\"", fun_args$by, "\"")),
        "), backend = \"emmeans\")`, or use `estimate_relation(model, by = c(",
        toString(paste0("\"", fun_args$by, "\"")),
        "))` instead. For contrasts or pairwise comparisons, save the output of `estimate_relation()` and pass it to `estimate_contrasts()`, e.g.\n"
      ),
      paste0(
        "out <- estimate_relation(model, by = c(",
        toString(paste0("\"", fun_args$by, "\"")),
        "))"
      ),
      paste0(
        "estimate_contrasts(out, contrast = c(",
        toString(paste0("\"", fun_args$by, "\"")),
        "))"
      )
    )
  }
  msg
}


# check that post-filtering results contain at least one row ------------------
#
# Certain combinations of `by`/`contrast` filters and `estimate = "average"`
# can produce an empty data frame when none of the requested values occur in
# the underlying data. This helper checks for zero-row results and throws a
# standardized error suggesting alternative `estimate` options or `newdata`.
#
# Arguments:
# - result: A data frame of estimated means, contrasts, or slopes.
# - prefix: Optional context-specific sentence prepended to the error message.
#
# Returns: `NULL` (throws an error if `nrow(result) == 0`).
#
#' @keywords internal
#' @noRd
.check_filter_args <- function(result, prefix = "") {
  if (nrow(result) == 0) {
    # small helper, because we have the same error message in several places
    insight::format_error(
      prefix,
      "Please check your `by` and `contrast` arguments, or try one of the following options:",
      "1. Use a different option for the `estimate` argument, e.g. `estimate = \"typical\"`.",
      "2. Use the `newdata` argument to provide a data grid of predictor values at which to evaluate predictions."
    )
  }
}


# ensure only a single focal predictor is passed to `trend` -------------------
#
# `estimate_slopes()` computes marginal effects (derivatives) with respect to
# a single numeric focal variable specified in `trend`. If a user passes a
# vector of multiple variables to `trend`, this helper retains only the first
# variable and alerts the user to pass conditioning variables via `by` instead.
#
# Arguments:
# - trend: Character vector specifying the focal trend variable(s).
# - verbose: Logical, whether to display the alert when `length(trend) > 1`.
#
# Returns: A character scalar containing the first element of `trend`.
#
#' @param trend The `trend` argument
#' @return updated `trend` variable
#' @keywords internal
#' @noRd
.check_trend_arg <- function(trend, verbose = TRUE) {
  if (length(trend) > 1) {
    if (verbose) {
      insight::format_alert(paste0(
        "More than one numeric variable was selected for slope estimation. Keeping only `",
        trend[1],
        "`. ",
        "If you want to estimate the slope of `",
        trend[1],
        "` at different values of `",
        trend[2],
        "`, use `by=\"",
        trend[2],
        "\"` instead."
      ))
    }
    trend <- trend[1]
  }
  trend
}


# Check for valid CI bounds ------------------------------------------------
#
# When predicting on the response scale (`predict = "response"`), standard errors
# and confidence intervals are typically computed via the delta method directly on
# the response scale. For non-Gaussian models with bounded support (e.g.,
# binomial, beta, or count models), this symmetrical approximation can yield
# confidence interval limits outside the theoretically valid parameter space
# (such as negative values or probabilities greater than 1).
#
# By contrast, computing intervals on the link scale and back-transforming them
# (`predict = "inverse_link"`) guarantees bounds within the valid domain, though
# point estimates may require bias adjustment (`bias_correction = TRUE`) due to
# Jensen's inequality.
#
# This helper checks whether any lower or upper confidence limits violate the
# model family's natural boundaries and alerts the user accordingly.
.check_ci_bounds <- function(means, predict, model_info, verbose) {
  if (!verbose) {
    return()
  }
  if (!identical(predict, "response")) {
    return()
  }

  # check which bounds are valid
  lower_bound_zero <- any(unlist(
    model_info[c("is_binomial", "is_count", "is_negbin", "is_beta")],
    use.names = FALSE
  ))
  upper_bound_one <- any(unlist(
    model_info[c("is_binomial", "is_beta")],
    use.names = FALSE
  ))

  # if lower/upper CI exceeds valid bounds, throw alert
  if (
    (lower_bound_zero && any(means$conf.low < 0, na.rm = TRUE)) ||
      (upper_bound_one && any(means$conf.high > 1, na.rm = TRUE))
  ) {
    insight::format_alert(
      paste0(
        "Some confidence intervals extend beyond plausible bounds. For ",
        model_info$family,
        " models, `predict = \"response\"` ",
        "generally yields less biased point estimates, but symmetric confidence intervals ",
        "can fall outside the valid parameter range. Consider using ",
        "`predict = \"inverse_link\"` with `bias_correction = TRUE` instead. ",
        "Optionally, you can also set `residual_variance` to a sensible value ",
        "to specify the assumed variance."
      ),
      "Set `verbose = FALSE` to silent this message (and other warnings)."
    )
  }
}


# validate user-specified response categories for ordinal models (emmeans) ----
#
# When filtering or contrasting specific response levels of a frequentist
# ordinal model (e.g., `by = "Sat = 'Low'"`), the requested values must match
# the outcome categories recognized by the fitted model. This helper extracts
# the model's response levels and throws an error if unknown categories are
# requested.
#
# Arguments:
# - model: The fitted ordinal model object.
# - data: Model data frame (fallback if levels are not stored on `model`).
# - response: Character string naming the ordinal response variable.
# - values: Character vector of requested response category names.
#
# Returns: `NULL` invisibly (throws an error if any `values` are invalid).
#
#' @keywords internal
#' @noRd
.check_ordinal_response_values <- function(model, data, response, values) {
  # the categories of the model, which can differ from the factor levels in
  # the data (e.g. `clm()` drops unused levels)
  response_levels <- model$y.levels
  if (is.null(response_levels)) {
    response_levels <- model$lev
  }
  if (is.null(response_levels)) {
    response_levels <- levels(droplevels(as.factor(data[[response]])))
  }
  unknown <- setdiff(values, response_levels)
  if (length(unknown)) {
    insight::format_error(paste0(
      "The response `",
      response,
      "` has no ",
      if (length(unknown) == 1) "category " else "categories ",
      datawizard::text_concatenate(unknown, enclose = "`"),
      ". Its categories are ",
      datawizard::text_concatenate(response_levels, enclose = "`"),
      "."
    ))
  }
  invisible(NULL)
}


# restrict ordinal response filtering to `predict = "prob"` (emmeans) ---------
#
# With `backend = "emmeans"`, response categories are only included as a column
# in the output when `predict = "prob"` (the default). Other ordinal modes
# (such as `"cum.prob"`, `"exc.prob"`, `"latent"`, or `"mean.class"`) operate on
# thresholds or latent/mean scales instead. This helper throws an error if the
# user attempts to filter response categories in `by` or `contrast` under any
# mode other than `"prob"`.
#
# Arguments:
# - response_filter: List with elements `$response` and `$values` extracted by
#   `.extract_ordinal_response_filter()`, or `NULL`.
# - ordinal_mode: Character string indicating the requested ordinal mode.
# - argument: Name of the argument where filtering was attempted (`"by"` or
#   `"contrast"`).
#
# Returns: `NULL` (throws an error if filtering is invalid for `ordinal_mode`).
#
#' @keywords internal
#' @noRd
.check_ordinal_response_filter <- function(
  response_filter,
  ordinal_mode,
  argument = "by"
) {
  if (!is.null(response_filter) && !identical(ordinal_mode, "prob")) {
    insight::format_error(paste0(
      "The response `",
      response_filter$response,
      "` can only be used in `",
      argument,
      "` with `predict = \"prob\"` (the default), which returns the probability of each response category."
    ))
  }
}
