# list supported distributional / auxiliary parameters for brms models --------
#
# Distributional regression models (such as `brmsfit` objects) can predict
# auxiliary parameters beyond the location parameter (e.g., `"sigma"`, `"zi"`,
# `"hu"`, `"kappa"`). This helper combines a static set of common distributional
# parameter names with any model-specific auxiliary parameters detected via
# `insight::find_auxiliary()`.
#
# It is used to route `predict = "<dpar>"` calls to the `dpar` argument in
# `{marginaleffects}` or `{emmeans}` and to recognize valid output column names.
#
# Arguments:
# - model: Optional fitted model object passed to `insight::find_auxiliary()`.
#
# Returns: A character vector of unique auxiliary parameter names.
#
#' @keywords internal
#' @noRd
.brms_aux_elements <- function(model = NULL) {
  out <- c(
    "sigma",
    "mu",
    "nu",
    "shape",
    "beta",
    "phi",
    "hu",
    "ndt",
    "zoi",
    "coi",
    "kappa",
    "bias",
    "bs",
    "zi",
    "alpha",
    "xi",
    "delta",
    "k"
  )
  unique(c(out, insight::find_auxiliary(model, verbose = FALSE)))
}


# list valid estimate column names across modelbased outputs ------------------
#
# Depending on the model family, prediction scale, or estimation function
# (`estimate_means()`, `estimate_contrasts()`, `estimate_slopes()`), the primary
# point-estimate column in the formatted output takes different names (such as
# `"Mean"`, `"Probability"`, `"Difference"`, `"Ratio"`, or `"Slope"`).
#
# This helper returns all recognized coefficient column names, dynamically
# appending title-cased auxiliary parameter names (e.g., `"Sigma"`) if the
# model includes distributional parameters. Used to populate the `"coef_name"`
# attribute for downstream formatting and plotting methods.
#
# Arguments:
# - model: Optional fitted model object used to query distributional
#   parameters via `insight::find_auxiliary()`.
#
# Returns: A character vector of valid estimate column names.
#
#' @keywords internal
#' @noRd
.valid_coefficient_names <- function(model = NULL) {
  out <- c(
    "Mean",
    "Probability",
    "Difference",
    "Ratio",
    "Rate",
    "ZI-Probability",
    "Proportion",
    "Median",
    "MAP",
    "Coefficient",
    "Odds_ratio",
    "Latent",
    "Linear_predictor",
    "Mean_class"
  )
  dpars <- insight::find_auxiliary(model, verbose = FALSE)
  if (!is.null(dpars)) {
    out <- unique(c(out, tools::toTitleCase(dpars)))
  }
  out
}


# validate `estimate` argument and resolve aliases ----------------------------
#
# Resolves the marginalization method requested via the `estimate` argument.
# When `estimate = NULL`, it falls back to the global option
# `getOption("modelbased_estimate", "typical")`. It validates the input against
# all supported options and normalizes the alias `"counterfactual"` to its
# canonical name `"population"`.
#
# Arguments:
# - estimate: Character string specifying the marginalization method, or `NULL`.
#
# Returns: A validated character scalar (`"typical"`, `"population"`,
#   `"specific"`, or `"average"`).
#
#' @keywords internal
#' @noRd
.validate_estimate_arg <- function(estimate) {
  # set default, if not provided
  if (is.null(estimate)) {
    estimate <- getOption("modelbased_estimate", "typical")
  }
  # validate argument
  estimate <- insight::validate_argument(
    estimate,
    c("typical", "population", "specific", "average", "counterfactual")
  )
  # handle aliases
  if (estimate == "counterfactual") {
    estimate <- "population"
  }
  # return result
  estimate
}


# safely evaluate expressions with optional error fallback --------------------
#
# Evaluates an R expression inside `tryCatch()`, returning `on_error` (default
# `NULL`) if an error occurs and optionally suppressing warnings. When the
# global debugging option `options(easystats_errors = TRUE)` is active and no
# explicit `on_error` fallback is given, errors are allowed to surface
# unintercepted for easier troubleshooting.
#
# Arguments:
# - code: An R expression to evaluate.
# - on_error: Value returned if `code` throws an error (defaults to `NULL`).
# - quietly: Logical, whether to suppress warnings during evaluation.
#
# Returns: The result of evaluating `code`, or `on_error` upon failure.
#
#' @keywords internal
#' @noRd
.safe <- function(code, on_error = NULL, quietly = FALSE) {
  if (isTRUE(getOption("easystats_errors", FALSE)) && is.null(on_error)) {
    code
  } else if (quietly) {
    suppressWarnings(tryCatch(code, error = function(e) on_error))
  } else {
    tryCatch(code, error = function(e) on_error)
  }
}


# check whether numeric values are whole numbers (integers) -------------------
#
# Tests element-wise whether values in `x` represent exact integers (including
# whole numbers stored as `double`, such as `c(1, 2, 3)`), returning `FALSE`
# for infinite values or non-numeric types that trigger errors.
#
# Arguments:
# - x: A vector to test.
#
# Returns: A logical vector (or scalar `FALSE` on error) indicating whether each
#   element is a finite whole number.
#
#' @keywords internal
#' @noRd
.is_integer <- function(x) {
  tryCatch(
    expr = {
      ifelse(is.infinite(x), FALSE, x %% 1 == 0)
    },
    warning = function(w) {
      is.integer(x)
    },
    error = function(e) {
      FALSE
    }
  )
}


# detect discrete / Likert-style integer predictors ---------------------------
#
# Determines whether a numeric predictor `x` should be treated as a discrete
# integer variable (e.g., an ordinal or Likert-scale item) rather than a
# continuous variable—for example, when deciding between pairwise value
# contrasts and slope contrasts in `estimate_contrasts()`.
#
# A variable is classified as discrete/Likert if all of its values are whole
# numbers (`.is_integer(x)`) and the number of unique values does not exceed
# `integer_as_continuous` (or `getOption("modelbased_integer")`). When a
# variable with 3 to `integer_as_continuous` unique values is auto-detected
# using the default threshold, an informative alert is shown to the user.
#
# Arguments:
# - x: Numeric predictor vector to inspect.
# - integer_as_continuous: Maximum number of unique integer values to treat as
#   discrete (default `5`), or `TRUE`/`NA`/`NULL` to disable the check and
#   always treat `x` as continuous.
# - verbose: Logical, whether to display an alert when a Likert-scale variable
#   is detected using the default threshold.
# - ...: Additional arguments (currently unused).
#
# Returns: Logical scalar (`TRUE` if `x` should be treated as discrete,
#   `FALSE` otherwise).
#
#' @keywords internal
#' @noRd
.is_likert <- function(x, integer_as_continuous = 5, verbose = TRUE, ...) {
  # check if argument is missing or not - message only shown when missing
  missing_default <- missing(integer_as_continuous)

  # check for global option
  if (!is.null(getOption("modelbased_integer"))) {
    integer_as_continuous <- getOption("modelbased_integer")
  }

  # no need to check if check is disabled
  if (
    is.null(integer_as_continuous) ||
      is.na(integer_as_continuous) ||
      isTRUE(integer_as_continuous)
  ) {
    return(FALSE)
  }

  # integer-values, and no more than `integer_as_continuous` unique values?
  is_likert <- all(.is_integer(x)) && insight::n_unique(x) <= integer_as_continuous

  # tell user, this handling might not be desired - but only if we have
  # more than 2 unique values, otherwise it's assumed to be a binary variable
  if (is_likert && verbose && missing_default && insight::n_unique(x) > 2) {
    insight::format_alert(
      "Numeric variable appears to be ordinal or Likert-scale (integer values, no more than 5 unique values) and is treated as discrete variable. Set `integer_as_continuous = TRUE` to disable this check and always treat numeric variables as continuous."
    )
  }
  is_likert
}
