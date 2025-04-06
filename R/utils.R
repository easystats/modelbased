#' @keywords internal
#' @noRd
.brms_aux_elements <- function(model = NULL) {
  out <- c(
    "sigma", "mu", "nu", "shape", "beta", "phi", "hu", "ndt", "zoi", "coi",
    "kappa", "bias", "bs", "zi", "alpha", "xi", "delta", "k"
  )
  if (inherits(model, "brmsfit")) {
    out <- unique(c(out, insight::find_auxiliary(model)))
  }
  out
}


#' @keywords internal
#' @noRd
.valid_coefficient_names <- function(model = NULL) {
  out <- c(
    "Mean", "Probability", "Difference", "Ratio", "Rate", "ZI-Probability",
    "Proportion", "Median", "MAP", "Coefficient", "Odds_ratio"
  )
  if (inherits(model, "brmsfit")) {
    dpars <- insight::find_auxiliary(model)
    if (!is.null(dpars)) {
      out <- unique(c(out, tools::toTitleCase(dpars)))
    }
  }
  out
}


#' @keywords internal
#' @noRd
.safe <- function(code, on_error = NULL) {
  if (isTRUE(getOption("easystats_errors", FALSE)) && is.null(on_error)) {
    code
  } else {
    tryCatch(code, error = function(e) on_error)
  }
}


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


#' @keywords internal
#' @noRd
.is_likert <- function(x, integer_as_numeric = 5, ...) {
  if (is.null(integer_as_numeric) || is.na(integer_as_numeric)) {
    return(FALSE)
  }
  all(.is_integer(x)) && insight::n_unique(x) <= integer_as_numeric
}
