#' @keywords internal
#' @noRd
.brms_aux_elements <- function() {
  c(
    "sigma", "mu", "nu", "shape", "beta", "phi", "hu", "ndt", "zoi", "coi",
    "kappa", "bias", "bs", "zi", "alpha", "xi"
  )
}


#' @keywords internal
#' @noRd
.valid_coefficient_names <- function() {
  c(
    "Mean", "Probability", "Difference", "Ratio", "Rate", "ZI-Probability",
    "Proportion", "Median", "MAP", tools::toTitleCase(.brms_aux_elements())
  )
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
.is_likert <- function(x, n_uniques = 5) {
  all(.is_integer(x)) && insight::n_unique(x) <= n_uniques
}
