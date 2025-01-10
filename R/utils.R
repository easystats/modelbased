#' @keywords internal
#' @noRd
.to_numeric <- function(x) {
  tryCatch(as.numeric(as.character(x)),
    error = function(e) x,
    warning = function(w) x
  )
}


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
    tools::toTitleCase(.brms_aux_elements())
  )
}
