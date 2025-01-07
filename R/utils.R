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
