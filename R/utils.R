#' @keywords internal
#' @noRd
.to_numeric <- function(x) {
  tryCatch(as.numeric(as.character(x)),
    error = function(e) x,
    warning = function(w) x
  )
}
