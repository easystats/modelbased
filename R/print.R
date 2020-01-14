#' @importFrom insight format_value format_table
#' @importFrom parameters parameters_table
#' @keywords internal
.print_estimate <- function(x, ...) {
  if ("Size" %in% names(x)) x$Size <- ifelse(x$Size < 1, paste0(insight::format_value(x$Size * 100), "%"), "100%")
  if ("Part" %in% names(x)) x$Part <- insight::format_value(x$Part, protect_integers = TRUE)
  formatted_table <- parameters::parameters_table(x, ...)
  cat(insight::format_table(formatted_table))
}

#' @export
print.estimate_contrasts <- .print_estimate

#' @export
print.estimate_means <- .print_estimate

#' @export
print.estimate_slopes <- .print_estimate

#' @export
print.estimate_smooth <- .print_estimate
