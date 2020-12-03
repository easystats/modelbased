#' @importFrom insight export_table
#' @export
print.estimate_contrasts <- function(x, ...) {
  cat(insight::export_table(format(x)))
  invisible(x)
}

#' @export
print.estimate_means <- print.estimate_contrasts

#' @export
print.estimate_slopes <- print.estimate_contrasts

#' @export
print.estimate_smooth <- print.estimate_contrasts



# Format ------------------------------------------------------------------



#' @importFrom insight format_value format_table
#' @export
format.estimate_contrasts <- function(x, ...) {
  orig_x <- x
  if ("Size" %in% names(x)) x$Size <- ifelse(x$Size < 1, paste0(insight::format_value(x$Size * 100), "%"), "100%")
  if ("Part" %in% names(x)) x$Part <- insight::format_value(x$Part, protect_integers = TRUE)
  insight::format_table(x, ...)
}

#' @export
format.estimate_means <- format.estimate_contrasts

#' @export
format.estimate_slopes <- format.estimate_contrasts

#' @export
format.estimate_smooth <- format.estimate_contrasts
