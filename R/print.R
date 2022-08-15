#' @export
print.estimate_contrasts <- function(x, ...) {
  cat(insight::export_table(format(x), ...))
  invisible(x)
}

#' @export
print.estimate_means <- print.estimate_contrasts

#' @export
print.estimate_slopes <- print.estimate_contrasts

#' @export
print.estimate_smooth <- print.estimate_contrasts

#' @export
print.estimate_predicted <- print.estimate_contrasts

#' @export
print.visualisation_matrix <- print.estimate_contrasts

#' @export
print.estimate_grouplevel <- print.estimate_contrasts

# Format ------------------------------------------------------------------

#' @export
format.estimate_contrasts <- function(x, format = NULL, ...) {
  # don't print columns of adjusted_for variables
  adjusted_for <- attr(x, "adjusted_for", exact = TRUE)
  if (!is.null(adjusted_for) && all(adjusted_for %in% colnames(x))) {
    # remove non-focal terms from data frame
    x[adjusted_for] <- NULL
  }

  if (!is.null(format) && format %in% c("md", "markdown", "html")) {
    insight::format_table(x, ci_brackets = c("(", ")"), ...)
  } else {
    insight::format_table(x, ...)
  }
}

#' @export
format.estimate_means <- format.estimate_contrasts

#' @export
format.estimate_slopes <- format.estimate_contrasts

#' @export
format.estimate_predicted <- format.estimate_contrasts

#' @export
format.estimate_grouplevel <- format.estimate_contrasts

#' @export
format.estimate_smooth <- function(x, ...) {
  # Colnames
  if ("Size" %in% names(x)) x$Size <- ifelse(x$Size < 1, paste0(insight::format_value(x$Size * 100), "%"), "100%")
  if ("Part" %in% names(x)) x$Part <- insight::format_value(x$Part, protect_integers = TRUE)

  insight::format_table(x, ...)
}

#' @export
format.visualisation_matrix <- function(x, ...) {
  x
}
