#' @export
display.estimate_contrasts <- function(object, format = "markdown", ...) {
  insight::validate_argument(format, c("md", "markdown", "html"))
  if (format == "html") {
    print_html(object, ...)
  } else {
    print_md(object, ...)
  }
}

#' @export
display.estimate_means <- display.estimate_contrasts

#' @export
display.estimate_slopes <- display.estimate_contrasts

#' @export
display.estimate_smooth <- display.estimate_contrasts

#' @export
display.estimate_predicted <- display.estimate_contrasts

#' @export
display.visualisation_matrix <- display.estimate_contrasts

#' @export
display.estimate_grouplevel <- display.estimate_contrasts
