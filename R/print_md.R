#' @export
print_md.estimate_contrasts <- function(x, ...) {
  formatted_table <- format(x, format = "markdown")
  insight::export_table(
    formatted_table,
    format = "markdown",
    ...
  )
}


#' @export
print_md.estimate_means <- print_md.estimate_contrasts

#' @export
print_md.estimate_slopes <- print_md.estimate_contrasts

#' @export
print_md.estimate_smooth <- print_md.estimate_contrasts

#' @export
print_md.estimate_predicted <- print_md.estimate_contrasts

#' @export
print_md.visualisation_matrix <- print_md.estimate_contrasts

#' @export
print_md.estimate_grouplevel <- print_md.estimate_contrasts
