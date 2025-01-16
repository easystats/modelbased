#' @export
print_md.estimate_contrasts <- function(x, full_labels = TRUE, ...) {
  formatted_table <- format(x, format = "markdown")

  # remove redundant labels, for "by" variables
  formatted_table <- .remove_redundant_labels(x, formatted_table, full_labels)

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
