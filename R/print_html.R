#' @export
print_html.estimate_contrasts <- function(x, full_labels = TRUE, ...) {
  formatted_table <- format(x, format = "html")

  # remove redundant labels, for "by" variables
  formatted_table <- .remove_redundant_labels(x, formatted_table, full_labels)

  insight::export_table(
    formatted_table,
    format = "html",
    ...
  )
}


#' @export
print_html.estimate_means <- print_html.estimate_contrasts

#' @export
print_html.estimate_slopes <- print_html.estimate_contrasts

#' @export
print_html.estimate_smooth <- print_html.estimate_contrasts

#' @export
print_html.estimate_predicted <- print_html.estimate_contrasts

#' @export
print_html.visualisation_matrix <- print_html.estimate_contrasts

#' @export
print_html.estimate_grouplevel <- print_html.estimate_contrasts
