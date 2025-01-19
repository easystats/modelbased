#' @export
print_html.estimate_contrasts <- function(x, full_labels = TRUE, ...) {
  formatted_table <- format(x, format = "html")

  # remove redundant labels, for "by" variables
  formatted_table <- .remove_redundant_labels(x, formatted_table, full_labels)

  # set alignment, left-align first and non-numerics
  align <- .align_columns(x, formatted_table)

  # update footer
  table_footer <- attributes(formatted_table)$table_footer
  if (!is.null(table_footer)) {
    table_footer <- insight::compact_character(strsplit(table_footer, "\\n")[[1]])
    attr(formatted_table, "table_footer") <- paste(table_footer, collapse = "; ")
  }

  insight::export_table(
    formatted_table,
    format = "html",
    align = align,
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
