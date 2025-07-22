#' @export
print_html.estimate_contrasts <- function(x,
                                          select = getOption("modelbased_select", NULL),
                                          include_grid = getOption("modelbased_include_grid", FALSE),
                                          full_labels = TRUE,
                                          ...) {
  # copy original
  out <- x
  # get attributes, but remove some of them - else, matching attribute fails
  attr <- attributes(x)
  attr <- attr[setdiff(names(attr), c("names", "row.names"))]

  # format table
  formatted_table <- format(out, select = select, format = "html", include_grid = include_grid, ...)
  attributes(formatted_table) <- utils::modifyList(attributes(formatted_table), attr)

  # remove redundant labels, for "by" variables
  formatted_table <- .remove_redundant_labels(x, formatted_table, full_labels)

  # set alignment, left-align first and non-numerics
  align <- .align_columns(x, formatted_table)

  # update footer
  table_footer <- attributes(formatted_table)$table_footer
  if (!is.null(table_footer)) {
    table_footer <- insight::compact_character(strsplit(table_footer, "\\n")[[1]])
    attr(formatted_table, "table_footer") <- paste(table_footer, collapse = "<br>")
  }

  insight::export_table(
    formatted_table,
    format = .check_format_backend(...),
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


# we allow exporting HTML format based on "gt" or "tinytable"
.check_format_backend <- function(...) {
  dots <- list(...)
  if (identical(dots$backend, "tt")) {
    "tt"
  } else {
    "html"
  }
}
