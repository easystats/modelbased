#' @export
print_md.estimate_contrasts <- function(x,
                                        select = getOption("modelbased_select", NULL),
                                        include_grid = getOption("modelbased_include_grid", FALSE),
                                        full_labels = TRUE,
                                        ...) {
  # copy original
  out <- x
  # get attributes, but remove some of them - else, matching attribute fails
  attr <- attributes(x)
  attr <- attr[setdiff(names(attr), c("names", "row.names"))]

  # select columns to print
  if (!is.null(select)) {
    out <- .format_layout(out, select)
    attributes(out) <- utils::modifyList(attributes(out), attr)
  }

  formatted_table <- format(out, format = "markdown", include_grid = include_grid, ...)

  # remove redundant labels, for "by" variables
  formatted_table <- .remove_redundant_labels(x, formatted_table, full_labels)

  # set alignment, left-align first and non-numerics
  align <- .align_columns(x, formatted_table)

  insight::export_table(
    formatted_table,
    format = "markdown",
    align = align,
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
