#' Printing modelbased-objects
#'
#' `print()` method for **modelbased** objects. Can be used to tweak the output
#' of tables.
#'
#' @param x An object returned by the different `estimate_*()` functions.
#' @param include_grid Logical, if `TRUE`, the data grid is included in the
#' table output. Only applies to prediction-functions like `estimate_relation()`
#' or `estimate_link()`.
#' @param full_labels Logical, if `TRUE` (default), all labels for focal terms
#' are shown. If `FALSE`, redundant (duplicated) labels are removed from rows.
#' @param ... Arguments passed to `insight::format_table()` or
#' `insight::export_table()`.
#'
#' @inheritParams insight::format_table
#'
#' @return Invisibly returns `x`.
#'
#' @section Global Options to Customize Tables when Printing:
#' Columns and table layout can be customized using `options()`:
#'
#' - `modelbased_select`: `options(modelbased_select = <string>)` will set a
#'   default value for the `select` argument and can be used to define a custom
#'   default layout for printing.
#'
#' - `modelbased_include_grid`: `options(modelbased_include_grid = TRUE)` will
#'   set a default value for the `include_grid` argument and can be used to
#'   include data grids in the output by default or not.
#'
#' - `modelbased_full_labels`: `options(modelbased_full_labels = FALSE)` will
#'   remove redundant (duplicated) labels from rows.
#'
#' @note Use `print_html()` and `print_md()` to create tables in HTML or
#' markdown format, respectively.
#'
#' @examplesIf insight::check_if_installed("marginaleffects", quietly = TRUE)
#' model <- lm(Petal.Length ~ Species, data = iris)
#' out <- estimate_means(model, "Species")
#'
#' # default
#' print(out)
#'
#' # smaller set of columns
#' print(out, select = "minimal")
#'
#' # remove redundant labels
#' data(efc, package = "modelbased")
#' efc <- datawizard::to_factor(efc, c("c161sex", "c172code", "e16sex"))
#' levels(efc$c172code) <- c("low", "mid", "high")
#' fit <- lm(neg_c_7 ~ c161sex * c172code * e16sex , data = efc)
#' out <- estimate_means(fit, c("c161sex", "c172code", "e16sex"))
#' print(out, full_labels = FALSE)
#'
#' @export
print.estimate_contrasts <- function(x,
                                     select = getOption("modelbased_select", NULL),
                                     include_grid = getOption("modelbased_include_grid", FALSE),
                                     full_labels = getOption("modelbased_full_labels", TRUE),
                                     ...) {
  # copy original
  out <- x
  # get attributes, but remove some of them - else, matching attribute fails
  attr <- attributes(x)
  attr <- attr[setdiff(names(attr), c("names", "row.names"))]

  # format table
  out <- format(out, select = select, include_grid = include_grid, ...)
  attributes(out) <- utils::modifyList(attributes(out), attr)

  # remove redundant labels, for "by" variables
  out <- .remove_redundant_labels(x, out, full_labels)

  # set alignment, left-align first and non-numerics
  align <- .align_columns(x, out)

  cat(insight::export_table(out, align = align, ...))
  invisible(x)
}

#' @export
print.estimate_means <- print.estimate_contrasts

#' @export
print.estimate_slopes <- print.estimate_contrasts

#' @export
print.summary_estimate_slopes <- print.estimate_contrasts

#' @export
print.estimate_smooth <- print.estimate_contrasts

#' @export
print.estimate_predicted <- print.estimate_contrasts

#' @export
print.visualisation_matrix <- print.estimate_contrasts

#' @export
print.estimate_grouplevel <- print.estimate_contrasts


# Helper --------------------------------

.remove_redundant_labels <- function(x, out, full_labels) {
  # remove redundant labels, for "by" variables
  if (!full_labels && nrow(out) > 1) {
    by <- attributes(x)$by
    # for estimate_means, we don't want to remove labels for first focal term
    # only for grouping variable. in `estimate_slopes()`, the first variable
    # is saved in attribute $trend, so we need to remove it only for estimate_means
    if (inherits(x, "estimate_means")) {
      by <- by[-1]
    }
    # remove repeating elements in focal term columns
    for (i in by) {
      if (i %in% colnames(out)) {
        for (j in nrow(x):2) {
          if (out[[i]][j] == out[[i]][j - 1]) out[[i]][j] <- ""
        }
      }
    }
  }
  out
}


.align_columns <- function(x, out) {
  align <- paste(c("l", rep.int("r", ncol(out) - 1)), collapse = "")
  non_numerics <- !vapply(x, is.numeric, logical(1))
  non_numeric_cols <- which(names(non_numerics) %in% colnames(out) & non_numerics)
  for (i in non_numeric_cols) {
    align <- sub(paste0("(.{", i - 1, "})."), "\\1l", align)
  }
  align
}


.format_layout <- function(x, select) {
  # possible names for estimate column
  estimate_names <- unique(
    attributes(x)$coef_name,
    c("Coefficient", "Slope", "Predicted", "Median", "Mean", "MAP", "Difference", "Ratio")
  )
  # define column name for estimates
  estimate_col <- estimate_names[which.min(estimate_names == colnames(x))]
  # focal terms
  focal_terms <- intersect(
    c("Level1", "Level2", attributes(x)$focal_terms),
    colnames(x)
  )

  # do we have shortcuts for "select"?
  select <- switch(select,
    # basic output, only focal terms, estimate, CI and p
    basic = {
      out <- c(focal_terms, estimate_col, "CI_low", "CI_high")
      if ("p" %in% colnames(x)) {
        out <- c(out, "p")
      }
      out
    },
    select
  )

  # if "select" refers to column names, return selected columns
  if (all(select %in% colnames(x))) {
    return(x[select])
  }

  # add significant stars, in case it's requested
  x$stars <- insight::format_p(x$p, stars = TRUE, stars_only = TRUE)

  x
}
