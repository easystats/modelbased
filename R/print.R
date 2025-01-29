#' @export
print.estimate_contrasts <- function(x, full_labels = TRUE, ...) {
  # format table
  out <- format(x, ...)

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
