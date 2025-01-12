#' @export
print.estimate_contrasts <- function(x, ...) {
  # format table
  out <- format(x, ...)

  # remove redundant labels, for "by" variables
  ## TODO: once `by` works in estimate_contrasts(), re-enable this
  if (!inherits(x, "estimate_contrasts") && nrow(out) > 1) {
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

  cat(insight::export_table(out, ...))
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
