#' @rdname estimate_grouplevel
#' @param x The output of `estimate_grouplevel()`.
#' @param indices A character vector containing the indices (i.e., which
#'   columns) to extract (e.g., "Coefficient", "Median").
#' @param group The name of the random factor to select as string value (e.g.,
#'   `"Participant"`, if the model was `y ~ x + (1|Participant)`.
#'
#' @export
reshape_grouplevel <- function(x, ...) {
  UseMethod("reshape_grouplevel")
}

#' @export
reshape_grouplevel.default <- function(x, ...) {
  insight::format_error(paste0(
    "`reshape_grouplevel()` not implemented yet for objects of class `",
    class(x)[1],
    "`."
  ))
}

#' @rdname estimate_grouplevel
#' @export
reshape_grouplevel.estimate_grouplevel <- function(x, indices = "all", group = NULL, ...) {
  # Find indices
  if (any(indices == "all")) {
    indices <- names(x)[!names(x) %in% c("Group", "Level", "Parameter", "CI")]
  }

  # Accommodate Bayesian
  if ("Coefficient" %in% indices) {
    indices <- c(indices, "Median", "Mean", "MAP")
  }
  if ("SE" %in% indices) {
    indices <- c(indices, "SD", "MAD")
  }
  indices <- intersect(colnames(x), unique(indices))

  # Random parameters
  if (is.null(group)) {
    group <- unique(x$Group)
  }

  if (length(group) > 1) {
    insight::format_alert(paste0(
      "Multiple groups are present (", toString(group),
      "). Selecting the first (", group[1], ")."
    ))
    group <- group[1]
  }
  x <- x[x$Group %in% group, ]

  # Create a new column for the parameter name
  x$.param <- x$Parameter
  if ("Component" %in% names(x)) {
    x$.param <- paste0(x$Component, "_", x$.param)
  }
  x$.param <- gsub("conditional_", "", x$.param)

  # Reshape
  data_wide <- datawizard::data_to_wide(
    x,
    id_cols = "Level",
    values_from = indices,
    names_from = ".param",
    names_sep = "_"
  )

  # Rename level to group
  names(data_wide)[names(data_wide) == "Level"] <- group

  class(data_wide) <- c("reshape_grouplevel", class(data_wide))
  data_wide
}

#' @export
reshape_grouplevel.data.frame <- reshape_grouplevel.estimate_grouplevel
