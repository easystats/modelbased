#' @rdname estimate_grouplevel
#' @param x The output of `estimate_grouplevel()`.
#' @param indices A list containing the indices (i.e., which columns) to
#'   extract (e.g., "Coefficient", "Median").
#' @param group A list containing the random factor to select (e.g., "Participant") if
#'   the model was `y ~ x + (1|Participant)`.
#' @export
reshape_grouplevel <- function(x, indices = "all", group = "all", ...) {
  UseMethod("reshape_grouplevel")
}


#' @export
reshape_grouplevel.estimate_grouplevel <- function(x, indices = "all", group = NULL, ...) {
  # Find indices
  if (any(indices == "all")) {
    indices <- names(x)[!names(x) %in% c("Group", "Level", "Parameter", "CI")]
  }

  # Accommodate Bayesian
  if ("Coefficient" %in% indices) indices <- c(indices, "Median", "Mean", "MAP")
  if ("SE" %in% indices) indices <- c(indices, "SD", "MAD")
  indices <- names(x)[names(x) %in% unique(indices)]

  # Random parameters
  if (all(is.null(group))) group <- unique(x$Group)
  if (length (group) > 1) {
    warning(paste0("Multiple group are present (", paste(group, collapse = ", "),
                   "). Selecting the first (group = ", group[1], ")."))
    group <- group[1]
  }
  x <- x[x$Group %in% group, ]

  # Create a new column for the parameter name
  x$.param <- x$Parameter
  if("Component" %in% names(x)) x$.param <- paste0(x$Component, "_", x$.param)
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
