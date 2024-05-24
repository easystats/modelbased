#' Create a reference grid
#'
#' This function is an alias (another name) for the [`insight::get_datagrid()`]
#' function. Same arguments apply.
#'
#' @inheritParams insight::get_datagrid
#' @param target,at Deprecated name. Please use `by` instead.
#'
#' @return Reference grid data frame.
#'
#' @examples
#' library(modelbased)
#'
#' # Add one row to change the "mode" of Species
#' data <- rbind(iris, iris[149, ], make.row.names = FALSE)
#'
#' # Single variable is of interest; all others are "fixed"
#' visualisation_matrix(data, by = "Sepal.Length")
#' visualisation_matrix(data, by = "Sepal.Length", length = 3)
#' visualisation_matrix(data, by = "Sepal.Length", range = "ci", ci = 0.90)
#' visualisation_matrix(data, by = "Sepal.Length", factors = "mode")
#'
#' # Multiple variables are of interest, creating a combination
#' visualisation_matrix(data, by = c("Sepal.Length", "Species"), length = 3)
#' visualisation_matrix(data, by = c(1, 3), length = 3)
#' visualisation_matrix(data, by = c("Sepal.Length", "Species"), preserve_range = TRUE)
#' visualisation_matrix(data, by = c("Sepal.Length", "Species"), numerics = 0)
#' visualisation_matrix(data, by = c("Sepal.Length = 3", "Species"))
#' visualisation_matrix(data, by = c("Sepal.Length = c(3, 1)", "Species = 'setosa'"))
#'
#' # with list-style at-argument
#' visualisation_matrix(data, by = list(Sepal.Length = c(1, 3), Species = "setosa"))
#'
#' # Standardize
#' vizdata <- visualisation_matrix(data, by = "Sepal.Length")
#' standardize(vizdata)
#' @export
visualisation_matrix <- function(x, ...) {
  UseMethod("visualisation_matrix")
}

# -------------------------------------------------------------------------
# Below are visualisation_matrix functions for DataFrames
# -------------------------------------------------------------------------

#' @rdname visualisation_matrix
#' @export
visualisation_matrix.data.frame <- function(x,
                                            by = "all",
                                            target = NULL,
                                            at = NULL,
                                            factors = "reference",
                                            numerics = "mean",
                                            preserve_range = FALSE,
                                            reference = x,
                                            ...) {
  if (!is.null(target)) {
    insight::format_warning("The 'target` argument name is deprecated in favour of `by`. Please replace `target` with `by`.") # nolint
    by <- target
  }
  if (!is.null(at)) {
    insight::format_warning("The `at` argument is deprecated and will be removed in the future. Please use `by` instead.") # nolint
    by <- at
  }

  insight::get_datagrid(x,
    by = by,
    factors = factors,
    numerics = numerics,
    preserve_range = preserve_range,
    reference = reference,
    ...
  )
}


#' @rdname visualisation_matrix
#' @export
visualisation_matrix.numeric <- function(x, ...) {
  insight::get_datagrid(x, ...)
}

#' @export
visualisation_matrix.double <- visualisation_matrix.numeric

#' @rdname visualisation_matrix
#' @export
visualisation_matrix.factor <- visualisation_matrix.numeric

#' @export
visualisation_matrix.character <- visualisation_matrix.factor

#' @export
visualisation_matrix.logical <- visualisation_matrix.character

# -------------------------------------------------------------------------
# Below are visualisation_matrix functions that work on statistical models
# -------------------------------------------------------------------------

#' @export
visualisation_matrix.default <- visualisation_matrix.numeric


#' @export
visualisation_matrix.visualisation_matrix <- visualisation_matrix.numeric
