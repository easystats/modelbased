#' Create a reference grid
#'
#' Create a reference matrix, useful for visualisation, with evenly spread and combined values. `data_matrix()` is an alternative name for `visualisation_matrix()`.
#'
#' @param x An object from which to construct the reference grid.
#' @param at,target Can be "all" or list of characters indicating columns of interest. Can also contain assignments (as named list, e.g. `at = list(c(Sepal.Length = c(2, 4), Species = "setosa"))`, or as string, e.g. `at = "Sepal.Length = 2"` or `at = c("Sepal.Length = 2", "Species = 'setosa'")` - note the usage of single and double quotes to assign strings within strings). The remaining variables will be fixed. (`target` is the deprecated name of that argument).
#' @param length Length of numeric "at" variables.
#' @param range Can be one of `c("range", "iqr", "ci", "hdi", "eti")`. If `"range"` (default), will use the min and max of the original vector as end-points. If any other interval, will spread within the range (the default CI width is `95%` but this can be changed by setting something else, e.g., `ci = 0.90`). See [IQR()] and [bayestestR::ci()].
#' @param factors Type of summary for factors. Can be "reference" (set at the reference level), "mode" (set at the most common level) or "all" to keep all levels.
#' @param numerics Type of summary for numeric values. Can be "all" (will duplicate the grid for all unique values), any function ("mean", "median", ...) or a value (e.g., `numerics = 0`).
#' @param preserve_range In the case of combinations between numeric variables and factors, setting `preserve_range = TRUE` will drop the observations where the value of the numeric variable is originally not present in the range of its factor level. This leads to an unbalanced grid. Also, if you want the minimum and the maximum to closely match the actual ranges, you should increase the `length` argument.
#' @param ... Arguments passed to or from other methods (for instance, `length` or `range` to control the spread of numeric variables.).
#' @inheritParams effectsize::format_standardize
#' @inheritParams estimate_response
#'
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
#' visualisation_matrix(data, at = "Sepal.Length")
#' visualisation_matrix(data, at = "Sepal.Length", length = 3)
#' visualisation_matrix(data, at = "Sepal.Length", range = "ci", ci = 0.90)
#' visualisation_matrix(data, at = "Sepal.Length", factors = "mode")
#'
#' # Multiple variables are of interest, creating a combination
#' visualisation_matrix(data, at = c("Sepal.Length", "Species"), length = 3)
#' visualisation_matrix(data, at = c(1, 3), length = 3)
#' visualisation_matrix(data, at = c("Sepal.Length", "Species"), preserve_range = TRUE)
#' visualisation_matrix(data, at = c("Sepal.Length", "Species"), numerics = 0)
#' visualisation_matrix(data, at = c("Sepal.Length = 3", "Species"))
#' visualisation_matrix(data, at = c("Sepal.Length = c(3, 1)", "Species = 'setosa'"))
#'
#' # with list-style at-argument
#' visualisation_matrix(data, at = list(Sepal.Length = c(1, 3), Species = "setosa"))
#'
#' # Standardize
#' vizdata <- visualisation_matrix(data, at = "Sepal.Length")
#' standardize(vizdata)
#' @export
visualisation_matrix <- function(x, ...) {
  UseMethod("visualisation_matrix")
}

#' @rdname visualisation_matrix
#' @export
data_matrix <- visualisation_matrix


# -------------------------------------------------------------------------
# Below are visualisation_matrix functions for DataFrames
# -------------------------------------------------------------------------

#' @rdname visualisation_matrix
#' @export
visualisation_matrix.data.frame <- function(x, at = "all", target = NULL, factors = "reference", numerics = "mean", preserve_range = FALSE, reference = x, ...) {
  if (!is.null(target)) {
    warning("The 'target' argument name is deprecated in favour of 'at'. Please replace 'target' with 'at'.")
    at <- target
  }
  insight::get_datagrid(x,
                        at = at,
                        factors = factors,
                        numerics = numerics,
                        preserve_range = preserve_range,
                        reference = reference,
                        ...)
}


#' @rdname visualisation_matrix
#' @export
visualisation_matrix.numeric <- function(x, length = 10, range = "range", ...) {
  insight::get_datagrid(x, length = length, range = range, ...)
}

#' @export
visualisation_matrix.double <- visualisation_matrix.numeric

#' @rdname visualisation_matrix
#' @export
visualisation_matrix.factor <- function(x, ...) {
  insight::get_datagrid(x, ...)
}

#' @export
visualisation_matrix.character <- visualisation_matrix.factor

#' @export
visualisation_matrix.logical <- visualisation_matrix.character


