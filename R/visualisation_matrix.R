#' Create a reference grid
#'
#' This function is an alias (another name) for the [`insight::get_datagrid()`]
#' function. Same arguments apply.
#'
#' @inheritParams insight::get_datagrid
#' @param target Deprecated name. Please use `at` instead.
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

# -------------------------------------------------------------------------
# Below are visualisation_matrix functions for DataFrames
# -------------------------------------------------------------------------

#' @rdname visualisation_matrix
#' @export
visualisation_matrix.data.frame <- function(x,
                                            at = "all",
                                            target = NULL,
                                            factors = "reference",
                                            numerics = "mean",
                                            preserve_range = FALSE,
                                            reference = x,
                                            ...) {
  if (!is.null(target)) {
    insight::format_warning("The 'target' argument name is deprecated in favour of 'at'. Please replace 'target' with 'at'.")
    at <- target
  }

  insight::get_datagrid(x,
    at = at,
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
