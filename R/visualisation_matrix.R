#' Create a reference grid
#'
#' This function is an alias (another name) for the [`insight::get_datagrid()`]
#' function. Same arguments apply.
#'
#' @inheritParams insight::get_datagrid
#'
#' @return Reference grid data frame.
#'
#' @examples
#' # See `?insight::get_datagrid`
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
                                            factors = "reference",
                                            numerics = "mean",
                                            preserve_range = FALSE,
                                            reference = x,
                                            ...) {
  ## TODO: remove visualisation_matrix latest for version 1.0.0
  insight::format_warning("`visualisation_matrix()` is deprecated. Please use `insight::get_datagrid()` instead.")

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
  ## TODO: remove visualisation_matrix latest for version 1.0.0
  insight::format_warning("`visualisation_matrix()` is deprecated. Please use `insight::get_datagrid()` instead.")

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
