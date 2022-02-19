#' @importFrom insight get_datagrid
#' @export
insight::get_datagrid


#' Create a reference grid
#'
#' Create a reference matrix, useful for visualisation, with evenly spread and combined values. `visualisation_matrix()` is an alias for `insight::get_datagrid()`.
#'
#' @inheritParams insight::get_datagrid
#'
#' @return Reference grid data frame.
#'
#' @seealso `visualisation_matrix()` is an alias for `insight::get_datagrid()`. See `?insight::get_datagrid` for an overview of options.
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
visualisation_matrix <- insight::get_datagrid
