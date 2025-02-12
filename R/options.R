#' @title Global options from the modelbased package
#' @name modelbased-options
#'
#' @section Global options to set defaults for function arguments:
#'
#' **For calculating marginal means**
#'
#' - `options(modelbased_backend = <string>)` will set a default value for the
#'   `backend` argument and can be used to set the package used by default to
#'   calculate marginal means. Can be `"marginaleffects"` or `"emmeans"`.
#'
#' - `options(modelbased_estimate = <string>)` will set a default value for the
#'   `estimate` argument, which modulates the type of target population
#'   predictions refer to.
#'
#' **For printing**
#'
#' - `options(modelbased_select = <string>)` will set a default value for the
#'   `select` argument and can be used to define a custom default layout for
#'   printing.
#'
#' - `options(modelbased_include_grid = TRUE)` will set a default value for the
#'   `include_grid` argument and can be used to include data grids in the output
#'   by default or not.
#'
#' - `options(modelbased_full_labels = FALSE)` will remove redundant
#'   (duplicated) labels from rows.
#'
#' **For plotting**
#'
#' - `options(modelbased_join_dots = <logical>)` will set a default value for
#'   the `join_dots`.
#'
#' - `options(modelbased_numeric_as_discrete = <number>)` will set a default
#'   value for the `modelbased_numeric_as_discrete` argument. Can also be
#'   `FALSE`.
NULL
