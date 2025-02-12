#' @docType options
#' @title Global options from the modelbased package
#' @name modelbased-options
#'
#' @section Global Options to Customize Estimation of Marginal Means:
#' Columns and table layout can be customized using `options()`:
#'
#' **For calculating marginal means**
#'
#' - `modelbased_backend`: `options(modelbased_backend = <string>)` will set a
#'   default value for the `backend` argument and can be used to set the package
#'   used by default to calculate marginal means. Can be `"marginalmeans"` or
#'   `"emmeans"`.
#'
#' - `modelbased_estimate`: `options(modelbased_estimate = <string>)` will
#'   set a default value for the `estimate` argument, which modulates the type
#'   of target population predictions refer to.
#'
#' **For printing**
#'
#' - `modelbased_select`: `options(modelbased_select = <string>)` will set a
#'   default value for the `select` argument and can be used to define a custom
#'   default layout for printing.
#'
#' - `modelbased_include_grid`: `options(modelbased_include_grid = TRUE)` will
#'   set a default value for the `include_grid` argument and can be used to
#'   include data grids in the output by default or not.
#'
#' - `modelbased_full_labels`: `options(modelbased_full_labels = FALSE)` will
#'   remove redundant (duplicated) labels from rows.
#'
#' **For plotting**
#'
#' - `modelbased_join_dots`: `options(modelbased_join_dots = <logical>)` will
#'   set a default value for the `join_dots`.
#'
#' - `modelbased_numeric_as_discrete`: `options(modelbased_numeric_as_discrete = <number>)`
#'   will set a default value for the `modelbased_numeric_as_discrete` argument.
#'   Can also be `FALSE`.
NULL
