#' Converting modelbased-objects into raw data frames
#'
#' `as.data.frame()` method for **modelbased** objects. Can be used to return
#' a "raw" data frame without attributes and with standardized column names.
#' By default, the original column names are preserved, to avoid unexpected
#' changes, but this can be changed with the `preserve_names` argument.
#'
#' @param x An object returned by the different `estimate_*()` functions.
#' @param use_responsename Logical, if `TRUE`, the response variable name is used
#' as column name for the estimate column (if available). If `FALSE` (default),
#' the column is named `"Coefficient"`.
#' @param preserve_names Logical, if `TRUE` (default), the original column names
#' are preserved. If `FALSE`, the estimate column is renamed to either the
#' response name (if `use_responsename = TRUE`) or to `"Coefficient"`.
#' @param ... Arguments passed to the `data.frame` method of `as.data.frame()`.
#' @inheritParams base::as.data.frame
#'
#' @return A data frame.
#'
#' @examplesIf insight::check_if_installed("marginaleffects", quietly = TRUE)
#' model <- lm(Petal.Length ~ Species, data = iris)
#' out <- estimate_means(model, "Species")
#'
#' # default
#' print(out)
#'
#' as.data.frame(out)
#'
#' as.data.frame(out, preserve_names = FALSE)
#'
#' as.data.frame(out, preserve_names = FALSE, use_responsename = TRUE)
#' @export
as.data.frame.estimate_contrasts <- function(
  x,
  row.names = NULL,
  optional = FALSE,
  ...,
  stringsAsFactors = FALSE,
  use_responsename = FALSE,
  preserve_names = TRUE
) {
  if (!preserve_names) {
    estimate_column <- intersect(colnames(x), .valid_coefficient_names())[1]
    response <- attr(x, "response")

    if (!is.na(estimate_column)) {
      if (use_responsename && !is.null(response)) {
        colnames(x)[colnames(x) == estimate_column] <- response
      } else {
        colnames(x)[colnames(x) == estimate_column] <- "Coefficient"
      }
    }
  }

  as.data.frame.data.frame(
    x,
    row.names = row.names,
    optional = optional,
    ...,
    stringsAsFactors = stringsAsFactors
  )
}

#' @export
as.data.frame.estimate_means <- as.data.frame.estimate_contrasts

#' @export
as.data.frame.estimate_predicted <- as.data.frame.estimate_contrasts

#' @export
as.data.frame.estimate_slopes <- as.data.frame.estimate_contrasts
