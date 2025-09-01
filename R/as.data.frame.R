#' Converting modelbased-objects into raw data frames
#'
#' `as.data.frame()` method for **modelbased** objects. Can be used to return
#' a "raw" data frame without attributes and with standardized column names.
#'
#' @param x An object returned by the different `estimate_*()` functions.
#' @param ... Arguments passed to `as.data.frame()`.
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
#' as.data.frame(out, use_responsename = TRUE)
#' @export
as.data.frame.estimate_contrasts <- function(
  x,
  row.names = NULL,
  optional = FALSE,
  ...,
  stringsAsFactors = FALSE,
  use_responsename = FALSE
)
{
  estimate_column <- intersect(colnames(x), .valid_coefficient_names())[1]
  response <- attr(x, "response")

  if (use_responsename && !is.null(response)) {
    colnames(x)[colnames(x) == estimate_column] <- response
  } else {
    colnames(x)[colnames(x) == estimate_column] <- "Coefficient"
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
