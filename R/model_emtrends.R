#' @rdname model_emmeans
#' @param trend A character vector indicating the name of the numeric variable
#'   for which to compute the slopes.
#'
#' @examples
#' model <- lm(Sepal.Width ~ Species * Petal.Length, data = iris)
#' model_emtrends(model)
#'
#' @export
model_emtrends <- function(model,
                           trend = NULL,
                           levels = NULL,
                           ci = 0.95,
                           ...) {

  # check if available
  insight::check_if_installed("emmeans")

  # Guess arguments
  args <- .guess_emtrends_arguments(model, trend, levels, ...)

  # Run emtrends
  estimated <- emmeans::emtrends(model, args$levels, var = args$trend, ...)

  estimated
}






# =========================================================================
# HELPERS (guess arguments) -----------------------------------------------
# =========================================================================


#' @keywords internal
.guess_emtrends_arguments <- function(model, trend = NULL, levels = NULL, ...) {
  # Gather info
  predictors <- insight::find_predictors(model, flatten = TRUE, ...)
  data <- insight::get_data(model)

  # Guess arguments
  if (is.null(trend)) {
    trend <- predictors[sapply(data[predictors], is.numeric)][1]
    if (!length(trend) || is.na(trend)) {
      stop("Model contains no numeric predictor. Cannot estimate trend.")
    }
    message('No numeric variable was specified for slope estimation. Selecting `trend = "', trend, '"`.')
  }
  if (length(trend) > 1) {
    message("More than one numeric variable was selected for slope estimation. Keeping only ", trend[1], ".")
    trend <- trend[1]
  }

  if (is.null(levels)) {
    levels <- predictors[!predictors %in% trend]
  }

  if (length(levels) == 0) {
    stop("No suitable factor levels detected over which to estimate slopes.")
  }

  list(trend = trend, levels = levels)
}
