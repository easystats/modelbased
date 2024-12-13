#' @rdname get_emmeans
#'
#' @param trend A character indicating the name of the variable
#'   for which to compute the slopes.
#'
#' @examples
#' if (require("emmeans")) {
#'   model <- lm(Sepal.Width ~ Species * Petal.Length, data = iris)
#'
#'   get_emtrends(model)
#'   get_emtrends(model, by = "Species")
#'   get_emtrends(model, by = "Petal.Length")
#'   get_emtrends(model, by = c("Species", "Petal.Length"))
#'
#'   model <- lm(Petal.Length ~ poly(Sepal.Width, 4), data = iris)
#'   get_emtrends(model)
#'   get_emtrends(model, by = "Sepal.Width")
#' }
#' @export
get_emtrends <- function(model,
                         trend = NULL,
                         by = NULL,
                         fixed = NULL,
                         ...) {
  # check if available
  insight::check_if_installed("emmeans")

  # Guess arguments
  my_args <- .guess_emtrends_arguments(model, trend, by, fixed, ...)

  # Run emtrends
  estimated <- emmeans::emtrends(
    model,
    specs = my_args$emmeans_specs,
    var = my_args$trend,
    at = my_args$emmeans_at,
    ...
  )

  attr(estimated, "trend") <- my_args$trend
  attr(estimated, "at") <- my_args$by
  attr(estimated, "by") <- my_args$by
  attr(estimated, "fixed") <- my_args$fixed
  estimated
}

#' @rdname get_emmeans
#' @export
model_emtrends <- get_emtrends

# =========================================================================
# HELPERS (guess arguments) -----------------------------------------------
# =========================================================================

#' @keywords internal
.guess_emtrends_arguments <- function(model,
                                      trend = NULL,
                                      by = NULL,
                                      fixed = NULL,
                                      ...) {
  # Gather info
  predictors <- insight::find_predictors(model, effects = "fixed", flatten = TRUE, ...)
  model_data <- insight::get_data(model)

  # Guess arguments
  if (is.null(trend)) {
    trend <- predictors[sapply(model_data[predictors], is.numeric)][1]
    if (!length(trend) || is.na(trend)) {
      insight::format_error("Model contains no numeric predictor. Please specify `trend`.")
    }
    insight::format_alert('No numeric variable was specified for slope estimation. Selecting `trend = "', trend, '"`.')
  }
  if (length(trend) > 1) {
    insight::format_alert("More than one numeric variable was selected for slope estimation. Keeping only ", trend[1], ".")
    trend <- trend[1]
  }

  my_args <- list(trend = trend, by = by, fixed = fixed)
  .format_emmeans_arguments(model, args = my_args, data = model_data, ...)
}
