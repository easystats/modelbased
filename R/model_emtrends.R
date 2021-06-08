#' @rdname model_emmeans
#' @param trend A character vector indicating the name of the numeric variable
#'   for which to compute the slopes.
#'
#' @examples
#' model <- lm(Sepal.Width ~ Species * Petal.Length, data = iris)
#'
#' model_emtrends(model, levels = "Species")
#' model_emtrends(model, levels = "Species", modulate = "Petal.Length")
#'
#' model <- lm(Petal.Length ~ poly(Sepal.Width, 4), data = iris)
#' model_emtrends(model, modulate = "Sepal.Width")
#' @export
model_emtrends <- function(model,
                           trend = NULL,
                           levels = NULL,
                           modulate = NULL,
                           ...) {

  # check if available
  insight::check_if_installed("emmeans")

  # Guess arguments
  args <- .guess_emtrends_arguments(model, trend, levels, modulate, ...)

  # Modulate
  if(is.null(args$modulate)) {
    cov.reduce <- TRUE
  } else {
    cov.reduce <- list()
    data <- insight::get_data(model)

    for(i in args$modulate) {
      vizdata <- visualisation_matrix(data, target = i, ...)
      var <- attributes(vizdata)$target_specs$varname[1] # Retrieve cleaned varname
      args$levels[args$levels == i] <- var  # Overwrite the corresponding level
      values <- vizdata[[var]]

      cov.reduce[[var]] <- local({values; function(x) values})  # See #119
    }
  }

  # Run emtrends
  estimated <- emmeans::emtrends(model, args$levels, var = args$trend, cov.reduce = cov.reduce, ...)

  attr(estimated, "args") <- args
  estimated
}






# =========================================================================
# HELPERS (guess arguments) -----------------------------------------------
# =========================================================================


#' @keywords internal
.guess_emtrends_arguments <- function(model, trend = NULL, levels = NULL, modulate = NULL, ...) {
  # Gather info
  predictors <- insight::find_predictors(model, effects = "fixed", flatten = TRUE, ...)
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

  # Look if there are any factors
  if (is.null(levels)) {
    levels <- predictors[!sapply(data[predictors], is.numeric)]
  }
  if (!is.null(modulate)) {
    levels <- c(levels, modulate)
  }
  if (length(levels) == 0) {
    levels <- predictors[predictors %in% trend][1]
  }


  list(trend = trend, levels = levels, modulate = modulate)
}
