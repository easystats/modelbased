#' @importFrom insight get_data find_predictors model_info find_random
#' @keywords internal
.estimate_response_init <- function(model, data, transform, include_smooth, include_random, length, preserve_range, predict, ...) {
  # Data
  if(include_smooth==FALSE) include_smooth <- "fixed"  # Otherwise cannot make predictions

  if (is.null(data)) {
    data <- insight::get_data(model)
  } else if (!is.data.frame(data)) {
    if (data == "grid") {
      data <- visualisation_matrix(model, include_smooth=include_smooth, include_random = include_random, length = length, preserve_range = preserve_range, reference = insight::get_data(model), ...)
    } else {
      stop('The `data` argument must either NULL, "grid" or another data.frame.')
    }
  }

  data <- data[names(data) %in% insight::find_predictors(model, effects = "all", flatten = TRUE)]

  # Deal with random
  if (insight::model_info(model)$is_mixed & include_random) {
    if (!insight::find_random(model, flatten = TRUE) %in% names(data)) {
      warning("Could not find random effects in data. Will turn `random` to FALSE.")
      include_random <- FALSE
    }
  }
  if (isTRUE(include_random)) {
    re.form <- NULL
  } else {
    re.form <- NA
  }

  # Deal with transform
  if (predict == "link") {
    if (insight::model_info(model)$is_bayesian) transform <- ifelse(transform == "response", TRUE, FALSE)
    interval <- "confidence"
  } else {
    interval <- "prediction"
  }


  list(data = data, re.form = re.form, transform = transform, interval = interval)
}
