#' @keywords internal
.estimate_response_data <- function(model, data, ...) {

  if (is.null(data)) {
    data <- insight::get_data(model)
  } else if (!is.data.frame(data)) {
    if (data == "grid") {
      data <- visualisation_matrix(model, reference = insight::get_data(model), ...)
    } else {
      stop('The `data` argument must either NULL, "grid" or another data.frame.')
    }
  }

  data[names(data) %in% insight::find_predictors(model, effects = "all", flatten = TRUE)]
}