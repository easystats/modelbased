#' @keywords internal
.restore_factor_levels <- function(predictions, data) {
  original_factors <- data[sapply(data, is.factor)]

  for (i in names(predictions)) {
    if (i %in% names(original_factors)) {
      predictions[[i]] <- factor(predictions[[i]], levels = levels(original_factors[[i]]))
    }
  }

  predictions
}

