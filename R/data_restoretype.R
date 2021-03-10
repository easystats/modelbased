#' Restore the type of columns according to a reference data.frame
#'
#' @param data The data.
#' @param reference_data A reference dataframe from which to find the correct
#'   column types.
#'
#' @examples
#' library(modelbased)
#'
#' x <- data.frame(
#'   Sepal.Length = c("1", "3", "2"),
#'   Species = c("setosa", "versicolor", "setosa")
#' )
#' fixed <- data_restoretype(x, reference_data = iris)
#' summary(fixed)
#' @export
data_restoretype <- function(data, reference_data = NULL) {
  data <- .data_restoretype_numerics(data)
  if (!is.null(reference_data)) {
    data <- .data_restoretype_factors(data, reference_data)
  }
  data
}


#' @keywords internal
.data_restoretype_factors <- function(data, reference_data) {
  original_factors <- reference_data[sapply(reference_data, is.factor)]

  for (i in names(data)) {
    if (i %in% names(original_factors)) {
      data[[i]] <- factor(data[[i]], levels = levels(original_factors[[i]]))
    }
  }

  data
}


#' @keywords internal
.data_restoretype_numerics <- function(data) {
  for (i in names(data)) {
    if (is.character(data[[i]])) {
      # Check if contains only numeric
      if (all(!grepl("^[[:alpha:]]+$", data[[i]], perl = TRUE))) {
        data[[i]] <- tryCatch(as.numeric(data[[i]]), error = function(e) data[[i]], warning = function(w) data[[i]])
      }
    }
  }
  data
}
