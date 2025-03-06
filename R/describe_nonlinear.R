#' Describe the smooth term (for GAMs) or non-linear predictors
#'
#' This function summarises the smooth term trend in terms of linear segments.
#' Using the approximate derivative, it separates a non-linear vector into
#' quasi-linear segments (in which the trend is either positive or negative).
#' Each of this segment its characterized by its beginning, end, size (in
#' proportion, relative to the total size) trend (the linear regression
#' coefficient) and linearity (the R2 of the linear regression).
#'
#' @param data The data containing the link, as for instance obtained by
#'   [estimate_relation()].
#' @param x,y The name of the responses variable (`y`) predicting variable
#'   (`x`).
#' @param ... Other arguments to be passed to or from.
#'
#' @examplesIf insight::check_if_installed("performance", quietly = TRUE)
#' # Create data
#' data <- data.frame(x = rnorm(200))
#' data$y <- data$x^2 + rnorm(200, 0, 0.5)
#'
#' model <<- lm(y ~ poly(x, 2), data = data)
#' link_data <- estimate_relation(model, length = 100)
#'
#' describe_nonlinear(link_data, x = "x")
#' @return A data frame of linear description of non-linear terms.
#' @export
describe_nonlinear <- function(data, ...) {
  UseMethod("describe_nonlinear")
}


#' @export
describe_nonlinear.estimate_predicted <- function(data,
                                                  x = NULL,
                                                  y = "Predicted",
                                                  ...) {
  describe_nonlinear.data.frame(data, x = x, y = y, ...)
}


#' @export
describe_nonlinear.numeric <- function(data, x = NULL, ...) {
  if (is.null(x)) {
    x <- seq_len(length(data))
  }

  describe_nonlinear(data.frame(x = x, y = data), x = "x", y = "y")
}


#' @rdname describe_nonlinear
#' @export
describe_nonlinear.data.frame <- function(data, x = NULL, y = NULL, ...) {
  # Input validation
  if (is.null(x) || !x %in% names(data)) {
    insight::format_error("The name of the predictor variable (`x`) must be correctly supplied.")
  }
  if (is.null(y) || !y %in% names(data)) {
    insight::format_error("The name of the response variable (`y`) must be correctly supplied.")
  }

  # Verify that the x-axis is sorted
  if (is.unsorted(data[[x]])) data <- data[order(data[[x]]), ]

  # Find inversions
  parts <- .describe_nonlinear_parts(y = data[[y]], x = data[[x]])

  # Prepare output
  class(parts) <- c("estimate_smooth", class(parts))
  parts
}

#' @rdname describe_nonlinear
#' @export
estimate_smooth <- describe_nonlinear


# Utils -------------------------------------------------------------------

#' @keywords internal
.describe_nonlinear_parts <- function(y, x) {
  insight::check_if_installed("performance")

  n <- length(y)

  # 1. Cut y into different parts delimited by inversion points
  inversions <- find_inversions(y)

  # Sanitize inversion points: add beginning and end
  if (all(is.na(inversions))) {
    inversions <- c(1, n)
  } else {
    if (inversions[1] != 1) {
      inversions <- c(1, inversions)
    }

    if (utils::tail(inversions, 1) < n) {
      inversions <- c(inversions, n)
    }
  }

  out <- data.frame()
  # Iterate through each identified segment of the non-linear trend.
  # Each segment is defined by two consecutive inversion points.
  for (i in 1:(length(inversions) - 1)) {
    # Determine the start and end indices of the current segment.
    # The start index is the current inversion point.
    idx_start <- round(inversions[i])
    # The end index is the next inversion point.
    idx_end <- round(inversions[i + 1])

    # Create a data frame to store the characteristics of the current segment.
    segment <- data.frame(
      # The x-value at the start of the segment.
      Start = x[idx_start],
      # The x-value at the end of the segment.
      End = x[idx_end],
      # The proportional length of the segment relative to the entire x-axis.
      Length = (inversions[i + 1] - inversions[i]) / n,
      # The change in the y-value across the segment (end - start).
      Change = y[idx_end] - y[idx_start],
      stringsAsFactors = FALSE
    )
    # Calculate the slope of the segment (change in y divided by change in x).
    segment$Slope <- segment$Change / (segment$End - segment$Start)

    # Check the linearity of the segment by calculating the R-squared value
    # of a linear model fitted to the y and x values within the segment.
    # This provides a measure of how well a straight line fits the data within the segment.
    segment$R2 <- performance::r2(stats::lm(y ~ x))$R2

    # Append the current segment's characteristics to the output data frame.
    out <- rbind(out, segment)
  }

  out
}
