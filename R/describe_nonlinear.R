#' Describe the smooth term (for GAMs) or non-linear predictors
#'
#' This function summarises the smooth term trend in terms of linear segments.
#' Using the approximative derivative, it separates a non-linear vector into
#' quasi-linear segments (in which the trend is either positive or negative).
#' Each of this segment its characterized by its beginning, end, size (in
#' proportion, relative to the total size) trend (the linear regression
#' coefficient) and linearity (the R2 of the linear regression).
#'
#' @param data The data containing the link, as for instance obtained by \code{\link{estimate_relation}}.
#' @param x,y The name of the responses variable (\code{y}) predicting variable (\code{x}).
#' @param ... Other arguments to be passed to or from.
#'
#' @examples
#' library(modelbased)
#'
#' # Create data
#' data <- data.frame(x = rnorm(200))
#' data$y <- data$x^2 + rnorm(200, 0, 0.5)
#'
#' model <- lm(y ~ poly(x, 2), data = data)
#' link_data <- estimate_relation(model, length = 100)
#'
#' describe_nonlinear(link_data, x = "x")
#'
#' @return A dataframe of linear description of non-linear terms.
#' @importFrom insight find_predictors get_data find_random
#' @importFrom stats mad median sd setNames predict loess
#' @export
describe_nonlinear <- function(data, x = NULL, y = NULL, ...) {
  UseMethod("describe_nonlinear")
}




#' @export
describe_nonlinear.estimate_predicted <- function(data, x = NULL, y = "Predicted", ...) {
  describe_nonlinear.data.frame(data, x = x, y = y, ...)
}



#' @export
describe_nonlinear.data.frame <- function(data, x = NULL, y = NULL, ...) {
  # Sanity check
  if(is.null(x) || !x %in% names(data)) {
    stop("The name of the predictor variable (`x`) must be correctly supplied.")
  }
  if(is.null(y) || !y %in% names(data)) {
    stop("The name of the response variable (`y`) must be correctly supplied.")
  }

  # Verify that the x-axis is sorted
  if(is.unsorted(data[[x]])) data <- data[order(data[[x]]), ]

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
  for(i in 1:(length(inversions) - 1)){

    idx_start <- round(inversions[i])
    idx_end <- round(inversions[i + 1])

    segment <- data.frame(Start = x[idx_start],
                          End = x[idx_end],
                          Length = (inversions[i + 1] - inversions[i]) / n,
                          Change = y[idx_end] - y[idx_start])
    segment$Slope <- segment$Change / (segment$End - segment$Start)

    segment$R2 <- .check_linearity(y, x)

    out <- rbind(out, segment)
  }
  out
}








#' @importFrom stats lm
#' @keywords internal
.check_linearity <- function(y, x) {
  performance::r2(stats::lm(y ~ x))$R2
}
