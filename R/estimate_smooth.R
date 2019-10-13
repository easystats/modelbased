#' Describe the smooth term (for GAMs) or non-linear predictors
#'
#' This function summarise the smooth term trend in terms of linear segments. Using the aproximative derivative, it separates a non-linear vector into quasi-linear segments (in which the trend is either positive or negative). Each of this segment its characterised by its beginning, end, size (in proportion, relative to the total size) trend (the linear regression coefficient) and linearity (the R2 of the linear regression).
#'
#' See the documentation for your object's class:
#' \itemize{
#'  \item{\link[=estimate_smooth.stanreg]{Bayesian models (stanreg and brms)}}
#'  }
#'
#' @param smooth A character indicating the name of the "smooth" term.
#' @inheritParams estimate_slopes
#' @inheritParams estimate_response
#'
#'
#' @export
estimate_smooth <- function(model, smooth = NULL, levels = NULL, length = 200, transform = "response", ...) {
  UseMethod("estimate_smooth")
}














#' Describe the smooth term (for GAMs) or non-linear predictors
#'
#' @inheritParams estimate_smooth
#' @inheritParams estimate_slopes.stanreg
#' @inheritParams estimate_response.stanreg
#'
#' @examples
#' library(estimate)
#' \dontrun{
#' library(rstanarm)
#' model <- stan_gamm4(Sepal.Width ~ s(Petal.Length), data = iris)
#' estimate_smooth(model)
#'
#' model <- stan_glm(Sepal.Width ~ poly(Petal.Length, 2), data = iris)
#' estimate_smooth(model)
#'
#' model <- stan_gamm4(Sepal.Width ~ Species + s(Petal.Length), data = iris)
#' estimate_smooth(model)
#'
#' model <- stan_glm(Sepal.Width ~ Species * poly(Petal.Length, 2), data = iris)
#' estimate_smooth(model)
#' estimate_smooth(model, levels = "Species")
#' }
#' @importFrom insight find_predictors get_data
#' @importFrom stats mad median sd setNames predict loess
#' @export
estimate_smooth.stanreg <- function(model, smooth = NULL, levels = NULL, length = 200, transform = "response", smooth_method = "smooth", smooth_strength = 0.2, centrality = "median", ...) {
  predictors <- insight::find_predictors(model)$conditional
  data <- insight::get_data(model)

  if (is.null(smooth)) {
    smooth <- predictors[sapply(data[predictors], is.numeric)][1]
    message('No numeric variable was specified for smooth analysis. Selecting `smooth = "', smooth, '"`.')
  }
  if (length(smooth) > 1) {
    message("More than one numeric variable was selected for smooth analysis. Keeping only ", smooth[1], ".")
    smooth <- smooth[1]
  }

  if (is.null(levels)) {
    target <- smooth
  } else {
    target <- c(levels[!levels %in% smooth], smooth)
  }

  # Basis
  newdata <- visualisation_matrix(data[predictors], target, length = length, factors = "reference", numerics = "mean", ...)

  smooth_data <- estimate_link(model, newdata,
    predict = "link",
    centrality = centrality, transform = transform,
    keep_draws = FALSE, draws = NULL,
    seed = NULL, random = FALSE, smooth_method = smooth_method,
    smooth_strength = smooth_strength, ...
  )

  if (!is.null(levels)) {
    description <- data.frame()
    groups <- visualisation_matrix(smooth_data[levels])
    for (row in 1:nrow(groups)) {
      data <- smooth_data
      for (col in names(groups)) {
        data <- data[data[[col]] == groups[row, col], ]
        current_description <- .describe_smooth(data$Median)
        current_description$Start <- data[current_description$Start, smooth]
        current_description$End <- data[current_description$End, smooth]
        group <- as.data.frame(groups[rep(row, nrow(current_description)), ])
        names(group) <- names(groups)
        current_description <- cbind(
          group,
          current_description
        )
        description <- rbind(
          description,
          current_description
        )
      }
    }
  } else {
    description <- .describe_smooth(smooth_data$Median)

    description$Start <- smooth_data[description$Start, smooth]
    description$End <- smooth_data[description$End, smooth]
  }

  attributes(description) <- c(
    attributes(description),
    list(smooth = smooth, levels = levels, transform = transform, response = insight::find_response(model))
  )
  class(description) <- c("estimate_smooth", class(description))
  description
}








#' @importFrom utils tail
#' @importFrom stats coef lm
#' @keywords internal
.describe_smooth <- function(smooth_values) {
  inversions <- find_inversions(smooth_values)

  # Add beginning and end
  if (all(is.na(inversions))) {
    parts <- c(1, length(smooth_values))
  } else {
    if (inversions[1] != 1) {
      parts <- c(1, inversions)
    } else {
      parts <- inversions
    }
    if (utils::tail(inversions, 1) < length(smooth_values)) {
      parts <- c(parts, length(smooth_values))
    }
  }
  n_parts <- length(parts) - 1

  df <- data.frame()
  for (part in 1:n_parts) {
    range <- parts[(1 * part):(1 * part + 1)]
    segment <- smooth_values[range[1]:range[2]]

    segment_df <- cbind(
      data.frame(
        "Part" = part,
        "Start" = range[1],
        "End" = range[2],
        "Size" = length(segment) / length(smooth_values)
      ),
      .describe_segment(segment, range)
    )

    df <- rbind(df, segment_df)
  }

  df
}



#' @importFrom stats lm
#' @importFrom parameters check_smoothness
#' @keywords internal
.describe_segment <- function(segment, range, smoothness = FALSE) {
  # Smoothness
  if (smoothness) {
    if (length(segment) < 10) {
      smoothness <- NA
    } else {
      smoothness <- parameters::smoothness(segment, method = "cor", lag = 0.1)
    }
  }


  if (length(segment) < 3) {
    trend <- NA
    linearity <- NA
  } else {
    model <- stats::lm(y ~ x,
      data = data.frame(
        "y" = segment,
        "x" = seq(range[1], range[2], length.out = length(segment))
      )
    )

    trend <- as.numeric(coef(model)[2])
    linearity <- as.numeric(summary(model)$r.squared)
  }

  out <- data.frame(
    "Trend" = trend,
    "Linearity" = linearity
  )
  if (smoothness != FALSE) {
    out$Smoothness <- smoothness
  }

  out
}
