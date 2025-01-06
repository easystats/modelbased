#' Smoothing a vector or a time series
#'
#' Smoothing a vector or a time series. For data.frames, the function will
#' smooth all numeric variables stratified by factor levels (i.e., will smooth
#' within each factor level combination).
#'
#' @param x A numeric vector.
#' @param method Can be ["loess"][loess] (default) or
#'   ["smooth"][smooth]. A loess smoothing can be slow.
#' @param strength This argument only applies when `method = "loess"`.
#'   Degree of smoothing passed to `span` (see [loess()]).
#' @param ... Arguments passed to or from other methods.
#'
#'
#' @examples
#' x <- sin(seq(0, 4 * pi, length.out = 100)) + rnorm(100, 0, 0.2)
#' plot(x, type = "l")
#' lines(smoothing(x, method = "smooth"), type = "l", col = "blue")
#' lines(smoothing(x, method = "loess"), type = "l", col = "red")
#'
#' x <- sin(seq(0, 4 * pi, length.out = 10000)) + rnorm(10000, 0, 0.2)
#' plot(x, type = "l")
#' lines(smoothing(x, method = "smooth"), type = "l", col = "blue")
#' lines(smoothing(x, method = "loess"), type = "l", col = "red")
#' @return A smoothed vector or data frame.
#' @export
smoothing <- function(x, method = "loess", strength = 0.25, ...) {
  UseMethod("smoothing")
}


#' @export
smoothing.numeric <- function(x, method = "loess", strength = 0.25, ...) {
  if (strength == 0 || isFALSE(strength) || is.null(method)) {
    return(x)
  }

  method <- match.arg(method, c("loess", "smooth"))
  if (method == "loess") {
    smoothed <- tryCatch(
      {
        stats::predict(stats::loess(
          paste0("y ~ x"),
          data = data.frame(y = x, x = seq_len(length(x))),
          span = strength
        ))
      },
      warning = function(w) {
        insight::format_warning(
          paste0(
            "Smoothing had some difficulties. Try tweaking the smoothing strength (currently at ",
            strength,
            ")."
          )
        )
        stats::predict(stats::loess(
          paste0("y ~ x"),
          data = data.frame(y = x, x = seq_len(length(x))),
          span = strength
        ))
      }
    )
  } else if (method == "smooth") {
    smoothed <- stats::smooth(x, ...)
  } else {
    stop('method must be one of c("loess", "smooth")', call. = FALSE)
  }
  smoothed
}


#' @export
smoothing.data.frame <- function(x, method = "loess", strength = 0.25, ...) {
  nums <- names(x)[vapply(x, is.numeric, TRUE)]

  # Stratify by factor levels
  factors <- names(x)[vapply(x, is.factor, TRUE)]

  if (length(factors) > 0) {
    combinations <- unique(x[factors])
    row.names(combinations) <- NULL
    x$temp <- apply(x[names(combinations)], 1, paste, collapse = "_")

    for (i in seq_len(nrow(combinations))) {
      current_row <- paste0(t(combinations[i, ]), collapse = "_")
      x[x$temp == current_row, nums] <- sapply(x[x$temp == current_row, nums], smoothing, method = method, strength = strength, ...)
    }

    x$temp <- NULL
  } else {
    x[nums] <- sapply(x[nums], smoothing, method = method, strength = strength, ...)
  }

  x
}
