#' Find zero-crossings and inversion points
#'
#' Find zero crossings of a vector, i.e., indices when the numeric variable
#' crosses 0. It is useful for finding the points where a function changes by
#' looking at the zero crossings of its derivative.
#'
#' @param x A numeric vector.
#'
#' @return Vector of zero crossings or points of inversion.
#' @seealso Based on the `uniroot.all` function from the rootSolve package.
#'
#' @examples
#' x <- sin(seq(0, 4 * pi, length.out = 100))
#' plot(x, type = "b")
#'
#' zero_crossings(x)
#' find_inversions(x)
#' @export
zero_crossings <- function(x) {
  # Estimate gradient
  zerocrossings <- .uniroot.all(stats::approxfun(seq_len(length(x)), x), interval = range(seq_len(length(x))))
  if (length(zerocrossings) == 0) {
    return(NA)
  }

  zerocrossings
}


#' Copied from rootSolve package
#' @keywords internal
.uniroot.all <- function(f,
                         interval,
                         lower = min(interval),
                         upper = max(interval),
                         tol = .Machine$double.eps^0.2,
                         maxiter = 1000,
                         n = 100,
                         ...) {
  ## error checking as in uniroot...
  if (!missing(interval) && length(interval) != 2) {
    insight::format_error("`interval` must be a vector of length two.")
  }

  if (!is.numeric(lower) || !is.numeric(upper) || lower >= upper) {
      insight::format_error("`lower` is not smaller than `upper`.")
  }

  ## subdivide interval in n subintervals and estimate the function values
  xseq <- seq(lower, upper, len = n + 1)
  mod <- f(xseq, ...)

  ## some function values may already be 0
  Equi <- xseq[which(mod == 0)]

  ss <- mod[1:n] * mod[2:(n + 1)] # interval where functionvalues change sign
  ii <- which(ss < 0)

  for (i in ii) {
    Equi <- c(Equi, stats::uniroot(f, lower = xseq[i], upper = xseq[i + 1], ...)$root)
  }

  Equi
}


#' @rdname zero_crossings
#'
#' @export
find_inversions <- function(x) {
  zero_crossings(diff(x))
}
