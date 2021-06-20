#' Find zero crossings of a vector
#'
#' Find zero crossings of a vector, i.e., indices when the numeric variable
#' crosses 0.
#'
#' @param x A numeric vector.
#'
#' @examples
#' x <- sin(seq(0, 4 * pi, length.out = 100))
#' plot(x)
#' zero_crossings(x)
#' @return Vector of zero crossings.
#' @seealso Based on the \code{uniroot.all} function from the rootSolve package.
#' @export
zero_crossings <- function(x) {

  # Estimate gradient
  zerocrossings <- .uniroot.all(stats::approxfun(1:length(x), x), interval = range(1:length(x)))
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
    stop("'interval' must be a vector of length 2")
  }
  if (!is.numeric(lower) || !is.numeric(upper) || lower >=
    upper) {
    stop("lower < upper  is not fulfilled")
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

  return(Equi)
}




#' Find points of inversion
#'
#' Find points of inversion of a curve.
#'
#' @param x A numeric vector.
#'
#' @examples
#' x <- sin(seq(0, 4 * pi, length.out = 100))
#' plot(x, type = "b")
#' find_inversions(x)
#' @return Vector of inversion points.
#' @export
find_inversions <- function(x) {
  zero_crossings(diff(x))
}
