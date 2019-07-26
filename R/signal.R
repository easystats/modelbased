#' Copied from rootSolve package
#' @importFrom stats uniroot
#' @keywords internal
.uniroot.all <- function(f, interval, lower = min(interval),
                         upper = max(interval), tol = .Machine$double.eps^0.2,
                         maxiter = 1000, n = 100, ...) {

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
    Equi <- c(Equi, uniroot(f, lower = xseq[i], upper = xseq[i + 1], ...)$root)
  }

  return(Equi)
}









#' Find zero crossings of a vector
#'
#' Find zero crossings of a vector, i.e., indices when the numeric variable crosses 0.
#' @param x A numeric vector.
#'
#' @examples
#' x <- sin(seq(0, 4 * pi, length.out = 100))
#' plot(x)
#' zero_crossings(x)
#' @importFrom stats approxfun
#' @seealso Based on the \code{uniroot.all} function from the rootSolve package.
#' @export
zero_crossings <- function(x) {

  # Estimate gradient
  zerocrossings <- .uniroot.all(approxfun(1:length(x), x), interval = range(1:length(x)))
  if (length(zerocrossings) == 0) {
    return(NA)
  }
  zerocrossings
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
#' @export
find_inversions <- function(x) {
  zero_crossings(diff(x))
}








#' Smoothing a vector or a time series
#'
#' Smoothing a vector or a time series.
#'
#' @param x A numeric vector.
#' @param method Can be \link[=loess]{"loess"} (default) or \link[=smooth]{"smooth"}. A loess smoothing can be slow.
#' @param strength Degree of smoothing. Either passed to \code{span} when \code{method = "loess"}.
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
#' @importFrom stats predict loess smooth
#' @export
smoothing <- function(x, method = "loess", strength = 0.25) {
  if (strength == 0 | strength == FALSE | is.null(method)) {
    return(x)
  }

  method <- match.arg(method, c("loess", "smooth"))
  if (method == "loess") {
    smoothed <- tryCatch({
      predict(loess(paste0("y ~ x"), data = data.frame(y = x, x = 1:length(x)), span = strength))
      }, warning = function(w) {
        warning(paste0("Smoothing had some difficulties. Try tweaking the smoothing strength (currently at ", strength, ")."))
        predict(loess(paste0("y ~ x"), data = data.frame(y = x, x = 1:length(x)), span = strength))
      })
  } else if (method == "smooth") {
    smoothed <- smooth(x)
  } else {
    stop('method must be one of c("loess", "smooth")')
  }
  smoothed
}
