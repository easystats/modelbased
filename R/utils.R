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





#' Convert to Numeric if Possible
#'
#' Tries to convert vector to numeric if possible. Otherwise, leaves it as is.
#' @param x A vector to be converted.
#'
#' @examples
#' as.numeric_ifnumeric(c("1", "2"))
#' as.numeric_ifnumeric(c("1", "2", "A"))
#' @export
as.numeric_ifnumeric <- function(x) {
  x <- tryCatch(as.numeric(as.character(x)), error = function(e) x, warning = function(w) x)
  x
}








#' @keywords internal
.remove_name_level <- function(x) {
  name <- .find_name_level(x)
  x <- sub(name, "", x)
  x <- trimws(x)
  x
}







#' @keywords internal
.find_name_level <- function(x, data, fixed, modulate) {
  splitted <- strsplit(as.character(x), " ")
  splitted <- data.frame(do.call(rbind, splitted), stringsAsFactors = FALSE)
  uniques <- sapply(splitted, unique)

  lengths <- sapply(uniques, length)
  if (lengths[1] == 1) {
    return(as.character(uniques[[1]]))
  } else if (all(grepl(" - ", x))) {
    return("Contrast")
  } else {
    warning("Couldn't find consistent level name.")
    if (is.null(names(x))) {
      return("X")
    } else {
      return(names(x))
    }
  }
}












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
  grad <- diff(x)
  zerocrossings <- .uniroot.all(approxfun(1:length(grad), grad), interval = range(1:length(grad)))
  if (length(zerocrossings) == 0) {
    return(NA)
  }
  return(zerocrossings)
}
