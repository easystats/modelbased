#' @export
plot.estimate_contrasts <- function(x, ...) {
  if (!requireNamespace("see", quietly = TRUE)) {
    stop("Package 'see' needed to plot results from contrast analysis. Please install it by running `install.packages('see')`.")
  }
  NextMethod()
}
