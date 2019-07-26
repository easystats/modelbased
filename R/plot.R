#' @export
plot.see_estimate_contrasts <- function(x, ...) {
  if (!requireNamespace("see", quietly = TRUE)) {
    stop("Package 'see' needed to plot results from equivalence-test. Please install it by running `install.packages('see')`.")
  }
  NextMethod()
}
