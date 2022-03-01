# -------------------------------------------------------------------------
# Below are visualisation_matrix functions that work on statistical models
# -------------------------------------------------------------------------

#' @export
visualisation_matrix.default <- function(x,
                                         at = "all",
                                         factors = "reference",
                                         numerics = "mean",
                                         preserve_range = TRUE,
                                         reference = x,
                                         include_smooth = TRUE,
                                         include_random = FALSE,
                                         include_response = FALSE,
                                         data = NULL,
                                         ...) {
  insight::get_datagrid(x,
                        at = at,
                        factors = factors,
                        numerics = numerics,
                        preserve_range = preserve_range,
                        reference = reference,
                        include_smooth = include_smooth,
                        include_random = include_random,
                        include_response = include_response,
                        data = data,
                        ...)
}






# -------------------------------------------------------------------------
# Below are visualisation_matrix functions that work on visualisation_matrix
# -------------------------------------------------------------------------

#' @export
visualisation_matrix.visualisation_matrix <- function(x, reference = attributes(x)$reference, ...) {
  insight::get_datagrid(x, reference = reference, ...)
}



# helpers -------------
.factor_to_numeric <- function(x, lowest = NULL) {
  if (is.numeric(x)) {
    return(x)
  }
  if (is.logical(x)) {
    return(as.numeric(x))
  }
  if (anyNA(suppressWarnings(as.numeric(as.character(stats::na.omit(x)))))) {
    if (is.character(x)) {
      x <- as.factor(x)
    }
    x <- droplevels(x)
    levels(x) <- 1:nlevels(x)
  }
  out <- as.numeric(as.character(x))
  if (!is.null(lowest)) {
    difference <- min(out) - lowest
    out <- out - difference
  }
  out
}
