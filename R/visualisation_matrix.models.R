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
  # Retrieve data from model
  if (is.null(data)) {
    data <- insight::get_data(x)[insight::find_variables(x, "all", flatten = TRUE)]
  }

  # Deal with intercept-only models
  if (include_response == FALSE) {
    data <- data[!names(data) %in% insight::find_response(x)]
    if (ncol(data) < 1) {
      stop(insight::format_message("Model only seems to be an intercept-only model. Use `include_response=TRUE` to create the visualization matrix."), call. = FALSE)
    }
  }

  # Drop random factors
  if (include_random == FALSE) {
    keep <- insight::find_predictors(x, effects = "fixed", flatten = TRUE)
    if (!is.null(keep)) {
      if (all(at != "all")) {
        keep <- c(keep, at[at %in% insight::find_random(x, flatten = TRUE)])
      }
      data <- data[names(data) %in% keep]
    }
  }

  if (all(at == "all")) at <- names(data)
  if (include_smooth == FALSE || include_smooth == "fixed") {
    s <- insight::find_smooth(x, flatten = TRUE)
    if (!is.null(s)) {
      at <- names(data)[!names(data) %in% insight::clean_names(s)]
    }
  }

  vm <- visualisation_matrix(
    data,
    at = at,
    factors = factors,
    numerics = numerics,
    preserve_range = preserve_range,
    reference = data,
    ...
  )

  if (include_smooth == FALSE) {
    vm[names(vm) %in% insight::clean_names(insight::find_smooth(x, flatten = TRUE))] <- NULL
  }

  attr(vm, "model") <- x
  vm
}






# -------------------------------------------------------------------------
# Below are visualisation_matrix functions that work on visualisation_matrix
# -------------------------------------------------------------------------

#' @export
visualisation_matrix.visualisation_matrix <- function(x, reference = attributes(x)$reference, ...) {
  grid <- visualisation_matrix(as.data.frame(x), reference = reference, ...)

  if ("model" %in% names(attributes(x))) {
    attr(grid, "model") <- attributes(x)$model
  }

  grid
}



# helpers -------------
.factor_to_numeric <- function(x, lowest = NULL)
{
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
