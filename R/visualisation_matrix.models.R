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
                                         ...) {
  # Retrieve data from model
  data <- insight::get_data(x)[insight::find_variables(x, "all", flatten = TRUE)]

  # Deal with factor transformations
  # f <- insight::find_terms(model)
  numeric_factors <- attributes(data)$factors
  data[] <- lapply(data, function(i) {
    if (isTRUE(attributes(i)$factor)) {
      as.factor(i)
    } else {
      i
    }
  })


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
