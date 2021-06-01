# -------------------------------------------------------------------------
# Below are visualisation_matrix functions that work on statistical models
# -------------------------------------------------------------------------

#' @export
visualisation_matrix.glm <- function(x,
                                     target = "all",
                                     factors = "reference",
                                     numerics = "mean",
                                     preserve_range = TRUE,
                                     reference = x,
                                     include_smooth = TRUE,
                                     include_random = FALSE,
                                     include_response = FALSE,
                                     ...) {
  data <- insight::get_data(x)[insight::find_variables(x, "all", flatten = TRUE)]

  if (include_response == FALSE) {
    data <- data[!names(data) %in% insight::find_response(x)]
  }

  if (include_random == FALSE) {
    data <- data[names(data) %in% insight::find_predictors(x, effects = "fixed", flatten = TRUE)]
  }

  if (all(target == "all")) target <- names(data)
  if (include_smooth == FALSE || include_smooth == "fixed") {
    s <- insight::find_smooth(x, flatten = TRUE)
    if (!is.null(s)) {
      target <- names(data)[!names(data) %in% insight::clean_names(s)]
    }
  }

  data <- visualisation_matrix(
    data,
    target = target,
    factors = factors,
    numerics = numerics,
    preserve_range = preserve_range,
    reference = data,
    ...
  )

  if (include_smooth == FALSE) {
    data[names(data) %in% insight::clean_names(insight::find_smooth(x, flatten = TRUE))] <- NULL
  }

  attr(data, "model") <- x
  data
}





#' @export
visualisation_matrix.lm <- visualisation_matrix.glm
#' @export
visualisation_matrix.brmsfit <- visualisation_matrix.glm
#' @export
visualisation_matrix.stanreg <- visualisation_matrix.glm
#' @export
visualisation_matrix.polr <- visualisation_matrix.glm
#' @export
visualisation_matrix.merMod <- visualisation_matrix.glm
#' @export
visualisation_matrix.lmerMod <- visualisation_matrix.glm
#' @export
visualisation_matrix.glmmTMB <- visualisation_matrix.glm
#' @export
visualisation_matrix.gamm <- visualisation_matrix.glm
#' @export
visualisation_matrix.list <- visualisation_matrix.glm # list is gamm4



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
