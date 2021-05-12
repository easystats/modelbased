# Models ------------------------------------------------------------------


#' @export
visualisation_matrix_old.glm <- function(x,
                                     target = "all",
                                     length = 10,
                                     factors = "reference",
                                     numerics = "mean",
                                     preserve_range = TRUE,
                                     standardize = FALSE,
                                     standardize_robust = FALSE,
                                     reference = x,
                                     na.rm = TRUE,
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

  data <- visualisation_matrix_old(
    data,
    target = target,
    length = length,
    factors = factors,
    numerics = numerics,
    preserve_range = preserve_range,
    standardize = standardize,
    standardize_robust = standardize_robust,
    reference = data,
    na.rm = na.rm,
    ...
  )

  if (include_smooth == FALSE) {
    data <- data[!names(data) %in% insight::clean_names(insight::find_smooth(x, flatten = TRUE))]
  }

  attr(data, "model") <- x
  data
}


# #' @export
# visualisation_matrix_old.gamm <- function(x, target = "all", length = 10, factors = "reference", numerics = "mean", preserve_range = FALSE, standardize = FALSE, standardize_robust = FALSE, reference = attributes(x)$reference, na.rm = TRUE, ...) {
#   data <- x$gam$model[insight::find_predictors(x$gam, flatten=TRUE)]
#
#   grid <- visualisation_matrix_old(data, target = target, length = length, factors = factors, numerics = numerics, preserve_range = preserve_range, standardize = standardize, standardize_robust = standardize_robust, reference = reference, na.rm = na.rm, ...)
#   attr(grid, "model") <- x
#   grid
# }




# #' @importFrom insight find_random
# #' @export
# visualisation_matrix_old.glmmTMB <- function(x, target = "all", length = 10, factors = "reference", numerics = "mean", preserve_range = FALSE, standardize = FALSE, standardize_robust = FALSE, reference = x, na.rm = TRUE, include_random = TRUE, include_response = FALSE, ...) {
#   data <- insight::get_data(x)
#   if (include_response == FALSE) {
#     data <- data[names(data) != insight::find_response(x)]
#   }
#
#   # if (include_random == FALSE) {
#   #   data <- data[names(data) %in% insight::find_predictors(x, effects = "fixed", flatten = TRUE)]
#   # }
#
#   data <- data[names(data) %in% insight::find_predictors(x, effects = "fixed", flatten = TRUE)]
#   data <- visualisation_matrix_old(data, target = target, length = length, factors = factors, numerics = numerics, preserve_range = preserve_range, standardize = standardize, standardize_robust = standardize_robust, reference = data, na.rm = na.rm, random = TRUE, ...)
#
#   random <- insight::find_random(x, split_nested = TRUE, flatten = TRUE)
#   data[random] <- NA
#
#   attr(data, "model") <- x
#   data
# }


#' @export
visualisation_matrix_old.lm <- visualisation_matrix_old.glm
#' @export
visualisation_matrix_old.brmsfit <- visualisation_matrix_old.glm
#' @export
visualisation_matrix_old.stanreg <- visualisation_matrix_old.glm
#' @export
visualisation_matrix_old.polr <- visualisation_matrix_old.glm
#' @export
visualisation_matrix_old.merMod <- visualisation_matrix_old.glm
#' @export
visualisation_matrix_old.lmerMod <- visualisation_matrix_old.glm
#' @export
visualisation_matrix_old.glmmTMB <- visualisation_matrix_old.glm
#' @export
visualisation_matrix_old.gamm <- visualisation_matrix_old.glm
#' @export
visualisation_matrix_old.list <- visualisation_matrix_old.glm # list is gamm4


#' @export
visualisation_matrix_old.visualisation_matrix <- function(x,
                                                      target = "all",
                                                      length = 10,
                                                      factors = "reference",
                                                      numerics = "mean",
                                                      preserve_range = FALSE,
                                                      standardize = FALSE,
                                                      standardize_robust = FALSE,
                                                      reference = attributes(x)$reference,
                                                      na.rm = TRUE,
                                                      ...) {
  grid <- visualisation_matrix_old(
    as.data.frame(x),
    target = target,
    length = length,
    factors = factors,
    numerics = numerics,
    preserve_range = preserve_range,
    standardize = standardize,
    standardize_robust = standardize_robust,
    reference = reference,
    na.rm = na.rm,
    ...
  )

  if ("model" %in% names(attributes(x))) {
    attr(grid, "model") <- attributes(x)$model
  }

  grid
}
