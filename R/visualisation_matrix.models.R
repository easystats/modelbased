# Models ------------------------------------------------------------------


#' @export
visualisation_matrix.glm <- function(x, target = "all", length = 10, factors = "reference", numerics = "mean", preserve_range = FALSE, standardize = FALSE, standardize_robust = FALSE, reference = x, na.rm = TRUE, include_smooth = TRUE, include_random = FALSE, include_response = FALSE, ...) {
  data <- insight::get_data(x)[insight::find_variables(x, "all", flatten = TRUE)]

  if (include_response == FALSE) {
    data <- data[!names(data) %in% insight::find_response(x)]
  }

  if (include_random == FALSE) {
    data <- data[names(data) %in% insight::find_predictors(x, effects = "fixed", flatten = TRUE)]
  }

  if (target == "all") target <- names(data)
  if (include_smooth == FALSE | include_smooth == "fixed") {
    target <- names(data)[!names(data) %in% insight::clean_names(insight::find_smooth(x, flatten = TRUE))]
  }

  data <- visualisation_matrix(data, target = target, length = length, factors = factors, numerics = numerics, preserve_range = preserve_range, standardize = standardize, standardize_robust = standardize_robust, reference = data, na.rm = na.rm, ...)

  if (include_smooth == FALSE) {
    data <- data[!names(data) %in% insight::clean_names(insight::find_smooth(x, flatten = TRUE))]
  }

  attr(data, "model") <- x
  data
}


# #' @export
# visualisation_matrix.gamm <- function(x, target = "all", length = 10, factors = "reference", numerics = "mean", preserve_range = FALSE, standardize = FALSE, standardize_robust = FALSE, reference = attributes(x)$reference, na.rm = TRUE, ...) {
#   data <- x$gam$model[insight::find_predictors(x$gam, flatten=TRUE)]
#
#   grid <- visualisation_matrix(data, target = target, length = length, factors = factors, numerics = numerics, preserve_range = preserve_range, standardize = standardize, standardize_robust = standardize_robust, reference = reference, na.rm = na.rm, ...)
#   attr(grid, "model") <- x
#   grid
# }




# #' @importFrom insight find_random
# #' @export
# visualisation_matrix.glmmTMB <- function(x, target = "all", length = 10, factors = "reference", numerics = "mean", preserve_range = FALSE, standardize = FALSE, standardize_robust = FALSE, reference = x, na.rm = TRUE, include_random = TRUE, include_response = FALSE, ...) {
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
#   data <- visualisation_matrix(data, target = target, length = length, factors = factors, numerics = numerics, preserve_range = preserve_range, standardize = standardize, standardize_robust = standardize_robust, reference = data, na.rm = na.rm, random = TRUE, ...)
#
#   random <- insight::find_random(x, split_nested = TRUE, flatten = TRUE)
#   data[random] <- NA
#
#   attr(data, "model") <- x
#   data
# }


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


#' @export
visualisation_matrix.visualisation_matrix <- function(x, target = "all", length = 10, factors = "reference", numerics = "mean", preserve_range = FALSE, standardize = FALSE, standardize_robust = FALSE, reference = attributes(x)$reference, na.rm = TRUE, ...) {
  grid <- visualisation_matrix(as.data.frame(x), target = target, length = length, factors = factors, numerics = numerics, preserve_range = preserve_range, standardize = standardize, standardize_robust = standardize_robust, reference = reference, na.rm = na.rm, ...)
  if ("model" %in% names(attributes(x))) {
    attr(grid, "model") <- attributes(x)$model
  }
  grid
}
