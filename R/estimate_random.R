#' Group-specific parameters of mixed models random effects
#'
#' Extract random parameters of each individual group in the context of mixed models. Can be reshaped to be of the same dimensions as the original data, which can be useful to add the random effects to the original data.
#'
#' @param model A mixed model with random effects.
#' @param ... Other arguments passed to or from other methods.
#'
#' @examples
#' if (require("lme4")) {
#'
#' # Random intercept ------------
#' model <- lmer(mpg ~ hp + (1 | carb), data = mtcars)
#' random <- estimate_random(model)
#' random
#'
#' # Reshape to wide data so that it matches the original dataframe...
#' reshaped <- reshape_random(random, indices = c("Coefficient", "SE"))
#'
#' # ... and can be easily combined
#' alldata <- cbind(mtcars, reshaped)
#'
#' # Use summary() to remove duplicated rows
#' summary(reshaped)
#' }
#' @export
estimate_random <- function(model, ...) {

  # Extract params
  params <- parameters::model_parameters(model, group_level = TRUE, ...)

  # TODO: improve / add new printing that groups by group/level?
  random <- as.data.frame(params[params$Effects == "random", ])

  # Remove columns with only NaNs (as these are probably those of fixed effects)
  random[sapply(random, function(x) all(is.na(x)))] <- NULL

  # Reorganize columns
  random$Effects <- NULL
  random <- random[c("Group", "Level", names(random)[!names(random) %in% c("Group", "Level")])]

  # Sort
  random <- random[order(random$Group, insight::to_numeric(random$Level), random$Parameter), ]

  # Clean
  row.names(random) <- NULL

  # Assign new class
  attr(random, "model") <- model
  attr(random, "parameters") <- params
  attr(random, "data") <- insight::get_data(model)[insight::find_random(model, split_nested = TRUE, flatten = TRUE)]

  class(random) <- c("estimate_random", class(random))
  random
}


# TODO (maybe) summary for estimate_random: keep only coefficient column