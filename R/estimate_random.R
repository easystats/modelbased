#' Group-specific parameters of mixed models random effects
#'
#' Extract random parameters of each individual group in the context of mixed models. Can be reshaped to be of the same dimensions as the original data, which can be useful to add the random effects to the original data.
#'
#' @param model A mixed model with random effects.
#' @param deviation If \code{TRUE}, the coefficients are the ones estimated natively by the model (as they are returned by, for instance, \code{lme4::ranef()}). They correspond to the \emph{deviation} of each individual group from their fixed effect. As such, a coefficient close to 0 means that the participants' effect is the same as the population-level effect (in other words, it is "in the norm"). If \code{FALSE}, will return the sum of the random effect and its corresponding fixed effects. This argument can be used to reproduce the results given by \code{lme4::ranef()} and \code{coef()} (see \code{?coef.merMod}). Note that the sum is made between the random effects parameters and the fixed effect coefficient (which is a point-estimate), and thus does not take into account the variability and uncertainty in the fixed effects estimations. Thus, use these with caution.
#' @param ... Other arguments passed to or from other methods.
#'
#' @examples
#' if (require("lme4") && require("see")) {
#'
#' # Random intercept ------------
#' model <- lmer(mpg ~ hp + (1 | carb), data = mtcars)
#' random <- estimate_random(model)
#' random
#'
#' # Visualize random effects
#' plot(random)
#'
#' # Show group-specific effects
#' estimate_random(model, deviation = FALSE)
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
estimate_random <- function(model, deviation = TRUE, ...) {

  # Extract params
  params <- parameters::model_parameters(model, group_level = TRUE, ...)

  # TODO: improve / add new printing that groups by group/level?
  random <- as.data.frame(params[params$Effects == "random", ])

  # Remove columns with only NaNs (as these are probably those of fixed effects)
  random[sapply(random, function(x) all(is.na(x)))] <- NULL

  # Correct for fixed effect
  if(deviation == FALSE) {
    fixed <- as.data.frame(params[params$Effects == "fixed", ])
    cols <- c("Coefficient", "Median", "Mean", "MAP_Estimate", "CI_low", "CI_high")
    cols <- cols[cols %in% names(random)]
    for(p in fixed$Parameter) {
      random[random$Parameter == p, cols] <- random[random$Parameter == p, cols] + fixed[fixed$Parameter == p, cols[1]]
    }
  }

  # Reorganize columns
  random$Effects <- NULL
  random <- random[c("Group", "Level", names(random)[!names(random) %in% c("Group", "Level")])]

  # Sort
  random <- random[order(random$Group, insight::to_numeric(random$Level), random$Parameter), ]

  # Clean
  row.names(random) <- NULL

  # Assign new class
  attr(random, "deviation") <- deviation
  attr(random, "model") <- model
  attr(random, "parameters") <- params
  attr(random, "data") <- insight::get_data(model)[insight::find_random(model, split_nested = TRUE, flatten = TRUE)]

  class(random) <- c("estimate_random", class(random))
  random
}


# TODO (maybe) summary for estimate_random: keep only coefficient column