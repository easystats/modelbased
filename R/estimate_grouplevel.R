#' Group-specific parameters of mixed models random effects
#'
#' Extract random parameters of each individual group in the context of mixed
#' models. Can be reshaped to be of the same dimensions as the original data,
#' which can be useful to add the random effects to the original data.
#'
#' @param model A mixed model with random effects.
#' @param type If `"random"` (default), the coefficients are the ones
#'   estimated natively by the model (as they are returned by, for instance,
#'   `lme4::ranef()`). They correspond to the deviation of each individual
#'   group from their fixed effect. As such, a coefficient close to 0 means that
#'   the participants' effect is the same as the population-level effect (in
#'   other words, it is "in the norm"). If "total", it will return the sum of
#'   the random effect and its corresponding fixed effects. These are known as
#'   BLUPs (Best Linear Unbiased Predictions). This argument can be used to
#'   reproduce the results given by `lme4::ranef()` and `coef()` (see
#'   `?coef.merMod`). Note that BLUPs currently don't have uncertainty
#'   indices (such as SE and CI), as these are not computable.
#' @param ... Other arguments passed to or from other methods.
#'
#' @examplesIf require("lme4") && require("see")
#' # lme4 model
#' data(mtcars)
#' model <- lme4::lmer(mpg ~ hp + (1 | carb), data = mtcars)
#' random <- estimate_grouplevel(model)
#' random
#'
#' # Visualize random effects
#' plot(random)
#'
#' # Show group-specific effects
#' estimate_grouplevel(model, deviation = FALSE)
#'
#' # Reshape to wide data so that it matches the original dataframe...
#' reshaped <- reshape_grouplevel(random, indices = c("Coefficient", "SE"))
#'
#' # ... and can be easily combined
#' alldata <- cbind(mtcars, reshaped)
#'
#' # Use summary() to remove duplicated rows
#' summary(reshaped)
#'
#' # Compute BLUPs
#' estimate_grouplevel(model, type = "total")
#' @export
estimate_grouplevel <- function(model, type = "random", ...) {
  # Extract params
  params <- parameters::model_parameters(model,
    effects = "all",
    group_level = TRUE,
    ...
  )

  # Re-add info
  if (!"Group" %in% names(params)) params$Group <- attributes(params)$clean_parameters$Group
  if (!"Level" %in% names(params)) params$Level <- attributes(params)$clean_parameters$Cleaned_Parameter

  # TODO: improve / add new printing that groups by group/level?
  random <- as.data.frame(params[params$Effects == "random", ])


  # Remove columns with only NaNs (as these are probably those of fixed effects)
  random[vapply(random, function(x) all(is.na(x)), TRUE)] <- NULL

  # Correct for fixed effect
  type <- match.arg(type, c("random", "total"))
  if (type == "total") {
    fixed <- as.data.frame(params[params$Effects == "fixed", ])
    cols <- intersect(c("Coefficient", "Median", "Mean", "MAP_Estimate"), names(random))
    for (p in fixed$Parameter) {
      random[random$Parameter == p, cols] <- random[random$Parameter == p, cols] + fixed[fixed$Parameter == p, cols[1]]
    }
    # Remove uncertainty indices
    for (col in c("CI", "CI_low", "CI_high")) random[[col]] <- NULL
    for (col in c("SE", "SD", "MAD")) random[[col]] <- NULL
  }

  # Reorganize columns
  random$Effects <- NULL
  random <- random[c("Group", "Level", names(random)[!names(random) %in% c("Group", "Level")])]

  # Sort
  random <- random[order(random$Group, .to_numeric(random$Level), random$Parameter), ]

  # Clean
  row.names(random) <- NULL

  # Assign new class
  attr(random, "type") <- type
  attr(random, "model") <- model
  attr(random, "parameters") <- params
  attr(random, "data") <- insight::get_data(model)[insight::find_random(model, split_nested = TRUE, flatten = TRUE)]

  class(random) <- c("estimate_grouplevel", class(random))
  random
}


.estimate_grouplevel_bayesian <- function(model, type = "random", ...) {
  param_names <- insight::clean_parameters(model)
  posteriors <- insight::get_parameters(model, effects = "all", component = "all", ...)
}
