#' Group-specific parameters of mixed models random effects
#'
#' Extract random parameters of each individual group in the context of mixed
#' models. Can be reshaped to be of the same dimensions as the original data,
#' which can be useful to add the random effects to the original data.
#'
#' @param model A mixed model with random effects.
#' @param type If `"random"` (default), the coefficients correspond to the
#'   conditional estimates of  the random effects (as they are returned by
#'   `lme4::ranef()`). They typically correspond to the deviation of each
#'   individual group from their fixed effect. As such, a coefficient close to 0
#'   means that the participants' effect is the same as the population-level
#'   effect (in other words, it is "in the norm"). If `"total"`, it will return
#'   the sum of the random effect and its corresponding fixed effects, which
#'   corresponds to `coef()` (see `?coef.merMod`). Note that `type = "total"`
#'   currently does not return uncertainty indices (such as SE and CI) for
#'   models from *lme4* or *glmmTMB*, as these are not computable. However, for
#'   Bayesian models, it is possible to compute them.
#' @param ... Other arguments passed to or from other methods.
#'
#' @examplesIf all(insight::check_if_installed(c("see", "lme4"), quietly = TRUE)) && packageVersion("insight") > "1.1.0" && packageVersion("parameters") > "0.24.1"
#' # lme4 model
#' data(mtcars)
#' model <- lme4::lmer(mpg ~ hp + (1 | carb), data = mtcars)
#' random <- estimate_grouplevel(model)
#'
#' # Show group-specific effects
#' random
#'
#' # Visualize random effects
#' plot(random)
#'
#' # Reshape to wide data so that it matches the original dataframe...
#' reshaped <- reshape_grouplevel(random, indices = c("Coefficient", "SE"))
#'
#' # ...and can be easily combined with the original data
#' alldata <- cbind(mtcars, reshaped)
#'
#' # Use summary() to remove duplicated rows
#' summary(reshaped)
#'
#' # overall coefficients
#' estimate_grouplevel(model, type = "total")
#' @export
estimate_grouplevel <- function(model, type = "random", ...) {
  # validate argument
  type <- insight::validate_argument(type, c("random", "total"))

  # Extract params
  params <- parameters::model_parameters(
    model,
    effects = ifelse(type == "random", "all", "total"),
    group_level = identical(type, "random"),
    ...
  )

  # Re-add info
  if (!"Group" %in% names(params)) {
    params$Group <- attributes(params)$clean_parameters$Group
  }
  if (!"Level" %in% names(params)) {
    params$Level <- attributes(params)$clean_parameters$Cleaned_Parameter
  }

  # TODO: improve / add new printing that groups by group/level?
  random <- as.data.frame(params[params$Effects == type, ])

  # Remove columns with only NaNs (as these are probably those of fixed effects)
  random[vapply(random, function(x) all(is.na(x)), TRUE)] <- NULL

  # Filter more columns
  random <- random[, grepl("Group|Level|Name|Parameter|Component|Median|Mean|MAP|Coefficient|CI|SE", names(random))]

  # Clean
  row.names(random) <- NULL
  random$Effects <- NULL
  if ("Component" %in% names(random) && insight::n_unique(random$Component) == 1 && unique(random$Component) == "conditional") {
    random$Component <- NULL
  }

  # Reorganize columns
  random <- datawizard::data_relocate(random, c("Component", "Group", "Level", "Parameter"), verbose = FALSE)

  # Clean-up brms output
  if (inherits(model, "brmsfit") && type == "random") {
    # Save brms name (just in case)
    random$Name <- random$Parameter
    # Filter out non-random effects
    random <- random[grepl("^r_", random$Parameter), ]
    # Remove Group from Level
    random$Level <- sapply(1:nrow(random), function(i) gsub(paste0("^", random$Group[i], "\\."), "", random$Level[i]))
    # Find the group name (what follows "r_" and before the first "[" or "__")
    random$Group <- gsub("^r_(.*?)(\\[.*|__.*)", "\\1", random$Name)
    # Keep Parameter what's in between [ and ]
    random$Parameter <- gsub("^r_.*?\\[(.*?)\\].*", "\\1", random$Name)
    # Remove Level from it
    random$Parameter <- sapply(1:nrow(random), function(i) gsub(paste0("^", random$Level[i], "\\,"), "", random$Parameter[i]))
    # remove temporary name column
    random$Name <- NULL
  }

  # Sort
  if ("Component" %in% names(random)) {
    random <- random[order(random$Component, random$Group, datawizard::coerce_to_numeric(random$Level), random$Parameter), ]
  } else {
    random <- random[order(random$Group, datawizard::coerce_to_numeric(random$Level), random$Parameter), ]
  }

  # find data and random effects
  model_data <- insight::get_data(model, source = "mf", verbose = FALSE)
  model_random <- insight::find_random(model, split_nested = TRUE, flatten = TRUE)

  # Assign new class
  attr(random, "type") <- type
  attr(random, "model") <- model
  attr(random, "parameters") <- params
  attr(random, "coef_name") <- intersect(.valid_coefficient_names(), colnames(random))
  attr(random, "data") <- .safe(model_data[model_random])

  class(random) <- c("estimate_grouplevel", class(random))
  random
}
