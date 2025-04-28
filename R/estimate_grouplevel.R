#' Group-specific parameters of mixed models random effects
#'
#' Extract random parameters of each individual group in the context of mixed
#' models, commonly referred to as BLUPs (Best Linear Unbiased Predictors).
#' Can be reshaped to be of the same dimensions as the original data,
#' which can be useful to add the random effects to the original data.
#'
#' @param model A mixed model with random effects.
#' @param type `"random"` or `"total"`. If `"random"` (default), the
#'   coefficients correspond to the conditional estimates of  the random effects
#'   (as they are returned by `lme4::ranef()`). They typically correspond to the
#'   deviation of each individual group from their fixed effect (assuming the
#'   random effect is also included as a fixed effect). As such, a coefficient
#'   close to 0 means that the participants' effect is the same as the
#'   population-level effect (in other words, it is "in the norm"). If
#'   `"total"`, it will return the sum of the random effect and its
#'   corresponding fixed effects, which internally relies on the `coef()` method
#'   (see `?coef.merMod`). Note that `type = "total"` yet does not return
#'   uncertainty indices (such as SE and CI) for models from *lme4* or
#'   *glmmTMB*, as the necessary information to compute them is not yet
#'   available. However, for Bayesian models, it is possible to compute them.
#' @param dispersion,test,diagnostic Arguments passed to
#'    [parameters::model_parameters()] for Bayesian models. By default, it won't
#'    return significance or diagnostic indices (as it is not typically very
#'    useful).
#' @param ... Other arguments passed to [parameters::model_parameters()].
#'
#' @details
#' Unlike raw group means, BLUPs apply shrinkage: they are a compromise between
#' the group estimate and the population estimate. This improves generalizability
#' and prevents overfitting.
#'
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
#' # Reshape to wide data...
#' reshaped <- reshape_grouplevel(random, group = "carb", indices = c("Coefficient", "SE"))
#'
#' # ...and can be easily combined with the original data
#' alldata <- merge(mtcars, reshaped)
#'
#' # overall coefficients
#' estimate_grouplevel(model, type = "total")
#' @export
estimate_grouplevel <- function(model, ...) {
  UseMethod("estimate_grouplevel")
}


#' @rdname estimate_grouplevel
#' @export
estimate_grouplevel.default <- function(model,
                                        type = "random",
                                        ...) {
  # validate argument
  type <- insight::validate_argument(type, c("random", "total"))

  # sanity check
  if (is.null(insight::find_random(model))) {
    insight::format_error("Model must be a mixed model with random effects.")
  }

  # Extract params
  params <- parameters::model_parameters(
    model,
    effects = ifelse(type == "random", "all", "total"),
    group_level = identical(type, "random"),
    ...
  )

  # get cleaned parameter names with additional information
  clean_parameters <- attributes(params)$clean_parameters

  # Re-add info
  if (!"Group" %in% names(params) && !is.null(clean_parameters)) {
    params$Group <- clean_parameters$Group
  }
  if (!"Level" %in% names(params) && !is.null(clean_parameters)) {
    params$Level <- clean_parameters$Cleaned_Parameter
  }

  # TODO: improve / add new printing that groups by group/level?
  random <- as.data.frame(params[params$Effects == type, ])

  # Remove columns with only NaNs (as these are probably those of fixed effects)
  random[vapply(random, function(x) all(is.na(x)), TRUE)] <- NULL

  # Clean and Reorganize columns
  random <- .clean_grouplevel(random)

  # Sort
  random <- .sort_random_effects(random)

  # add data and attributes
  .add_grouplevel_attributes(model, type, params, random)
}


#' @rdname estimate_grouplevel
#' @export
estimate_grouplevel.brmsfit <- function(model,
                                        type = "random",
                                        dispersion = TRUE,
                                        test = NULL,
                                        diagnostic = NULL,
                                        ...) {
  # validate argument
  type <- insight::validate_argument(type, c("random", "total"))

  # sanity check
  if (is.null(insight::find_random(model))) {
    insight::format_error("Model must be a mixed model with random effects.")
  }

  # Extract params
  params <- parameters::model_parameters(
    model,
    ## TODO: replace "all" by "full" once insight > 1.2.0 is on CRAN
    effects = ifelse(type == "random", "all", "total"),
    group_level = identical(type, "random"),
    dispersion = dispersion,
    test = test,
    diagnostic = diagnostic,
    ...
  )

  # get cleaned parameter names with additional information
  clean_parameters <- attributes(params)$clean_parameters

  # Re-add info
  if (!"Group" %in% names(params) && !is.null(clean_parameters)) {
    params$Group <- clean_parameters$Group
  }
  if (!"Level" %in% names(params) && !is.null(clean_parameters)) {
    params$Level <- clean_parameters$Cleaned_Parameter
  }

  # TODO: improve / add new printing that groups by group/level?
  random <- as.data.frame(params[params$Effects == type, ])

  # Remove columns with only NaNs (as these are probably those of fixed effects)
  random[vapply(random, function(x) all(is.na(x)), TRUE)] <- NULL

  # Clean and Reorganize columns
  random <- .clean_grouplevel(random)

  # Clean-up brms output
  if (type == "random") {
    # Save brms name (just in case)
    random$Name <- random$Parameter
    # Filter out non-random effects
    random <- random[startsWith(random$Parameter, "r_"), ]
    # Remove Group from Level
    random$Level <- sapply(
      1:nrow(random),
      function(i) gsub(paste0("^", random$Group[i], "\\."), "", random$Level[i])
    )
    # Find the group name (what follows "r_" and before the first "[" or "__")
    random$Group <- gsub("^r_(.*?)(\\[.*|__.*)", "\\1", random$Name)
    # Keep Parameter what's in between [ and ]
    random$Parameter <- gsub("^r_.*?\\[(.*?)\\].*", "\\1", random$Name)
    # Remove Level from it
    random$Parameter <- sapply(
      1:nrow(random),
      function(i) gsub(paste0("^", random$Level[i], "\\,"), "", random$Parameter[i])
    )
    # remove temporary name column
    random$Name <- NULL
  }

  # Sort
  random <- .sort_random_effects(random)

  # add data and attributes
  .add_grouplevel_attributes(model, type, params, random)
}


#' @export
estimate_grouplevel.stanreg <- function(model,
                                        type = "random",
                                        dispersion = TRUE,
                                        test = NULL,
                                        diagnostic = NULL,
                                        ...) {
  # validate argument
  type <- insight::validate_argument(type, c("random", "total"))

  # sanity check
  if (is.null(insight::find_random(model))) {
    insight::format_error("Model must be a mixed model with random effects.")
  }

  # Extract params
  params <- parameters::model_parameters(
    model,
    effects = ifelse(type == "random", "all", "total"),
    group_level = identical(type, "random"),
    dispersion = dispersion,
    test = test,
    diagnostic = diagnostic,
    drop = "^Sigma\\[",
    ...
  )

  # get cleaned parameter names with additional information
  clean_parameters <- attributes(params)$clean_parameters

  ## TODO: for now, rstanarm has no random effects on the sigma parameter
  ## however, if this changes, we need another solution here (and in
  ## insight::clean_parameter())
  if (!is.null(clean_parameters)) {
    # fix for rstanarm, which contains a sigma columns
    clean_parameters <- clean_parameters[
      clean_parameters$Component != "sigma" & !startsWith(clean_parameters$Parameter, "Sigma["), # nolint
    ]

    params$Parameter <- insight::trim_ws(sub(":.*", "", clean_parameters$Group))
    params$Group <- insight::trim_ws(sub("^[^:]*:", "", clean_parameters$Group))
    params$Level <- insight::trim_ws(sub("^[^:]*:", "", clean_parameters$Cleaned_Parameter))
  }

  # TODO: improve / add new printing that groups by group/level?
  random <- as.data.frame(params[params$Effects == type, ])

  # Remove columns with only NaNs (as these are probably those of fixed effects)
  random[vapply(random, function(x) all(is.na(x)), TRUE)] <- NULL

  # Clean and Reorganize columns
  random <- .clean_grouplevel(random)

  # Sort
  random <- .sort_random_effects(random)

  # add data and attributes
  .add_grouplevel_attributes(model, type, params, random)
}


# helpers ----------------------------------------------------------

.add_grouplevel_attributes <- function(model, type, params, random) {
  model_data <- insight::get_data(model, source = "mf", verbose = FALSE)
  model_random <- insight::find_random(model, split_nested = TRUE, flatten = TRUE)

  # Assign new class
  attr(random, "type") <- type
  attr(random, "model") <- model
  attr(random, "parameters") <- params
  attr(random, "coef_name") <- intersect(.valid_coefficient_names(model), colnames(random))
  attr(random, "data") <- .safe(model_data[model_random])

  class(random) <- c("estimate_grouplevel", class(random))
  random
}


.clean_grouplevel <- function(random) {
  row.names(random) <- NULL
  random$Effects <- NULL
  if (
    "Component" %in%
      names(random) &&
      insight::has_single_value(random$Component, remove_na = TRUE) &&
      unique(random$Component) == "conditional"
  ) {
    random$Component <- NULL
  }

  # Reorganize columns
  random <- datawizard::data_relocate(
    random,
    c("Component", "Group", "Level", "Parameter"),
    verbose = FALSE
  )
  random
}


.sort_random_effects <- function(random) {
  if ("Component" %in% names(random)) {
    random <- random[
      order(
        random$Component,
        random$Group,
        datawizard::coerce_to_numeric(random$Level),
        random$Parameter
      ),
    ]
  } else {
    random <- random[order(random$Group, datawizard::coerce_to_numeric(random$Level), random$Parameter), ] # nolint
  }
  random
}
