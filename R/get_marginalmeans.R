#' Easy 'avg_predictions' and 'avg_slopes'
#'
#' The `get_marginalmeans()` function is a wrapper to facilitate the usage of
#' `marginaleffects::avg_predictions()` and `marginaleffects::avg_slopes()`,
#' providing a somewhat simpler and intuitive API to find the specifications and
#' variables of interest. It is meanly made to for the developers to facilitate
#' the organization and debugging, and end-users should rather use the
#' `estimate_*()` series of functions.
#'
#' @param model A statistical model.
#' @param transform Can be used to easily modulate the `type` argument in
#' `marginaleffects::avg_predictions()`. Can be `"none"` or `"response"`.
#' `"none"` will leave the values on scale of the linear predictors.
#' `"response"` will transform them on scale of the response variable. Thus for
#' a logistic model, `"none"` will give estimations expressed in log-odds
#' (probabilities on logit scale) and `"response"` in terms of probabilities.
#' @param by The predictor variable(s) at which to evaluate the desired effect
#' / mean / contrasts. Other predictors of the model that are not included
#' here will be collapsed and "averaged" over (the effect will be estimated
#' across them).
#' @param ci Level for confidence intervals.
#' @param ... Other arguments passed, for instance, to [insight::get_datagrid()]
#' or [marginaleffects::avg_predictions()].
#'
#' @examplesIf insight::check_if_installed("marginaleffects", quietly = TRUE)
#' model <- lm(Sepal.Length ~ Species + Petal.Width, data = iris)
#'
#' # By default, 'by' is set to "Species"
#' get_marginalmeans(model)
#'
#' # Overall mean (close to 'mean(iris$Sepal.Length)')
#' get_marginalmeans(model, by = NULL)
#'
#' # One can estimate marginal means at several values of a 'modulate' variable
#' get_marginalmeans(model, by = "Petal.Width", length = 3)
#'
#' # Interactions
#' model <- lm(Sepal.Width ~ Species * Petal.Length, data = iris)
#'
#' get_marginalmeans(model)
#' get_marginalmeans(model, by = c("Species", "Petal.Length"), length = 2)
#' get_marginalmeans(model, by = c("Species", "Petal.Length = c(1, 3, 5)"), length = 2)
#' @export
get_marginalmeans <- function(model,
                              by = "auto",
                              transform = NULL,
                              ci = 0.95,
                              ...) {
  # check if available
  insight::check_if_installed("marginaleffects")
  dots <- list(...)

  # Guess arguments
  my_args <- .guess_arguments_means(model, by, ...)

  # find default response-type
  type <- .get_type_argument(model, transform, ...)

  # setup arguments
  dg_args <- list(
    model,
    by = my_args$by,
    factors = "all",
    include_random = TRUE
  )
  # always show all theoretical values by default
  if (is.null(dots$preserve_range)) {
    dg_args$preserve_range <- FALSE
  }
  # add user-arguments from "...", but remove those arguments that are already set
  dots[c("by", "factors", "include_random")] <- NULL
  dg_args <- insight::compact_list(c(dg_args, dots))

  # Get corresponding datagrid (and deal with particular ats)
  datagrid <- do.call(insight::get_datagrid, dg_args)
  at_specs <- attributes(datagrid)$at_specs

  # model df
  dof <- insight::get_df(model, verbose = FALSE)

  # setup arguments
  fun_args <- list(
    model,
    by = at_specs$varname,
    newdata = as.data.frame(datagrid),
    conf_level = ci,
    df = dof,
    type = type
  )
  # add user-arguments from "...", but remove those arguments that are already set
  dots[c("by", "newdata", "conf_level", "df", "type", "verbose")] <- NULL
  fun_args <- insight::compact_list(c(fun_args, dots))

  ## TODO: need to check against different mixed models results from other packages
  # set to NULL
  fun_args$re.form <- NULL

  # we can use this function for contrasts as well,
  # just need to add "hypothesis" argument
  means <- suppressWarnings(do.call(marginaleffects::avg_predictions, fun_args))

  attr(means, "at") <- my_args$by
  attr(means, "by") <- my_args$by
  attr(means, "focal_terms") <- at_specs$varname
  attr(means, "datagrid") <- datagrid
  means
}

#' @rdname get_marginalmeans
#' @export
model_marginalmeans <- get_marginalmeans


# Format ------------------------------------------------------------------


#' @keywords internal
.format_marginaleffects_means <- function(means, model, transform = NULL, ...) {
  # model information
  model_data <- insight::get_data(model)
  info <- insight::model_info(model, verbose = FALSE)
  non_focal <- setdiff(colnames(model_data), attr(means, "focal_terms"))
  is_contrast_analysis <- !is.null(list(...)$hypothesis)

  # do we have contrasts? For contrasts, we want to keep p-values
  if (is_contrast_analysis) {
    remove_column <- "SE"
    estimate_name <- "Difference"
  } else {
    remove_column <- "p"
    # estimate name
    if (!identical(transform, "none") && (info$is_binomial || info$is_bernoulli)) {
      estimate_name <- "Probability"
    } else {
      estimate_name <- "Mean"
    }
  }

  # Format
  params <- suppressWarnings(parameters::model_parameters(means, verbose = FALSE))
  # add ci?
  params <- .add_contrasts_ci(is_contrast_analysis, params)
  params <- datawizard::data_relocate(params, c("Predicted", "SE", "CI_low", "CI_high"), after = -1, verbose = FALSE) # nolint
  # move p to the end
  params <- datawizard::data_relocate(params, "p", after = -1, verbose = FALSE)
  params <- datawizard::data_rename(params, "Predicted", estimate_name)
  # remove redundant columns
  params <- datawizard::data_remove(params, c(remove_column, "Statistic", "s.value", "S", "CI", "df", "rowid_dedup", non_focal), verbose = FALSE) # nolint
  params <- datawizard::data_restoretype(params, model_data)

  # Store info
  attr(params, "at") <- attr(means, "by")
  attr(params, "by") <- attr(means, "by")
  params
}


#' @keywords internal
.add_contrasts_ci <- function(is_contrast_analysis, params) {
  if (is_contrast_analysis && !"CI_low" %in% colnames(params) && "SE" %in% colnames(params)) {
    # extract ci-level
    if ("CI" %in% colnames(params)) {
      ci <- params[["CI"]][1]
    } else {
      ci <- attributes(params)$ci
    }
    if (is.null(ci)) {
      ci <- 0.95
    }
    # get degrees of freedom
    if ("df" %in% colnames(params)) {
      dof <- params[["df"]]
    } else {
      dof <- Inf
    }
    # critical test value
    crit <- stats::qt((1 + ci) / 2, df = dof)
    # add CI
    params$CI_low <- params$Predicted - crit * params$SE
    params$CI_high <- params$Predicted + crit * params$SE
  }
  params
}



# Guess -------------------------------------------------------------------

#' @keywords internal
.guess_arguments_means <- function(model, by = NULL, ...) {
  # Gather info and data from model
  model_data <- insight::get_data(model)
  predictors <- intersect(
    colnames(model_data),
    insight::find_predictors(model, effects = "fixed", flatten = TRUE, ...)
  )

  # Guess arguments 'by'
  if (identical(by, "auto")) {
    # Find categorical predictors
    by <- predictors[!vapply(model_data[predictors], is.numeric, logical(1))]
    if (!length(by) || all(is.na(by))) {
      insight::format_error("Model contains no categorical predictor. Please specify `by`.")
    }
    insight::format_alert(paste0("We selected `by = c(", toString(paste0('"', by, '"')), ")`."))
  }
  list(by = by)
}
