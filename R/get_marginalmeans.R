#' @keywords internal
.get_marginalmeans <- function(model,
                               by = "auto",
                               ci = 0.95,
                               hypothesis = NULL,
                               ...) {
  # check if available
  insight::check_if_installed("marginaleffects")

  # Guess arguments
  my_args <- .guess_arguments_means(model, by, ...)

  # find default response-type
  type <- .get_type_argument(model, ...)

  # Get corresponding datagrid (and deal with particular ats)
  datagrid <- insight::get_datagrid(model, by = my_args$by, factors = "all", include_random = TRUE, ...)
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
    type = type,
    hypothesis = hypothesis
  )
  fun_args <- insight::compact_list(c(fun_args, list(...)))

  ## TODO: need to check against different mixed models results from other packages
  # set to NULL
  fun_args$re.form = NULL

  # we can use this function for contrasts as well,
  # just need to add "hypothesis" argument
  means <- suppressWarnings(do.call(marginaleffects::avg_predictions, fun_args))

  attr(means, "at") <- my_args$by
  attr(means, "by") <- my_args$by
  attr(means, "focal_terms") <- at_specs$varname
  means
}


# Format ------------------------------------------------------------------


#' @keywords internal
.format_marginaleffects_means <- function(means, model, ...) {
  # model information
  model_data <- insight::get_data(model)
  info <- insight::model_info(model)
  non_focal <- setdiff(colnames(model_data), attr(means, "focal_terms"))

  # estimate name
  if (info$is_binomial || info$is_bernoulli) {
    estimate_name <- "Probability"
  } else {
    estimate_name <- "Mean"
  }

  # Format
  params <- suppressWarnings(parameters::model_parameters(means, verbose = FALSE))
  params <- datawizard::data_relocate(params, c("Predicted", "SE", "CI_low", "CI_high"), after = -1, verbose = FALSE) # nolint
  params <- datawizard::data_rename(params, "Predicted", estimate_name)
  params <- datawizard::data_remove(params, c("p", "Statistic", "s.value", "S", "CI", "df", "rowid_dedup", non_focal), verbose = FALSE) # nolint
  params <- datawizard::data_restoretype(params, model_data)

  # Store info
  attr(params, "at") <- attr(means, "by")
  attr(params, "by") <- attr(means, "by")
  params
}

# Guess -------------------------------------------------------------------

#' @keywords internal
.guess_arguments_means <- function(model, by = NULL, ...) {
  # Gather info and data from model
  predictors <- insight::find_predictors(model, flatten = TRUE, ...)
  model_data <- insight::get_data(model)

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
