#' @keywords internal
.get_marginalmeans <- function(model,
                               by = "auto",
                               ci = 0.95,
                               marginal = FALSE,
                               ...) {
  # check if available
  insight::check_if_installed("marginaleffects")

  # Guess arguments
  my_args <- .guess_arguments_means(model, by, ...)

  # Get corresponding datagrid (and deal with particular ats)
  datagrid <- insight::get_datagrid(model, by = my_args$by, ...)
  # Drop random effects
  datagrid <- datagrid[insight::find_predictors(model, effects = "fixed", flatten = TRUE)]
  at_specs <- attributes(datagrid)$at_specs

  ## TODO: extract correct type argument
  means <- marginaleffects::avg_predictions(
    model,
    by = at_specs$varname,
    conf_level = ci
  )

  attr(means, "at") <- my_args$by
  attr(means, "by") <- my_args$by
  means
}


# Format ------------------------------------------------------------------


#' @keywords internal
.format_marginaleffects_means <- function(means, model, ...) {
  # check if available
  insight::check_if_installed("datawizard")

  # Format
  params <- parameters::parameters(means)
  params <- datawizard::data_relocate(params, c("Predicted", "SE", "CI_low", "CI_high"), after = -1)
  params <- datawizard::data_rename(params, "Predicted", "Mean")
  params <- datawizard::data_remove(params, c("p", "Statistic", "s.value", "S", "CI"))
  params <- datawizard::data_restoretype(params, insight::get_data(model))

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

  # Guess arguments ('by' and 'fixed')
  if (identical(by, "auto")) {
    # Find categorical predictors
    by <- predictors[!vapply(model_data[predictors], is.numeric, logical(1))]
    if (!length(by) || all(is.na(by))) {
      insight::format_error("Model contains no categorical factor. Please specify 'by'.")
    }
    insight::format_alert(paste0("We selected `by = c(", toString(paste0('"', by, '"')), ")`."))
  }

  list(by = by)
}
