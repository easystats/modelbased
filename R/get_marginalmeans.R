#' @keywords internal
.get_marginalmeans <- function(model,
                               at = "auto",
                               ci = 0.95,
                               marginal = FALSE,
                               ...) {
  # check if available
  insight::check_if_installed("marginaleffects")

  # Guess arguments
  args <- .guess_arguments_means(model, at, ...)

  # Get corresponding datagrid (and deal with particular ats)
  datagrid <- insight::get_datagrid(model, at = args$at, ...)
  # Drop random effects
  datagrid <- datagrid[insight::find_predictors(model, effects = "fixed", flatten = TRUE)]
  at_specs <- attributes(datagrid)$at_specs

  if (marginal) {
    means <- marginaleffects::predictions(model,
      newdata = insight::get_data(model),
      by = at_specs$varname,
      conf_level = ci
    )
  } else if (insight::is_mixed_model(model)) {
    means <- marginaleffects::predictions(model,
      newdata = datagrid,
      by = at_specs$varname,
      conf_level = ci,
      re.form = NA
    )
  } else {
    means <- marginaleffects::predictions(model,
      newdata = datagrid,
      by = at_specs$varname,
      conf_level = ci
    )
  }
  attr(means, "at") <- args$at
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
  attr(params, "at") <- attr(means, "at")
  params
}

# Guess -------------------------------------------------------------------

#' @keywords internal
.guess_arguments_means <- function(model, at = NULL, ...) {
  # Gather info and data from model
  predictors <- insight::find_predictors(model, flatten = TRUE, ...)
  data <- insight::get_data(model)

  # Guess arguments ('at' and 'fixed')
  if (!is.null(at) && length(at) == 1 && at == "auto") {
    # Find categorical predictors
    at <- predictors[!vapply(data[predictors], is.numeric, logical(1))]
    if (!length(at) || all(is.na(at))) {
      insight::format_error("Model contains no categorical factor. Please specify 'at'.")
    }
    insight::format_alert("We selected `at = c(", toString(paste0('"', at, '"')), ")`.")
  }

  list(at = at)
}
