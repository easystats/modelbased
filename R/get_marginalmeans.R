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
  datagrid <- datagrid[insight::find_predictors(model, effects="fixed", flatten = TRUE)]
  at_specs <- attributes(datagrid)$at_specs


  if (marginal == FALSE) {
    if(insight::is_mixed_model(model)) {
      means <- marginaleffects::predictions(model,
                                            newdata=datagrid,
                                            by=at_specs$varname,
                                            conf_level = ci,
                                            re.form=NA)
    } else {
      means <- marginaleffects::predictions(model,
                                            newdata=datagrid,
                                            by=at_specs$varname,
                                            conf_level = ci)
    }
  } else {
    means <- marginaleffects::predictions(model,
                                          newdata=insight::get_data(model),
                                          by=at_specs$varname,
                                          conf_level = ci)
  }
  attr(means, "at") <- args$at
  means
}


# Format ------------------------------------------------------------------


#' @keywords internal
.format_marginaleffects_means <- function(means, model, ...) {
  # Format
  params <- parameters::parameters(means) |>
    datawizard::data_relocate(c("Predicted", "SE", "CI_low", "CI_high"), after=-1) |>
    datawizard::data_rename("Predicted", "Mean") |>
    datawizard::data_remove(c("p", "Statistic", "s.value", "S", "CI")) |>
    datawizard::data_restoretype(insight::get_data(model))

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
    at <- predictors[!sapply(data[predictors], is.numeric)]
    if (!length(at) || all(is.na(at))) {
      stop("Model contains no categorical factor. Please specify 'at'.", call. = FALSE)
    }
    message("We selected `at = c(", toString(paste0('"', at, '"')), ")`.")
  }

  list(at=at)
}
