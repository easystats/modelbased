#' @rdname get_emmeans
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
                              predict = NULL,
                              ci = 0.95,
                              transform = NULL,
                              verbose = TRUE,
                              ...) {
  # check if available
  insight::check_if_installed("marginaleffects")
  dots <- list(...)

  ## TODO: remove deprecation warning later
  if (!is.null(transform)) {
    insight::format_warning("Argument `transform` is deprecated. Please use `predict` instead.")
    predict <- transform
  }

  # Guess arguments
  my_args <- .guess_marginaleffects_arguments(model, by, verbose = verbose, ...)

  # find default response-type
  predict <- .get_marginaleffects_type_argument(model, predict, ...)


  # First step: create a data grid --------------------------------------------
  # ---------------------------------------------------------------------------

  # exception: by = NULL computes overall mean
  if (is.null(by)) {
    datagrid <- at_specs <- NULL
  } else {
    # setup arguments
    dg_args <- list(
      model,
      by = my_args$by,
      factors = "all",
      include_random = TRUE,
      verbose = FALSE
    )
    # always show all theoretical values by default
    if (is.null(dots$preserve_range)) {
      dg_args$preserve_range <- FALSE
    }
    # add user-arguments from "...", but remove those arguments that are already set
    dots[c("by", "factors", "include_random", "verbose")] <- NULL
    dg_args <- insight::compact_list(c(dg_args, dots))

    # Get corresponding datagrid (and deal with particular ats)
    datagrid <- do.call(insight::get_datagrid, dg_args)
    at_specs <- attributes(datagrid)$at_specs

    # restore data types -  if we have defined numbers in `by`, like
    # `by = "predictor = 5"`, and `predictor` was a factor, it is returned as
    # numeric in the data grid. Fix this here, else marginal effects will fail
    datagrid <- datawizard::data_restoretype(datagrid, insight::get_data(model))

    # add user-arguments from "...", but remove those arguments that are already set
    dots[c("by", "newdata", "conf_level", "df", "type", "verbose")] <- NULL
  }


  # Second step: prepare arguments for marginaleffects ------------------------
  # ---------------------------------------------------------------------------

  # model df
  dof <- insight::get_df(model, type = "wald", verbose = FALSE)

  # sanity check
  if (!is.null(datagrid)) {
    datagrid <- as.data.frame(datagrid)
  }

  # setup arguments - either for conditional or counterfactual predictions
  if (isTRUE(dots$counterfactual)) {
    # sanity check
    if (is.null(datagrid)) {
      insight::format_error("Could not create data grid based on variables selected in `by`. Please check if all `by` variables are present in the data set.") # nolint
    }
    fun_args <- list(
      model,
      variables = lapply(datagrid, unique),
      by = at_specs$varname,
      conf_level = ci,
      df = dof
    )
  } else {
    fun_args <- list(
      model,
      by = at_specs$varname,
      newdata = datagrid,
      conf_level = ci,
      df = dof
    )
  }

  # handle distributional parameters
  if (predict %in% .brms_aux_elements() && inherits(model, "brmsfit")) {
    fun_args$dpar <- predict
  } else {
    fun_args$type <- predict
  }

  # cleanup
  dots$counterfactual <- NULL
  fun_args <- insight::compact_list(c(fun_args, dots))

  ## TODO: need to check against different mixed models results from other packages
  # set to NULL
  if (!"re.form" %in% names(dots)) {
    fun_args$re.form <- NULL
  }


  # Third step: compute marginal means ----------------------------------------
  # ---------------------------------------------------------------------------

  # we can use this function for contrasts as well,
  # just need to add "hypothesis" argument
  means <- suppressWarnings(do.call(marginaleffects::avg_predictions, fun_args))


  # Last step: Save information in attributes  --------------------------------
  # ---------------------------------------------------------------------------

  attr(means, "at") <- my_args$by
  attr(means, "by") <- my_args$by
  attr(means, "focal_terms") <- at_specs$varname
  attr(means, "datagrid") <- datagrid
  attr(means, "preserve_range") <- attributes(datagrid)$preserve_range
  attr(means, "predict") <- predict
  class(means) <- unique(c("marginaleffects_means", class(means)))

  means
}


# Guess -------------------------------------------------------------------

#' @keywords internal
.guess_marginaleffects_arguments <- function(model, by = NULL, contrast = NULL, verbose = TRUE, ...) {
  # Gather info and data from model
  model_data <- insight::get_data(model)
  predictors <- intersect(
    colnames(model_data),
    insight::find_predictors(model, effects = "fixed", flatten = TRUE, ...)
  )

  validate_arg <- function(spec_value, spec_name) {
    if (identical(spec_value, "auto")) {
      # Find categorical predictors
      spec_value <- predictors[!vapply(model_data[predictors], is.numeric, logical(1))]
      if (!length(spec_value) || all(is.na(spec_value))) {
        insight::format_error(paste0(
          "Model contains no categorical predictor. Please specify `", spec_name, "`."
        ))
      }
      if (verbose) {
        insight::format_alert(paste0(
          "We selected `", spec_name, "=c(", toString(paste0('"', spec_value, '"')), ")`."
        ))
      }
    }
    spec_value
  }

  # Guess arguments 'by'
  by <- validate_arg(by, "by")
  # Guess arguments 'contrast'
  contrast <- validate_arg(contrast, "contrast")

  list(by = by, contrast = contrast)
}
