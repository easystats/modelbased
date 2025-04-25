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
#' \dontrun{
#' # One can estimate marginal means at several values of a 'modulate' variable
#' get_marginalmeans(model, by = "Petal.Width", length = 3)
#'
#' # Interactions
#' model <- lm(Sepal.Width ~ Species * Petal.Length, data = iris)
#'
#' get_marginalmeans(model)
#' get_marginalmeans(model, by = c("Species", "Petal.Length"), length = 2)
#' get_marginalmeans(model, by = c("Species", "Petal.Length = c(1, 3, 5)"), length = 2)
#' }
#' @export
get_marginalmeans <- function(model,
                              by = "auto",
                              predict = NULL,
                              ci = 0.95,
                              estimate = NULL,
                              transform = NULL,
                              keep_iterations = FALSE,
                              verbose = TRUE,
                              ...) {
  # check if available
  insight::check_if_installed("marginaleffects")

  # First step: process arguments --------------------------------------------
  # --------------------------------------------------------------------------

  dots <- list(...)
  comparison <- dots$hypothesis

  # set defaults
  if (is.null(estimate)) {
    estimate <- getOption("modelbased_estimate", "typical")
  }

  # validate input
  estimate <- insight::validate_argument(
    estimate,
    c("typical", "population", "specific", "average")
  )

  # model details
  model_info <- insight::model_info(model, response = 1, verbose = FALSE)

  # Guess arguments
  my_args <- .guess_marginaleffects_arguments(model, by, verbose = verbose, ...)

  # inform user about appropriate use of offset-terms
  .check_offset(model, estimate, offset = dots$offset, verbose = verbose)

  # find default response-type, and get information about back transformation
  predict_args <- .get_marginaleffects_type_argument(
    model,
    predict,
    comparison,
    model_info,
    verbose,
    ...
  ) # nolint

  # Second step: create a data grid -------------------------------------------
  # ---------------------------------------------------------------------------

  # exception: by = NULL computes overall mean
  if (is.null(by)) {
    datagrid <- datagrid_info <- NULL
  } else {
    # setup arguments to create the data grid
    dg_factors <- switch(estimate,
      specific = "reference",
      "all"
    )
    dg_args <- list(
      model,
      by = my_args$by,
      factors = dg_factors,
      include_random = TRUE,
      verbose = FALSE
    )
    # did user request weights? These are not supported for data-grid
    # marginalization types
    if (estimate %in% c("specific", "typical") && (!is.null(dots$weights) || !is.null(dots$wts))) {
      insight::format_warning("Using weights is not possible when `estimate` is set to \"typical\" or \"specific\". Use `estimate = \"average\"` to include weights for marginal means or contrasts.") # nolint
      dots[c("weights", "wts")] <- NULL
    }

    # always show all theoretical values by default
    if (is.null(dots$preserve_range)) {
      dg_args$preserve_range <- FALSE
    }
    # add user-arguments from "...", but remove those arguments that are already set
    dots[c("by", "factors", "include_random", "verbose")] <- NULL
    dg_args <- insight::compact_list(c(dg_args, dots))

    # Get corresponding datagrid (and deal with particular ats)
    datagrid <- do.call(insight::get_datagrid, dg_args)
    datagrid_info <- attributes(datagrid)

    # handle offsets
    if (!is.null(dots$offset)) {
      model_offset <- insight::find_offset(model)
      if (!is.null(model_offset)) {
        datagrid[[model_offset]] <- dots$offset
      }
    }

    # restore data types -  if we have defined numbers in `by`, like
    # `by = "predictor = 5"`, and `predictor` was a factor, it is returned as
    # numeric in the data grid. Fix this here, else marginal effects will fail
    datagrid <- datawizard::data_restoretype(
      datagrid,
      insight::get_data(model, verbose = FALSE)
    )
  }

  # Third step: prepare arguments for marginaleffects ------------------------
  # --------------------------------------------------------------------------

  # remove user-arguments from "..." that will be used when calling marginaleffects
  dots[c("by", "conf_level", "type", "digits", "bias_correction", "sigma", "offset")] <- NULL

  # model df - can be passed via `...`
  if (is.null(dots$df) && model_info$is_bayesian) {
    dots$df <- insight::get_df(model, type = "wald", verbose = FALSE)
  }

  # sanity check
  if (!is.null(datagrid)) {
    datagrid <- datawizard::data_arrange(
      as.data.frame(datagrid),
      select = datagrid_info$at_specs$varname
    )
  }

  # setup arguments
  fun_args <- list(model, conf_level = ci)

  # handle variables/by/newdata
  # ---------------------------

  # counterfactual predictions - we need the "variables" argument
  if (estimate == "population") {
    # sanity check
    if (is.null(datagrid)) {
      insight::format_error("Could not create data grid based on variables selected in `by`. Please check if all `by` variables are present in the data set.") # nolint
    }
    fun_args$variables <- lapply(datagrid, unique)[datagrid_info$at_specs$varname]
  } else {
    # all other "marginalizations"
    # we don't want a datagrid for "average" option
    if (is.null(dots$newdata) && estimate != "average") {
      # we allow individual "newdata" options, so do not
      # # overwrite if explicitly set
      fun_args$newdata <- datagrid
    }
    fun_args$by <- datagrid_info$at_specs$varname
  }

  # handle brms auxiliary
  # ---------------------------

  # handle distributional parameters
  if (predict_args$predict %in% .brms_aux_elements(model) && inherits(model, "brmsfit")) {
    fun_args$dpar <- predict_args$predict
  } else {
    fun_args$type <- predict_args$predict
  }

  # weights?
  # ---------------------------

  # handle weights - argument is named "wts" in marginal effects
  if (!is.null(dots$weights)) {
    dots$wts <- dots$weights
    dots$weights <- NULL
  }

  # =========================================================================
  # only needed to estimate_contrasts() with custom hypothesis ==============
  # =========================================================================
  # for custom hypothesis, like "b2=b5" or "(b2-b1)=(b4-b3)", we need to renumber
  # the b-values internally, because we have a different sorting in our output
  # compared to what "avg_predictions()" returns... so let's check if we have to
  # take care of this
  if (.is_custom_comparison(comparison)) {
    dots$hypothesis <- .reorder_custom_hypothesis(
      comparison,
      datagrid,
      focal = datagrid_info$at_specs$varname
    )
  }

  # cleanup
  fun_args <- insight::compact_list(c(fun_args, dots))

  ## TODO: need to check against different mixed models results from other packages
  # set to NULL
  if (!"re.form" %in% names(dots)) {
    fun_args$re.form <- NULL
  }

  # transform reponse?
  if (isTRUE(transform)) {
    transform <- insight::get_transformation(model, verbose = FALSE)$inverse
  }
  if (!is.null(transform)) {
    fun_args$transform <- transform
  }

  # Fourth step: compute marginal means ---------------------------------------
  # ---------------------------------------------------------------------------

  # we can use this function for contrasts as well,
  # just need to add "hypothesis" argument
  means <- .call_marginaleffects(fun_args)

  # Fifth step: post-processin marginal means----------------------------------
  # ---------------------------------------------------------------------------

  # filter "by" rows when we have "average" marginalization, because we don't
  # pass data grid in such situations - but we still created the data grid based
  # on the `by` variables, for internal use, for example filtering at this point
  means <- .filter_datagrid_average(means, estimate, datagrid, datagrid_info)

  # back-transform from link-scale? this functions is...
  # - only called for means, not contrasts, because for contrasts we rely on
  #   the delta-method for SEs on the response scale
  # - only called when `type` (i.e. `predict`) is "response" AND the model class
  #   has a "link" prediction type
  if (predict_args$backtransform) {
    means <- .backtransform_predictions(means, model, predict_args, ci, df = dots$df)
    # make sure we have the original string value for the "predict" argument
    predict_args$predict <- "response"
  }

  # =========================================================================
  # only needed to estimate_contrasts() with custom hypothesis ==============
  # =========================================================================
  # fix term label for custom hypothesis
  if (.is_custom_comparison(comparison)) {
    ## TODO: check which column name is used in marginaleffects update, and
    ## keep only the new one later - or for safety, we can keep both code lines
    means$term <- gsub(" ", "", comparison, fixed = TRUE)
    means$hypothesis <- gsub(" ", "", comparison, fixed = TRUE)
  }

  # Last step: Save information in attributes  --------------------------------
  # ---------------------------------------------------------------------------

  means <- .add_attributes(
    means,
    by = my_args$by,
    model_info = model_info,
    info = c(
      datagrid_info,
      list(
        predict = predict_args$predict,
        estimate = estimate,
        datagrid = datagrid,
        transform = !is.null(transform),
        keep_iterations = keep_iterations
      )
    )
  )
  class(means) <- unique(c("marginaleffects_means", class(means)))

  means
}


# call marginaleffects and process potential errors ---------------------------


.call_marginaleffects <- function(fun_args, type = "means") {
  out <- tryCatch(
    suppressWarnings(do.call(marginaleffects::avg_predictions, fun_args)),
    error = function(e) e
  )

  # display informative error
  if (inherits(out, "simpleError")) {
    insight::format_error(.marginaleffects_errors(out, fun_args))
  }

  out
}


.marginaleffects_errors <- function(out, fun_args) {
  # what was requested?
  if (!is.null(fun_args$hypothesis)) {
    fun <- "marginal contrasts"
  } else {
    fun <- "marginal means"
  }
  # clean original error message
  out$message <- gsub("\\s+", " ", gsub("\n", "", out$message))
  # setup clear error message
  msg <- c(
    paste0("Sorry, calculating ", fun, " failed with following error:"),
    insight::color_text(gsub("\n", "", out$message, fixed = TRUE), "red")
  )
  # handle exceptions ------------------------------------------------------
  # we get this error when we should use counterfactuals
  if (grepl("not found in column names", out$message, fixed = TRUE)) {
    msg <- c(msg, "\nIt seems that not all required levels of the focal terms are available in the provided data. If you want predictions extrapolated to a hypothetical target population, try setting `estimate=\"population\".") # nolint
  }
  # we get this error for models with complex random effects structures in glmmTMB
  if (grepl("map factor length must equal", out$message, fixed = TRUE)) {
    msg <- c(
      msg,
      paste0(
        "\nYou may try using the `emmeans` backend, e.g. `estimate_means(model, by = c(",
        toString(paste0("\"", fun_args$by, "\"")),
        "), backend = \"emmeans\")`, or use `estimate_relation(model, by = c(",
        toString(paste0("\"", fun_args$by, "\"")),
        "))` instead. For contrasts or pairwise comparisons, save the output of `estimate_relation()` and pass it to `estimate_contrasts()`, e.g.\n" # nolint
      ),
      paste0("out <- estimate_relation(model, by = c(", toString(paste0("\"", fun_args$by, "\"")), "))"),
      paste0("estimate_contrasts(out, contrast = c(", toString(paste0("\"", fun_args$by, "\"")), "))")
    )
  }
  msg
}


# filter datagrid foe `estimate = "average"`---------------------------------

.filter_datagrid_average <- function(means, estimate, datagrid, datagrid_info) {
  # filter "by" rows when we have "average" marginalization, because we don't
  # pass data grid in such situations - but we still created the data grid based
  # on the `by` variables, for internal use, for example filtering at this point
  if (identical(estimate, "average") && all(datagrid_info$at_specs$varname %in% colnames(means))) {
    # sanity check - are all filter values from the data grid in the marginaleffects
    # object? For `estimate_average()`, predictions are based on the data, not
    # the theoretical data grid. When users request filtering by numeric predictors,
    # we need to make sure all filter-values (from which the dummy-data grid is built)
    # are available for filter. E.g., `by = "Petal.Width=c(3,5)"` won't work for
    # estimate = "average", because 3 and 5 don't appear in the iris data.
    filter_ok <- vapply(
      datagrid_info$at_specs$varname,
      function(j) any(datagrid[[j]] %in% means[[j]]),
      logical(1)
    )
    # stop if not...
    if (!all(filter_ok)) {
      # set up for informative error message
      invalid_filters <- names(filter_ok)[!filter_ok]
      first_invalid <- invalid_filters[1]
      example_values <- sample(
        unique(means[[first_invalid]]),
        pmin(3, insight::n_unique(means[[first_invalid]]))
      )
      # tell user...
      insight::format_error(paste0(
        "None of the values specified for the predictors ",
        datawizard::text_concatenate(invalid_filters, enclose = "`"),
        " are available in the data. This is required for `estimate=\"average\"`.",
        " Either use a different option for the `estimate` argument, or use values that",
        " are present in the data, such as ",
        datawizard::text_concatenate(example_values, last = " or ", enclose = "`"),
        "."
      ))
    }
    # else, filter values
    means <- datawizard::data_match(means, datagrid[datagrid_info$at_specs$varname])
  }
  means
}


# handle attributes -----------------------------------------------------------

# we have following attributes for modelbased-objects:
# - at, by, trend, contrasts, comparison, estimate, predict, p_adjust, transform,
#   ci: the values from the corresponding arguments from their related function
# - focal_terms: all variables from arguments `by`, `trend` and `contrasts`
# - adjusted_for: non-focal terms, all variables in the model that are not
#   in focal_terms
# - datagrid: the internal data grid that was used for the "newdata" argument
# - coef_name: name of the column with the predictions/contrasts
# - slope: the type of slope, e.g. "dx/dy". equals the "slope" argument when
#   calling avg_slopes()
# - model_info: object from insight::model_info()
#' @keywords internal
.add_attributes <- function(x, by = NULL, model_info = NULL, info = NULL) {
  attr(x, "at") <- by
  attr(x, "by") <- by
  attr(x, "model_info") <- model_info

  # compact list
  info <- insight::compact_list(info)

  if (!is.null(info) && length(info)) {
    if (!is.null(info$at_specs$varname)) {
      attr(x, "focal_terms") <- info$at_specs$varname
    }
    for (i in .info_elements()) {
      if (!is.null(info[[i]])) {
        attr(x, i) <- info[[i]]
      }
    }
  }
  x
}

# these are the names of attributes that can be flexibly added via
# `info` argument in `.add_attributes()`
.info_elements <- function() {
  c(
    "at", "by", "focal_terms", "adjusted_for", "predict", "trend", "comparison",
    "contrast", "estimate", "p_adjust", "transform", "datagrid", "preserve_range",
    "coef_name", "slope", "ci", "model_info", "contrast_filter", "keep_iterations"
  )
}


# Guess -------------------------------------------------------------------

#' @keywords internal
.guess_marginaleffects_arguments <- function(model, by = NULL, contrast = NULL, verbose = TRUE, ...) {
  # Gather info and data from model
  model_data <- insight::get_data(model, verbose = FALSE)
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


.check_offset <- function(model, estimate, offset = NULL, verbose = TRUE) {
  # check if model has an offset at all
  if (!is.null(insight::find_offset(model)) && verbose) {
    msg <- NULL
    if (is.null(offset)) {
      # if no offset argument was specified, tell user what this means
      msg <- switch(estimate,
        specific = ,
        typical = "Model contains an offset-term, which is set to its mean value. If you want to average predictions over the distribution of the offset (if appropriate), use `estimate = \"average\"`. If you want to fix the offset to a specific value, for instance `1`, use `offset = 1`.",
        "Model contains an offset-term and you average predictions over the distribution of that offset. If you want to fix the offset to a specific value, for instance `1`, use `offset = 1` and set `estimate = \"typical\"`."
      )
      # if offset term is log-transformed, tell user. offset should be fixed then
      log_offset <- insight::find_transformation(insight::find_offset(model, as_term = TRUE))
      if (!is.null(log_offset) && startsWith(log_offset, "log")) {
        msg <- c(
          msg,
          "We also found that the model has a log-transformed offset term. If you use the `offset` argument, the log-transformation will automatically be applied to the provided offset-value. I.e., consider using, for instance, `offset = 10` and not `offset = log(10)`."
        )
      }
    } else {
      # if offset was specified, and estimate averages over predictions, tell this
      msg <- switch(estimate,
        average = ,
        population = paste0("For `estimate = \"", estimate, "\"`, predictions are averaged over the distribution of the offset and the `offset` argument is ignored. If you want to fix the offset to a specific value, for instance `1`, use `offset = 1` and set `estimate = \"typical\"`.")
      )
    }
    if (!is.null(msg)) {
      insight::format_alert(msg)
    }
  }
}
