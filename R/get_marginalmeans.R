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
get_marginalmeans <- function(
  model,
  by = "auto",
  predict = NULL,
  ci = 0.95,
  estimate = NULL,
  transform = NULL,
  iterations = NULL,
  keep_iterations = FALSE,
  verbose = TRUE,
  ...
) {
  # check if available
  insight::check_if_installed("marginaleffects", minimum_version = "0.29.0")

  # First step: process arguments --------------------------------------------
  # --------------------------------------------------------------------------

  dots <- list(...)
  comparison <- dots$hypothesis
  post_process <- dots$post_process
  original_contrast <- dots$.original_contrast
  joint_test <- isTRUE(dots$.joint_test)
  omnibus_test <- isTRUE(dots$.omnibus_test)

  # validate input
  estimate <- .validate_estimate_arg(estimate)

  # model details
  model_info <- insight::model_info(model, response = 1, verbose = FALSE)

  # Guess arguments
  my_args <- .guess_marginaleffects_arguments(model, by, verbose = verbose, ...)

  # find default response-type, and get information about back transformation
  predict_args <- .get_marginaleffects_type_argument(
    model,
    predict,
    comparison,
    estimate,
    model_info,
    omnibus_test,
    verbose,
    ...
  )

  # was "data" argument used? if so, it replaces "newdata" in dots
  dots <- .check_dots_data(dots, verbose)

  # inform user about appropriate use of offset-terms
  model_offset <- dots$offset
  .check_offset(model, estimate, offset = model_offset, my_args, dots, verbose = verbose)

  # Second step: create a data grid -------------------------------------------
  # ---------------------------------------------------------------------------

  # exception: by = NULL computes overall mean
  if (is.null(by)) {
    datagrid <- datagrid_info <- NULL
  } else {
    # setup arguments to create the data grid
    out <- .get_datagrid_means(model, my_args$by, estimate, dots)
    # update objects
    datagrid <- out$datagrid
    datagrid_info <- out$datagrid_info
    dots <- out$dots
  }

  # stop for variable names that marginaleffects overwrites
  .check_marginaleffects_names(
    model,
    model_info,
    focal = datagrid_info$at_specs$varname,
    contrast = original_contrast
  )

  # Third step: prepare arguments for marginaleffects ------------------------
  # --------------------------------------------------------------------------

  # remove user-arguments from "..." that will be used when calling marginaleffects
  # fmt: skip
  dots[c(
    "by", "conf_level", "type", "digits", "bias_correction", "sigma",
    "offset", ".joint_test", ".omnibus_test", ".original_contrast", "post_process"
  )] <- NULL

  # model df - can be passed via `...`
  if (is.null(dots$df) && !model_info$is_bayesian) {
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
      insight::format_error(
        "Could not create data grid based on variables selected in `by`. Please check if all `by` variables are present in the data set."
      )
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
    if (!is.null(model_offset) && estimate == "average") {
      # handling offsets for estimate = "average" is different,
      # we need to add the specific offset-value to the "variables"
      # argument
      fun_args$variables <- as.list(stats::setNames(
        model_offset,
        insight::find_offset(model)
      ))
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

  ## TODO: document "fast" argument?

  # fast mode?
  # ---------------------------

  # we have several options for "fast"
  # fast = NULL:    nothing happens
  # fast = TRUE:    creates a weighted (reduced) data frame, binning numerics
  #                 into 5 groups
  # fast = NA:      creates a weighted (reduced) data frame, setting numerics
  #                 to their mean value
  # fast = <value>: creates a weighted (reduced) data frame, binning
  #                 numerics into <value> groups

  # create weighted (reduced) data grid
  tmp <- .create_weighted_datagrid(
    model = model,
    estimate = estimate,
    fun_args = fun_args,
    dots = dots
  )
  # if we made any changes, update arguments
  fun_args <- tmp$fun_args
  dots <- tmp$dots

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
    # first check if we have filtering in by-variables when estimate is set to
    # 'average' - this won't work, because the internal re-ordering relies on
    # predictions based on a "filtered" data grid, while `estimate = "average"`
    # does the filtering later in the code, *after* calling marginaleffects
    .check_custom_contrasts_and_filter(estimate, by, original_contrast, comparison)

    # reorder custom row-indices
    dots$hypothesis <- .reorder_custom_hypothesis(
      comparison,
      datagrid,
      focal = datagrid_info$at_specs$varname
    )
  }

  # cleanup
  fun_args <- insight::compact_list(c(fun_args, dots))

  # handle remaining arguments ---------------------------------------
  # ------------------------------------------------------------------

  ## TODO: need to check against different mixed models results from other packages
  # set to NULL
  if (!"re.form" %in% names(dots)) {
    fun_args$re.form <- NULL
  }

  # missing or NA for conf_level? If so, we want to suppress SE and CI
  if (is.null(fun_args$conf_level) || is.na(fun_args$conf_level)) {
    fun_args$conf_level <- NULL # for NA
    fun_args$vcov <- FALSE
  }

  # transform response? we keep the argument as passed by the user for the
  # table footer
  transform_arg <- transform
  if (isTRUE(transform)) {
    transform <- insight::get_transformation(model, verbose = FALSE)$inverse
  }
  if (!is.null(transform)) {
    fun_args$transform <- transform
  }

  # bayesian models: number of posterior draws to use, passed to `ndraws`
  if (!is.null(iterations)) {
    fun_args$ndraws <- iterations
  }

  # Fourth step: compute marginal means ---------------------------------------
  # ---------------------------------------------------------------------------

  # we can use this function for contrasts as well,
  # just need to add "hypothesis" argument
  means <- .call_marginaleffects(fun_args, post_process, verbose)
  vcov_means <- .safe(stats::vcov(means))

  # check for valid bounds, when model is glm and predict = "response"
  .check_ci_bounds(means, predict_args$predict, model_info, verbose)

  # intermediate step: joint tests --------------------------------------------
  # ---------------------------------------------------------------------------

  if (joint_test || omnibus_test) {
    means <- .get_jointtest(
      means,
      my_args,
      test = c(dots$joint_test, dots$test),
      null = dots$null,
      is_omnibus = omnibus_test
    )
    # update null
    dots$null <- attr(means, "null", exact = TRUE)
  }

  # Fifth step: post-processing marginal means----------------------------------
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
    predict_args$predict <- "invlink(link)"
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

  # handle brms-alias
  if (!is.null(dots$re_formula)) {
    dots$re.form <- dots$re_formula
  }

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
        transform_arg = transform_arg,
        iterations = iterations,
        keep_iterations = keep_iterations,
        joint_test = joint_test,
        omnibus_test = omnibus_test,
        null = dots$null,
        vcov = vcov_means,
        equivalence = dots$equivalence
      )
    ),
    # needed for some additional information for mixed models
    model = model,
    re_form_na = !is.null(dots$re.form) && is.na(dots$re.form)
  )
  class(means) <- unique(c("marginaleffects_means", class(means)))

  means
}


# call marginaleffects and process potential errors ---------------------------
#
# This wrapper function executes `marginaleffects::avg_predictions()` with the
# prepared argument list (`fun_args`) and handles post-processing, error
# translation, and sanity checks.
#
# Specifically, it:
# 1. Calls `marginaleffects::avg_predictions()` inside a `tryCatch()` block,
#    suppressing package-level warnings that are handled separately by
#    `modelbased`.
# 2. Applies sequential contrast or hypothesis tests via
#    `.post_process_comparisons()` if `post_process` is specified (for example,
#    when computing contrasts of contrasts / interaction contrasts).
# 3. Intercepts raw errors from `{marginaleffects}` and translates them into
#    more user-friendly, context-aware error messages via
#    `.check_marginaleffects_errors()`.
# 4. Verifies that the returned data frame is non-empty (e.g., when filtering
#    under `estimate = "average"` yields zero matching rows).
#
# Arguments:
# - fun_args: A named list of arguments passed to `marginaleffects::avg_predictions()`.
# - post_process: Optional formula, character string, or list of formulas/strings
#   passed to `marginaleffects::hypotheses()` for sequential post-processing.
# - verbose: Logical, whether to display progress alerts during post-processing.
#
# Returns: A `marginaleffects` predictions/comparisons object.
.call_marginaleffects <- function(fun_args, post_process = NULL, verbose = TRUE) {
  out <- tryCatch(
    {
      # call marginaleffects
      result <- suppressWarnings(do.call(marginaleffects::avg_predictions, fun_args))
      # process subsequential comparisons, if any
      .post_process_comparisons(result, post_process = post_process, verbose = verbose)
    },
    error = function(e) e
  )

  # display informative error
  if (inherits(out, "simpleError")) {
    insight::format_error(.check_marginaleffects_errors(out, fun_args))
  }

  # check number of rows - for estimate = "average", no rows might be returned
  .check_filter_args(out, "No rows returned from marginal means.")

  out
}


# process sequential comparisons ----------------------------------------------
#
# This helper applies one or more sequential hypothesis tests (contrasts of
# contrasts, such as difference-in-differences) on top of an existing
# `marginaleffects` output object using `marginaleffects::hypotheses()`.
#
# When `marginaleffects::hypotheses()` is called repeatedly on a previous
# hypothesis object, it replaces readable level names in the `hypothesis`
# column with generic positional indices like `(b1) - (b2)`. This function
# preserves the original parameter and group labels across each step and
# delegates label reconstruction to `.update_post_process_params()`.
#
# Arguments:
# - out: Output object returned by `marginaleffects::avg_predictions()` (or a
#   previous `marginaleffects::hypotheses()` call).
# - post_process: A formula, character string, or list thereof specifying the
#   sequential hypothesis/contrast tests to apply.
# - verbose: Logical, whether to print an alert for each post-processing step.
#
# Returns: The updated `marginaleffects` object with human-readable `hypothesis`
#   labels restored.
.post_process_comparisons <- function(out, post_process = NULL, verbose = TRUE) {
  if (!is.null(post_process)) {
    # coerce to list
    if (!is.list(post_process)) {
      post_process <- list(post_process)
    }
    # save temporary parameter names
    temp_params <- .safe(out$hypothesis)
    # parameter names from groups
    group_params <- .safe(out[seq_len(which(colnames(out) == "estimate") - 1)[-1]])
    # check
    for (i in seq_along(post_process)) {
      if (verbose) {
        insight::format_alert(paste0(
          "Post-processing ",
          deparse(post_process[[i]]),
          "..."
        ))
      }
      out <- suppressWarnings(marginaleffects::hypotheses(
        out,
        hypothesis = post_process[[i]]
      ))
      temp_params <- .update_post_process_params(
        out,
        temp_params,
        group_params,
        post_process[[i]]
      )
    }
    # check if extractinb parameters worked
    if (length(temp_params) == nrow(out)) {
      out$hypothesis <- temp_params
    }
  }
  out
}


# update parameter labels after sequential hypothesis tests -------------------
#
# After `marginaleffects::hypotheses()` computes contrasts of contrasts, its
# `hypothesis` column contains row-index expressions of the form `(b1) - (b2)`.
# This helper maps those row indices (`lhs` and `rhs`) back to the original
# human-readable contrast labels (`temp_params`) and any non-stratified grouping
# variables (`group_params`).
#
# If the post-processing formula includes conditioning/grouping variables
# (e.g., `difference ~ pairwise | group`), those variables are excluded from
# the combined string label because `{marginaleffects}` already retains them
# as dedicated columns in `out`.
#
# Arguments:
# - out: The updated `marginaleffects` object containing `(bX) - (bY)` strings
#   in `out$hypothesis`.
# - temp_params: Character vector of parameter/contrast names from the step
#   prior to the latest `hypotheses()` call.
# - group_params: Data frame of grouping columns located before the `estimate`
#   column in the prior output.
# - post_process: The formula or character string used in the current
#   post-processing step.
#
# Returns: A character vector of reconstructed contrast labels (or `NULL` if
#   reconstruction fails or `temp_params` is `NULL`).
.update_post_process_params <- function(
  out,
  temp_params,
  group_params,
  post_process = NULL
) {
  if (is.null(temp_params)) {
    return(NULL)
  }

  # we may have groups in post-processing contrasts, and these groups should not
  # be included in the levels of the hypothesis
  if (
    !is.null(post_process) &&
      (inherits(post_process, "formula") || is.character(post_process))
  ) {
    # extract all group variables from post-processing hypothesis, i.e.
    # extract "groups" from "difference ~ pairwise | groups"
    comparison_groups <- vapply(
      strsplit(
        paste(insight::safe_deparse(post_process), collapse = ""),
        "|",
        fixed = TRUE
      )[[1L]],
      insight::trim_ws,
      character(1)
    )
    # if we have any groups, check if these names also appear in the group-name
    # data frame, and if yes, remove them. these groups have their own column
    # in the output and don't need to appear twice (in the comparison labels)
    # again
    if (length(comparison_groups) > 1) {
      comparison_groups <- all.vars(stats::formula(paste("~", comparison_groups[2])))
      remove <- intersect(colnames(group_params), comparison_groups)
      if (length(remove)) {
        group_params[remove] <- NULL
      }
    }
  }

  .safe(
    {
      # clean current parameter names
      temp_params <- gsub("(", "", gsub(")", "", temp_params, fixed = TRUE), fixed = TRUE)
      # extract rownumbers (from b-cofficients) from current output
      lhs <- as.numeric(gsub("\\(b(\\d+)\\) - \\(b(\\d+)\\)", "\\1", out$hypothesis))
      rhs <- as.numeric(gsub("\\(b(\\d+)\\) - \\(b(\\d+)\\)", "\\2", out$hypothesis))
      # update current parameter names
      if (all(temp_params[lhs] == temp_params[rhs])) {
        temp_params <- temp_params[lhs]
      } else {
        temp_params <- paste(temp_params[lhs], "-", temp_params[rhs])
      }
      if (!is.null(group_params)) {
        for (i in colnames(group_params)) {
          temp_params <- paste0(
            temp_params,
            ", ",
            group_params[lhs, i],
            " - ",
            group_params[rhs, i]
          )
        }
      }
      temp_params
    },
    quietly = TRUE
  )
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
# - transform_arg: the `transform` argument as passed by the user (`transform`
#   is often only a logical), used for the scale line of the table footer
# - renamed_focal: focal terms with the name of an output column, named by
#   their old names, with the new names "<name>.1" as values
#' @keywords internal
.add_attributes <- function(
  x,
  by = NULL,
  model_info = NULL,
  info = NULL,
  model = NULL,
  re_form_na = NULL
) {
  attr(x, "at") <- by
  attr(x, "by") <- by
  attr(x, "model_info") <- model_info

  # extract information, required for mixed models handling
  if (!is.null(model)) {
    response <- insight::find_response(model, combine = FALSE)
    re_intercept <- insight::find_random(model, flatten = TRUE, split_nested = TRUE)
  } else {
    response <- re_form_na <- re_intercept <- NULL
  }

  # compact list
  info <- insight::compact_list(info)

  # some special handling for mixed models - we need this information for
  # printing an informative table footer.
  if (isTRUE(model_info$is_mixed)) {
    # if we have mixed models, we don't want the response in the adusted-for
    # information, so we check and remove it here, if necessary
    if (!is.null(info$adjusted_for) && !is.null(response)) {
      info$adjusted_for <- setdiff(info$adjusted_for, response)
    }
    # we definitely don't want the random-intercept in the "adjusted for" list
    info$adjusted_for <- setdiff(info$adjusted_for, re_intercept)
    # save information about random effects (cluster/grouping variable)
    if (!isTRUE(re_form_na)) {
      info$re_intercept <- re_intercept
    }
    # save info if re.form was NA
    info$re_form_na <- isTRUE(re_form_na)
  }

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
# fmt: skip
.info_elements <- function() {
  # fmt: skip
  c(
    "at", "by", "focal_terms", "adjusted_for", "predict", "trend", "comparison",
    "contrast", "estimate", "p_adjust", "transform", "transform_arg", "datagrid",
    "preserve_range", "coef_name", "slope", "ci", "model_info", "contrast_filter", "null",
    "iterations", "keep_iterations", "joint_test", "omnibus_test", "vcov",
    "equivalence", "context_effects", "renamed_focal", "re_intercept", "re_form_na"
  )
}


# Guess -------------------------------------------------------------------

#' @keywords internal
.guess_marginaleffects_arguments <- function(
  model,
  by = NULL,
  contrast = NULL,
  comparison = NULL,
  verbose = TRUE,
  ...
) {
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
        # in-formula transformations, like `as.factor(x)`, need special handling
        # because these predictors are no factors in the data. we get flags for
        # such transformations when we request data from the model frame
        model_frame <- insight::get_data(model, source = "mf", verbose = FALSE)
        factors <- attributes(model_frame)$factors
        # if still no factors found, throw error
        if (is.null(factors)) {
          # tell user to specify argument
          msg <- paste0(
            "Model contains no categorical predictor. Please specify `",
            spec_name,
            "`."
          )
          # for `by`, we also allow NULL - tell user
          if (identical(spec_name, "by")) {
            msg <- paste(msg, "Or use `by = NULL` to predict the grand mean.")
          }
          insight::format_error(msg)
        }
        spec_value <- factors
      }
      if (verbose) {
        insight::format_alert(paste0(
          "We selected `",
          spec_name,
          "=c(",
          toString(paste0('"', spec_value, '"')),
          ")`."
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
