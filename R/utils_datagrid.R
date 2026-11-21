# create data grid for marginal means ----------------------------------------
#
# This helper function prepares and creates the data grid (or "reference grid")
# that is used to calculate marginal means. It sets up arguments for and calls
# `insight::get_datagrid()`. It also handles some special cases, like removing
# weights for certain marginalization types, handling offsets, and ensuring
# data types in the grid are consistent with the original model data.
#
# returns: A list containing:
#   - `datagrid`: The created data grid.
#   - `datagrid_info`: The attributes of the data grid.
#   - `dots`: The `...` arguments, with arguments consumed by this function removed.
.get_datagrid_means <- function(model, by, estimate, dots) {
  dg_factors <- switch(estimate, specific = "reference", "all")
  dg_args <- list(
    model,
    by = by,
    factors = dg_factors,
    include_random = TRUE,
    verbose = FALSE
  )
  # did user request weights? These are not supported for data-grid
  # marginalization types
  if (
    estimate %in%
      c("specific", "typical") &&
      (!is.null(dots$weights) || !is.null(dots$wts))
  ) {
    insight::format_warning(
      "Using weights is not possible when `estimate` is set to \"typical\" or \"specific\". Use `estimate = \"average\"` to include weights for marginal means or contrasts."
    )
    dots[c("weights", "wts")] <- NULL
  }

  # always show all theoretical values by default
  if (is.null(dots$preserve_range)) {
    dg_args$preserve_range <- FALSE
  }
  # add user-arguments from "...", but remove those arguments that are already set
  dots[c("by", "factors", "include_random", "verbose")] <- NULL
  dg_args <- insight::compact_list(c(dg_args, dots))

  # we first need to determine whether we have an offset term at all
  # ==========================================================================

  # for estimate = "population", we need the offset in `by`, thus, we have to
  # add it before we call data grid
  model_offset <- insight::find_offset(model)

  # offset_arg is a flag for the "offset" argument, which is used by users
  # to define a specific offset-value
  offset_arg <- (!is.null(dots$offset) && !is.null(model_offset))

  # check if offset was specified via `offset` argument in the model call, and
  # not inside formula. We need to handle this differently.
  call_offset <- !is.null(insight::get_call(model)$offset)

  # by_miss_offset is a flag that checks whether the offset-term was
  # specified in the "by" argument or not
  by_miss_offset <- (!is.null(dg_args$by) &&
    !is.null(model_offset) &&
    !any(startsWith(dg_args$by, model_offset)))

  if (offset_arg && estimate == "population" && by_miss_offset) {
    dg_args$by <- c(dg_args$by, paste(model_offset, "=", dots$offset))
  }

  # Get corresponding datagrid (and deal with particular ats)
  datagrid <- do.call(insight::get_datagrid, dg_args)
  datagrid_info <- attributes(datagrid)

  # handle offsets for other estimate-options
  if (offset_arg && estimate != "population") {
    # if user provided an offset-argument, we use this value in the datagrid
    datagrid[[model_offset]] <- dots$offset
  } else if (call_offset && estimate != "population") {
    # else, we take the mean-value of the offset from the data
    model_data <- insight::get_data(model)
    datagrid[[model_offset]] <- mean(model_data[[model_offset]], na.rmn = TRUE)
  }

  # check if we have a "newdata" argument, where offset is already specified
  # if so, we use that data and don't need to do anything else. However, if
  # a) model has an offset, b) newdata is provided, and c) model-offset is
  # not yet included in the newdata, or explicitly specied in the offset-argument,
  # we add/overwrite that value. "datagrid" is already handled above
  if (
    !is.null(model_offset) &&
      !is.null(dots$newdata) &&
      (!model_offset %in% colnames(dots$newdata) || offset_arg)
  ) {
    if (offset_arg) {
      # use user-specified offset-value
      offset_val <- dots$offset
    } else {
      # take mean from model data
      model_data <- insight::get_data(model)
      offset_val <- mean(model_data[[model_offset]], na.rmn = TRUE)
    }
    # add offset-term to newdata
    dots$newdata[[model_offset]] <- offset_val
  }

  # restore data types -  if we have defined numbers in `by`, like
  # `by = "predictor = 5"`, and `predictor` was a factor, it is returned as
  # numeric in the data grid. Fix this here, else marginal effects will fail
  datagrid <- datawizard::data_restoretype(
    datagrid,
    insight::get_data(model, verbose = FALSE)
  )

  list(datagrid = datagrid, datagrid_info = datagrid_info, dots = dots)
}


# filter datagrid for `estimate = "average"`---------------------------------
#
# This helper function filters the results of marginal means/predictions when
# `estimate = "average"`. For this estimation type, predictions are based on
# the original data, not a theoretical data grid. However, a data grid is
# still created internally to capture user-specified values in the `by`
# argument (e.g., `by = "x = c(1, 2)"`).
#
# This function applies that filtering post-computation. It also performs a
# sanity check to ensure that the values specified for filtering actually
# exist in the data, throwing an informative error if they don't.
#
# Arguments:
# - means: The data frame of marginal means/predictions from {marginaleffects}.
# - estimate: The estimation method string (e.g., "average").
# - datagrid: The data grid created based on the `by` argument.
# - datagrid_info: Attributes of the data grid, containing focal term info.
#
# returns: The filtered `means` data frame.
.filter_datagrid_average <- function(means, estimate, datagrid, datagrid_info) {
  # filter "by" rows when we have "average" marginalization, because we don't
  # pass data grid in such situations - but we still created the data grid based
  # on the `by` variables, for internal use, for example filtering at this point
  if (
    identical(estimate, "average") &&
      !is.null(datagrid) &&
      all(datagrid_info$at_specs$varname %in% colnames(means))
  ) {
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
        " To resolve this, either select a different option for the `estimate` argument, supply a defined data grid via the `data` argument, or use values that",
        " are present in the data, such as ",
        datawizard::text_concatenate(example_values, last = " or ", enclose = "`"),
        "."
      ))
    }
    # else, filter values
    means <- datawizard::data_match(means, datagrid[datagrid_info$at_specs$varname])
    # sanity check - do we have any rows left?
    .check_filter_args(means, "No rows left after filtering.")
  }
  means
}


# create weighted data grid, for faster computation of predictions. Works best
# if reduced data grid is considerably smaller than the original model data,
# and is more efficient if model contains fewer categorical predictors.
.create_weighted_datagrid <- function(model, estimate, fun_args, dots) {
  if (
    !is.null(dots$fast) &&
      (isTRUE(dots$fast) || is.numeric(dots$fast) || is.na(dots$fast))
  ) {
    # this overrides the existing data grid, if any. this means, "fast = TRUE"
    # only works with marginal predictions (when `estimate` is "average" or
    # "population"), not conditional, datagrid-based predictions (when
    # `estimate` is "specific" or "typical")
    if (estimate %in% c("specific", "typical")) {
      insight::format_error(
        "`fast` only works for marginal predictions, i.e. when `estimate` is set to \"average\" or \"population\"."
      )
    }
    # do we have bins?
    n_bins <- dots$n_bins
    # if not specified, we default to 5 bins, or to the value provided in "fast"
    if (is.null(n_bins)) {
      if (is.numeric(dots$fast)) {
        # use given number for bins
        n_bins <- dots$fast
      } else if (is.na(dots$fast)) {
        # if "NA", set n_bins to NULL, meaning we just take the mean for
        # numerics and only bin categorical predictors
        n_bins <- NULL
      } else {
        # if fast = TRUE, use the default (5 bins for numerics)
        n_bins <- 5
      }
    }
    # does model have weights?
    model_weights <- insight::find_weights(model)
    # if not, we default to TRUE to trigger weighted data grid. Else, we
    # pass the name of the weights variable
    if (is.null(model_weights)) {
      model_weights <- TRUE
    }
    # create weighted (reduced) data grid
    fast_grid <- insight::get_datagrid(model, n_bins = n_bins, weighted = model_weights)
    # update newdata with reduced data grid for faster computation
    fun_args$newdata <- fast_grid
    dots$weights <- fast_grid$Weight
    # clean-up dots
    dots$fast <- dots$n_bins <- NULL
  }
  list(fun_args = fun_args, dots = dots)
}
