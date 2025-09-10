#' @rdname get_emmeans
#' @export
get_marginalcontrasts <- function(
  model,
  contrast = NULL,
  by = NULL,
  predict = NULL,
  ci = 0.95,
  comparison = "pairwise",
  estimate = NULL,
  transform = NULL,
  p_adjust = "none",
  keep_iterations = FALSE,
  verbose = TRUE,
  ...
) {
  # check if available
  insight::check_if_installed("marginaleffects", minimum_version = "0.29.0")

  # temporarily overwrite settings that error on "too many" rows
  me_option <- getOption("marginaleffects_safe")
  options(marginaleffects_safe = FALSE)
  on.exit(options(marginaleffects_safe = me_option))

  # First step: prepare arguments ---------------------------------------------
  # ---------------------------------------------------------------------------

  # set default, if NULL
  if (is.null(contrast)) {
    contrast <- "auto"
  }

  # validate arguments
  estimate <- .validate_estimate_arg(estimate)
  comparison <- .check_for_inequality_comparison(comparison)

  # check whether contrasts should be made for numerics or categorical
  model_data <- insight::get_data(model, source = "mf", verbose = FALSE)
  on_the_fly_factors <- attributes(model_data)$factors

  # model details
  model_info <- insight::model_info(model, response = 1, verbose = FALSE)

  # Guess arguments
  my_args <- .guess_marginaleffects_arguments(model, by, contrast, verbose = verbose, ...)

  # sanitize comparison argument, to ensure compatibility between different
  # marginaleffects versions - newer versions don't accept a string argument,
  # only formulas (older versions don't accept formulas)
  my_args <- .get_marginaleffects_hypothesis_argument(
    comparison,
    my_args,
    model_data,
    estimate,
    ...
  )

  # extract first focal term
  first_focal <- my_args$contrast[1]

  # sanity check - is it a list? if so, use name
  if (is.list(first_focal)) {
    first_focal <- names(first_focal)
  }

  # sanity check: `contrast` and `by` cannot be the same
  cleaned_by <- gsub("=.*", "\\1", my_args$by)
  cleaned_contrast <- gsub("=.*", "\\1", my_args$contrast)
  if (
    length(cleaned_by) &&
      length(cleaned_contrast) &&
      (all(cleaned_by %in% cleaned_contrast) || all(cleaned_contrast %in% cleaned_by))
  ) {
    insight::format_error(
      "You cannot specifiy the same variables in `contrast` and `by`. Either omit `by`, or choose a different variable for `contrast` or `by`." # nolint
    )
  }

  # check whether we contrast slopes or categorical predictors:
  # if first focal term is numeric, we contrast slopes, but slopes only for
  # numerics with many values, not for binary or likert-alike
  compute_slopes <- is.numeric(model_data[[first_focal]]) &&
    !.is_likert(model_data[[first_focal]], verbose = verbose, ...) &&
    !first_focal %in% on_the_fly_factors

  # Second step: compute contrasts, for slopes or categorical -----------------
  # ---------------------------------------------------------------------------

  if (.is_inequality_comparison(comparison)) {
    # inequality effect summary, see Trenton D. Mize, Bing Han 2025
    # Inequality and Total Effect Summary Measures for Nominal and Ordinal Variables
    # Sociological Science February 5, 10.15195/v12.a7
    # this requires a special handling, because we can only use it with avg_comparisons
    out <- get_inequalitycontrasts(
      model,
      model_data,
      my_args,
      comparison,
      ci,
      compute_slopes,
      estimate,
      ...
    )
    predict <- "response"
  } else if (compute_slopes) {
    # sanity check - contrast for slopes only makes sense when we have a "by" argument
    if (is.null(my_args$by)) {
      insight::format_error("Please specify the `by` argument to calculate contrasts of slopes.") # nolint
    }
    # call slopes with hypothesis argument
    out <- estimate_slopes(
      model = model,
      trend = my_args$contrast,
      by = my_args$by,
      ci = ci,
      estimate = estimate,
      hypothesis = my_args$comparison_slopes,
      backend = "marginaleffects",
      transform = transform,
      keep_iterations = keep_iterations,
      verbose = verbose,
      ...
    )
  # } else if (identical(estimate, "population")) {
  #   # we "overwrite" contrast with the original argument, because above in
  #   # .get_marginaleffects_hypothesis_argument(), we removed `by` from `contrast`
  #   # however, sometimes we want counterfactual contrasts of a predictor *and*
  #   # stratify by the same predictor - removing `by` from `contrast` would mean
  #   # that "contrast" is empty, which is not what we want here
  #   my_args$contrast <- contrast
  #   # counterfactual needs comparisons of differences, thus we call avg_comparisons
  #   out <- get_counterfactualcontrasts(
  #     model = model,
  #     model_info = model_info,
  #     my_args = my_args,
  #     predict = predict,
  #     comparison = comparison,
  #     ci = ci,
  #     p_adjust = p_adjust,
  #     transform = transform,
  #     verbose = verbose,
  #     ...
  #   )
  } else {
    # for contrasts of categorical predictors, we call avg_predictions
    out <- estimate_means(
      model = model,
      by = unique(c(my_args$contrast, my_args$by)),
      ci = ci,
      hypothesis = my_args$comparison,
      predict = predict,
      backend = "marginaleffects",
      estimate = estimate,
      transform = transform,
      keep_iterations = keep_iterations,
      verbose = verbose,
      .joint_test = my_args$joint_test,
      ...
    )
  }

  # filter results - for `estimate_contrasts()`, and when `estimate = "average"`,
  # we don't filter using the data grid; due to the flexible way of defining
  # comparisons, we need the full data grid and filter here (e.g., when we have
  # `by="Petal.Width=c(1, 2)"`)
  out <- .filter_contrasts_average(out, my_args)

  # adjust p-values
  if (!model_info$is_bayesian) {
    out <- .p_adjust(model, out, p_adjust, verbose, ...)
  }

  # Last step: Save information in attributes  --------------------------------
  # ---------------------------------------------------------------------------

  out <- .add_attributes(
    out,
    by = my_args$by,
    model_info = model_info,
    info = list(
      contrast = my_args$contrast,
      predict = predict,
      comparison = my_args$comparison,
      estimate = estimate,
      p_adjust = p_adjust,
      contrast_filter = my_args$contrast_filter,
      keep_iterations = keep_iterations
    )
  )

  # remove "estimate_means" class attribute
  class(out) <- setdiff(unique(c("marginaleffects_contrasts", class(out))), "estimate_means")
  out
}


# filter "contrasts" for `estimate = "average"` -------------------------------

.filter_contrasts_average <- function(out, my_args) {
  # filter results - for `estimate_contrasts()`, and when `estimate = "average"`,
  # we don't filter using the data grid; due to the flexible way of defining
  # comparisons, we need the full data grid and filter here (e.g., when we have
  # `by="Petal.Width=c(1, 2)"`)
  if (!is.null(my_args$by_filter) && all(names(my_args$by_filter) %in% colnames(out))) {
    for (i in names(my_args$by_filter)) {
      filter_ok <- any(my_args$by_filter[[i]] %in% out[[i]])
      # stop if not...
      if (!filter_ok) {
        # set up informative message
        example_values <- sample(unique(out[[i]]), pmin(3, insight::n_unique(out[[i]])))
        # tell user...
        insight::format_error(paste0(
          "None of the values specified for the predictor `", i,
          "` are available in the data. This is required for `estimate=\"average\"`.",
          " Either use a different option for the `estimate` argument, or use values that",
          " are present in the data, such as ",
          datawizard::text_concatenate(example_values, last = " or ", enclose = "`"),
          "."
        ))
      }
      out <- out[out[[i]] %in% my_args$by_filter[[i]], ]
    }
    # sanity check - do we have any rows left?
    if (nrow(out) == 0) {
      .filter_error("No rows left after filtering.")
    }
  }
  out
}


# make "comparison" argument compatible -----------------------------------
# -------------------------------------------------------------------------

# this function has two major tasks: format the "comparison" argument for use
# in the marginaleffects package, and extract the potential filter values used
# in `by` and `contrast` (if any), to "clean" these arguments and save the levels
# or values at which rows should be filtered later...
.get_marginaleffects_hypothesis_argument <- function(comparison,
                                                     my_args,
                                                     model_data = NULL,
                                                     estimate = NULL,
                                                     ...) {
  # init
  comparison_slopes <- by_filter <- contrast_filter <- by_token <- NULL
  joint_test <- FALSE
  # save original `by`
  original_by <- my_args$by
  original_comparison <- comparison

  # make sure "by" is a valid column name, and no filter-directive, like
  # "Species='setosa'". If `by` is also used for filtering, split and extract
  # filter value for later - we have to filter rows manually after calculating
  # contrasts (but only for `estimate = "average"`!). Furthermore, "clean" `by`
  # argument (remove filter), because we need the pure variable name for setting
  # up the hypothesis argument, where variables in `by` are used in the formula
  if (!is.null(my_args$by) && any(grepl("=", my_args$by, fixed = TRUE))) { # "[^0-9A-Za-z\\._]"
    # find which element in `by` has a filter
    filter_index <- grep("=", my_args$by, fixed = TRUE)
    for (f in filter_index) {
      # look for filter values
      filter_value <- insight::trim_ws(unlist(
        strsplit(my_args$by[f], "=", fixed = TRUE),
        use.names = FALSE
      ))
      if (length(filter_value) > 1) {
        # parse filter value and save for later use - we create a named list,
        # because we need to know *which* variables in `by` used a filter. we
        # could have `by = c("x", "y=c(1,2)")`, but also `by = c("x=c('a','b')", "y")`.
        # the list has the variable name as name, and the filter values as element
        by_value <- stats::setNames(
          list(.safe(eval(str2lang(filter_value[2])))),
          filter_value[1]
        )
        by_filter <- c(by_filter, by_value)
        # check if evaluation was possible, or if we had a "token", like
        # "[sd]" or "[fivenum]". If not, update `by`, else preserve
        if (is.null(by_value[[1]]) && !grepl("[\\[\\]]", filter_value[2])) {
          by_token <- c(by_token, stats::setNames(list(filter_value[2]), filter_value[1]))
        }
        # copy "cleaned" variable
        my_args$by[f] <- filter_value[1]
      }
    }
  }

  # if filtering is requested for contrasts, we also want to extract the filter
  # values for later use. we only need this for `estimate = "average"`, because
  # that is the only situation where we do *not* use a data grid, which we else
  # could use for filtering, by dropping not-wanted rows from the grid.
  if (identical(estimate, "average") && !is.null(my_args$contrast) && any(grepl("=", my_args$contrast, fixed = TRUE))) { # nolint
    # find which element in `by` has a filter
    filter_index <- grep("=", my_args$contrast, fixed = TRUE)
    for (f in filter_index) {
      # look for filter values
      filter_value <- insight::trim_ws(unlist(
        strsplit(my_args$contrast[f], "=", fixed = TRUE),
        use.names = FALSE
      ))
      if (length(filter_value) > 1) {
        # parse filter value and save for later user - we need as named list
        # for the same reason as mentioned above...
        contrast_filter <- c(
          contrast_filter,
          stats::setNames(list(.safe(eval(str2lang(filter_value[2])))), filter_value[1])
        )
      }
    }
  }

  # convert comparison and by into a formula
  if (is.null(comparison)) {
    # default to pairwise, if comparison = NULL
    comparison <- comparison_slopes <- ~pairwise
  } else if (.is_custom_comparison(comparison)) {
    # we have not set "comparison_slopes" yet - we also set it to custom hypothesis
    comparison_slopes <- comparison
    # if "comparison" is a function, or a matrix (possibly with contrasts),
    # we don't modify or check it, but just pass it to the "hypothesis" argument
    # in marginaleffects
  } else if (!is.function(comparison) && !is.matrix(comparison)) {
    # only proceed if we don't have custom comparisons
    # if we have a formula as comparison, we convert it into strings in order
    # to extract the information for "comparison" and "by", because we
    # recombine the formula later - we always use variables in `by` as group
    # in the formula-definition for marginaleffects
    if (inherits(comparison, "formula")) {
      # check if we have grouping in the formula, indicated via "|". we split
      # the formula into the three single components: lhs ~ rhs | group
      f <- insight::trim_ws(unlist(strsplit(insight::safe_deparse(comparison), "[~|]")))
      # extract formula parts
      formula_lhs <- f[1]
      formula_rhs <- f[2]
      formula_group <- f[3]
      # can be NA when no group
      if (is.na(formula_group) || !nzchar(formula_group)) {
        # no grouping via formula
        formula_group <- NULL
      } else {
        # else, if we have groups, update by-argument
        my_args$by <- formula_group
      }
    } else {
      # if comparison is a string, do sanity check for "comparison" argument
      insight::validate_argument(comparison, .valid_hypothesis_strings())
      # exception: we have an option "joint" to avoid more arguments. we
      # must recode this into proper syntax
      if (comparison == "joint") {
        comparison <- "reference"
        joint_test <- TRUE
      }
      # for some comparisons, we need an empty left-hand side. else, we default
      # to "difference".
      formula_lhs <- switch(
        comparison,
        poly = ,
        helmert = "",
        "difference"
      )
      formula_rhs <- comparison
    }
    # we put "by" into the formula. user either provided "by", or we put the
    # group variable from the formula into "by" (see code above), hence,
    # "my_args$by" definitely contains the requested groups
    formula_group <- my_args$by
    # compose formula
    f <- paste(formula_lhs, "~", paste(formula_rhs, collapse = "+"))
    # for contrasts of slopes, we don *not* want the group-variable in the formula
    comparison_slopes <- stats::as.formula(f)
    # for contrasts of categorical, we add the group variable and update `by`
    if (!is.null(formula_group)) {
      f <- paste(f, "|", paste(formula_group, collapse = "+"))
      my_args$by <- formula_group
    }
    comparison <- stats::as.formula(f)
    # if user specified group in "by" *and* in formula, we keep the group
    # for contrasts of slopes - thus,we need to update comparison_slopes
    by_formula <- trimws(unlist(
      strsplit(deparse(original_comparison), "|", fixed = TRUE),
      use.names = FALSE
    ))[2]
    if (!is.na(by_formula) && identical(by_formula, formula_group)) {
      # we have a group variable in the formula, which is the same as in `by`
      # so we keep it for the slopes comparison - this is required to add
      # grouping in (pairwise) slopes
      comparison_slopes <- comparison
    }
  }
  # remove "by" from "contrast"
  if (estimate != "population") {
    my_args$contrast <- setdiff(my_args$contrast, my_args$by)
  }

  # for `estimate = "average"`, we cannot create a data grid, thus we need to
  # filter manually. However, for all other `estimate` options, we can simply
  # use the data grid for filtering
  if (!identical(estimate, "average") && !is.null(original_by)) {
    my_args$by <- original_by
    by_filter <- NULL
  } else if (!is.null(by_token)) {
    # add back token to `by`
    for (i in names(by_token)) {
      my_args$by[my_args$by == i] <- paste(i, by_token[[i]], sep = "=")
    }
  }

  c(
    # the "my_args" argument, containing "by" and "contrast"
    my_args,
    list(
      # the modifed comparison, as formula, which also includes "by" as group
      comparison = comparison,
      # the modifed comparison, as formula, excluding "by" as group
      comparison_slopes = comparison_slopes,
      # the filter-value, in case `by` or contrast indicated any filtering
      by_filter = insight::compact_list(by_filter),
      contrast_filter = insight::compact_list(contrast_filter),
      # in case we have a joint/omnibus test
      joint_test = joint_test,
      # cleaned `by` and `contrast`, without filtering information
      cleaned_by = gsub("=.*", "\\1", my_args$by),
      cleaned_contrast = gsub("=.*", "\\1", my_args$contrast)
    )
  )
}


# supported comparison strings  --------------------------------------
# --------------------------------------------------------------------

.valid_hypothesis_strings <- function() {
  c(
    "pairwise", "reference", "sequential", "meandev", "meanotherdev",
    "revpairwise", "revreference", "revsequential", "poly", "helmert",
    "trt_vs_ctrl", "joint", "inequality", "inequality_pairwise",
    "inequality_ratio", "inequality_ratio_pairwise"
  )
}


# check for custom hypothesis  --------------------------------------
# -------------------------------------------------------------------

.is_custom_comparison <- function(comparison) {
  !is.null(comparison) &&
    length(comparison) == 1 &&
    is.character(comparison) &&
    grepl("=", comparison, fixed = TRUE) &&
    grepl("\\bb\\d+\\b", comparison)
}


.extract_custom_comparison <- function(comparison) {
  # find all "b" strings
  matches <- gregexpr("\\bb\\d+\\b", comparison)[[1]]
  match_lengths <- attr(matches, "match.length")

  # extract all "b" strings, so we have a vector of all "b" used in the comparison
  unlist(lapply(seq_along(matches), function(i) {
    substr(comparison, matches[i], matches[i] + match_lengths[i] - 1)
  }), use.names = FALSE)
}


.reorder_custom_hypothesis <- function(comparison, datagrid, focal) {
  # create a data frame with the same sorting as the data grid, but only
  # for the focal terms terms
  datagrid <- data.frame(expand.grid(lapply(datagrid[focal], unique)))
  # this is the row-order we use in modelbased
  datagrid$.rowid <- 1:nrow(datagrid)
  # this is the row-order in marginaleffects
  datagrid <- datawizard::data_arrange(
    datagrid,
    colnames(datagrid)[1:(length(datagrid) - 1)]
  )
  # we need to extract all b's and the former parameter numbers
  b <- .extract_custom_comparison(comparison)
  old_b_numbers <- as.numeric(gsub("b", "", b, fixed = TRUE))
  # these are the new numbers of the b-values
  new_b_numbers <- match(old_b_numbers, datagrid$.rowid)
  new_b <- paste0("b", new_b_numbers)
  # we need to replace all occurences of "b" in comparison with "new_b".
  # however, to avoid overwriting already replaced values with "gsub()", we
  # first replace with a non-existing pattern "new_b_letters", which we will
  # replace with "new_b" in a second step
  new_b_letters <- paste0("b", letters[new_b_numbers])
  # first, numbers to letters
  for (i in seq_along(b)) {
    comparison <- gsub(b[i], new_b_letters[i], comparison, fixed = TRUE)
  }
  # next, letters to new numbers
  for (i in seq_along(b)) {
    comparison <- gsub(new_b_letters[i], new_b[i], comparison, fixed = TRUE)
  }
  comparison
}
