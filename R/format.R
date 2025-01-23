# Format ------------------------------------------------------------------

#' @export
format.estimate_contrasts <- function(x, format = NULL, ...) {
  # don't print columns of adjusted_for variables
  adjusted_for <- attr(x, "adjusted_for", exact = TRUE)
  if (!is.null(adjusted_for) && all(adjusted_for %in% colnames(x))) {
    # remove non-focal terms from data frame
    x[adjusted_for] <- NULL
  }

  # arrange columns (not for contrast now)
  by <- rev(attr(x, "focal_terms", exact = TRUE))
  if (!is.null(by) && all(by %in% colnames(x))) {
    # arrange predictions
    x <- datawizard::data_arrange(x, select = by)
    # protect integers, only for focal terms
    for (i in by) {
      if (is.numeric(x[[i]])) {
        x[[i]] <- insight::format_value(x[[i]], protect_integers = TRUE, ...)
      }
    }
  }

  # remove all-NA columns
  x <- datawizard::remove_empty_columns(x)

  if (!is.null(format) && format %in% c("md", "markdown", "html")) {
    insight::format_table(x, ci_brackets = c("(", ")"), ...)
  } else {
    insight::format_table(x, ...)
  }
}

#' @export
format.estimate_means <- format.estimate_contrasts

#' @export
format.estimate_slopes <- format.estimate_contrasts

#' @export
format.estimate_predicted <- format.estimate_contrasts

#' @export
format.estimate_grouplevel <- format.estimate_contrasts


#' @export
format.estimate_smooth <- function(x, ...) {
  # Colnames
  if ("Size" %in% names(x)) x$Size <- ifelse(x$Size < 1, paste0(insight::format_value(x$Size * 100), "%"), "100%")
  if ("Part" %in% names(x)) x$Part <- insight::format_value(x$Part, protect_integers = TRUE)

  insight::format_table(x, ...)
}


#' @export
format.visualisation_matrix <- function(x, ...) {
  x
}


#' @export
format.marginaleffects_means <- function(x, model, ci = 0.95, ...) {
  # model information
  model_data <- insight::get_data(model, verbose = FALSE)
  info <- insight::model_info(model, verbose = FALSE)
  non_focal <- setdiff(colnames(model_data), attr(x, "focal_terms"))
  is_contrast_analysis <- !is.null(list(...)$hypothesis)
  predict_type <- attributes(x)$predict

  # define all columns that should be removed
  remove_columns <- c("s.value", "S", "CI", "rowid_dedup", non_focal)

  # do we have contrasts? For contrasts, we want to keep p-values
  if (is_contrast_analysis) {
    estimate_name <- "Difference"
  } else {
    # for simple means, we don't want p-values
    remove_columns <- c(remove_columns, "p")
    # estimate name
    estimate_name <- .guess_estimate_name(predict_type, info)
  }

  # reshape and format columns
  params <- .standardize_marginaleffects_columns(
    x,
    remove_columns,
    model,
    model_data,
    info,
    ci,
    estimate_name,
    is_contrast_analysis,
    ...
  )

  .set_back_attributes(x, params)
}


#' @export
format.marginaleffects_slopes <- function(x, model, ci = 0.95, ...) {
  # model information
  info <- insight::model_info(model, verbose = FALSE)
  model_data <- insight::get_data(model, verbose = FALSE)
  # define all columns that should be removed
  remove_columns <- c("Predicted", "s.value", "S", "CI", "rowid_dedup")
  # for contrasting slope, we need to keep the "Parameter" column
  # however, for estimating trends/slope, the "Parameter" column is usually
  # redundant. Since we cannot check for class-attributes, we simply check if
  # all values are identical
  if ("term" %in% colnames(x) && insight::n_unique(x$term) == 1) {
    remove_columns <- c("Parameter", remove_columns)
  }
  # there are some exceptions for `estimate_slope()`, when the `Comparison`
  # column contains information about the type of slope (dx/dy etc.). we want
  # to remove this here, but add information as attribute.
  if ("contrast" %in% colnames(x) && all(x$contrast %in% .marginaleffects_slopes())) {
    remove_columns <- c("Comparison", "contrast", remove_columns)
    attr(x, "slope") <- unique(x$contrast)
  }
  # reshape and format columns
  params <- .standardize_marginaleffects_columns(
    x,
    remove_columns,
    model,
    model_data,
    info,
    ci,
    estimate_name = "Slope",
    ...
  )

  .set_back_attributes(x, params)
}


#' @export
format.marginaleffects_contrasts <- function(x, model = NULL, p_adjust = NULL, comparison = NULL, ...) {
  predict <- attributes(x)$predict
  by <- attributes(x)$by
  contrast <- attributes(x)$contrast
  focal_terms <- attributes(x)$focal_terms
  dgrid <- attributes(x)$datagrid

  # sanity check - method "get_marginalmeans()" calls "format.estimate_means()"
  # for printing, and that method doesn't pass "comparison" - thus, we have to
  # extract it from the attributes
  if (is.null(comparison)) {
    comparison <- attributes(x)$comparison
  }

  # clean "by" and contrast variable names, for the special cases. for example,
  # if we have `by = "name [fivenum]"`, we just want "name"
  for (i in focal_terms) {
    if (!is.null(by) && any(startsWith(by, i)) && !any(by %in% i)) {
      by[startsWith(by, i)] <- i
    }
    if (!is.null(contrast) && any(startsWith(contrast, i)) && !any(contrast %in% i)) {
      contrast[startsWith(contrast, i)] <- i
    }
  }

  # only when we have a comparison based on these options from marginaleffects,
  # we want to "clean" the parameter names
  valid_options <- .valid_hypothesis_strings()

  # Column name for coefficient - fix needed for contrasting slopes
  colnames(x)[colnames(x) == "Slope"] <- "Difference"

  # for contrasting slopes, we do nothing more here. for other contrasts,
  # we prettify labels now
  if (!is.null(comparison) && is.character(comparison) && comparison %in% valid_options) {
    #  the goal here is to create tidy columns with the comparisons.
    # marginaleffects returns a single column that contains all levels that
    # are contrasted. We want to have the contrasted levels per predictor in
    # a separate column. This is what we do here...

    # split parameter column into comparison groups.
    params <- as.data.frame(do.call(
      rbind,
      lapply(x$Parameter, .split_at_minus_outside_parentheses)
    ))

    # we *could* stop here and simply rename the split columns, but then
    # we cannot filter by `by` - thus, we go on, extract all single levels,
    # combine only relevant levels and then we're able to filter...

    # When we filter contrasts, e.g. `contrast = c("vs", "am='1'")` or
    # `contrast = c("vs", "am"), by = "gear='5'"`, we get no contrasts if one
    # of the focal terms only has one unique value in the data grid. Thus,
    # we need to exclude all those focal terms that only have one unique value
    # in the data grid now. Fingers crossed that it works...
    focal_terms <- focal_terms[lengths(lapply(dgrid[focal_terms], unique)) > 1]

    # in the second example, `contrast = c("vs", "am"), by = "gear='5'"`, the
    # `by` column is the one with one unique value only, we thus have to update
    # `by` as well, and also `contrast` (the latter not(!) for numerics)...
    by <- by[lengths(lapply(dgrid[by], unique)) > 1]

    # for contrasts, we also filter variables with one unique value, but we
    # keep numeric variables. When these are hold constant in the data grid,
    # they are set to their mean value - meaning, they only have one unique
    # value in the data grid, anyway. so we need to keep them
    keep_contrasts <- lengths(lapply(dgrid[contrast], unique)) > 1 | vapply(dgrid[contrast], is.numeric, logical(1)) # nolint
    contrast <- contrast[keep_contrasts]

    # set to NULL, if all by-values have been removed here
    if (!length(by)) by <- NULL

    # if we have no contrasts left, e.g. due to `contrast = "time = factor(2)"`,
    # we error here - we have no contrasts to show
    if (!length(contrast)) {
      insight::format_error("No contrasts to show. Please adjust `contrast`.")
    }

    # contrasts can't be longer than focal terms - make sure we have not
    # removed too much (and that we now have captured all exceptions...)
    if (length(contrast) > length(focal_terms)) focal_terms <- contrast

    # for more than one term, we have comma-separated levels.
    if (length(focal_terms) > 1) {
      # we now have a data frame with each comparison-pairs as single column.
      # next, we need to separate the levels from the different variables at the ","
      params <- datawizard::data_separate(
        params,
        separator = ",",
        guess_columns = "max",
        verbose = FALSE
      )
      new_colnames <- paste0(
        rep.int(focal_terms, 2),
        rep(1:2, each = length(focal_terms))
      )
    } else {
      new_colnames <- paste0(focal_terms, 1:2)
    }

    # sanity check - for some edgecases, we have fewer columns than expected
    # then just don't prettify anything
    if (length(new_colnames) == ncol(params)) {
      colnames(params) <- new_colnames

      # make sure all whitespaces are removed
      params[] <- lapply(params, insight::trim_ws)

      # unite back columns with focal contrasts - only needed when not slopes
      if (inherits(x, "estimate_slopes")) {
        contrast <- by
        by <- NULL
      }

      # if we have more than one contrast term, we unite the levels from
      # all contrast terms that belong to one "contrast group", separated
      # by comma, and each the two "new contrast groups" go into separate
      # columns named "Level1" and "Level2". For one contrast term, we just
      # need to rename the columns
      if (length(contrast) == 1) {
        # rename columns
        params <- datawizard::data_rename(
          params,
          select = paste0(contrast, 1:2),
          replacement = c("Level1", "Level2"),
          verbose = FALSE
        )
      } else {
        # prepare all contrast terms
        for (i in 1:2) {
          contrast_names <- paste0(contrast, i)
          params <- .fix_duplicated_contrastlevels(params, contrast_names)
          # finally, unite levels back into single column
          params <- datawizard::data_unite(
            params,
            new_column = paste0("Level", i),
            select = contrast_names,
            separator = ", ",
            verbose = FALSE
          )
        }
      }

      # we need to update these variables, because these are the new column
      # names for contrasts and focal terms
      contrast <- focal_terms <- c("Level1", "Level2")

      # ------------------------------------------------------------------
      # old code for the display was just:
      #
      # for (i in seq_along(contrast)) {
      #   contrast_names <- paste0(contrast[i], 1:2)
      #   params <- datawizard::data_unite(
      #     params,
      #     new_column = contrast[i],
      #     select = contrast_names,
      #     separator = " - ",
      #     verbose = FALSE
      #   )
      # }
      # ------------------------------------------------------------------

      # filter by "by" variables
      if (!is.null(by)) {
        keep_rows <- seq_len(nrow(params))
        for (i in by) {
          by_names <- paste0(i, 1:2)
          keep_rows <- keep_rows[apply(params[by_names], 1, function(j) {
            all(j == j[1])
          })]
        }

        # here we make sure that one of the "by" column has its original
        # column name back, so we can properly merge all variables in
        # "contrast" and "by" to the original data
        by_columns <- paste0(by, 1)
        params <- datawizard::data_rename(
          params,
          select = by_columns,
          replacement = by,
          verbose = FALSE
        )

        # filter original data and new params by "by"
        x <- x[keep_rows, ]
        params <- params[keep_rows, ]
      }

      # remove old column
      x$Parameter <- NULL

      # add back new columns
      x <- cbind(params[c(contrast, by)], x)

      # make sure terms are factors, for data_arrange later
      for (i in focal_terms) {
        x[[i]] <- factor(x[[i]], levels = unique(x[[i]]))
      }
    }
  }

  x
}


# Helper ----------------------------------------------------------------------
# -----------------------------------------------------------------------------


# since we combine levels from different factors, we have to make
# sure levels are unique across different terms. If not, paste
# variable names to levels. We first find the intersection of all
# levels from all current contrast terms

.fix_duplicated_contrastlevels <- function(params, contrast_names) {
  multiple_levels <- Reduce(
    function(i, j) intersect(i, j),
    lapply(params[contrast_names], unique),
    accumulate = FALSE
  )
  # if we find any intersections, we have identical labels for different
  # terms in one "contrast group" - we thus add the variable name to the
  # levels, to avoid identical levels without knowing to which factor
  # it belongs
  if (length(multiple_levels)) {
    for (cn in contrast_names) {
      params[[cn]] <- paste(gsub(".{1}$", "", cn), params[[cn]])
    }
  }
  params
}


# This function renames columns to have a consistent naming scheme,
# and relocates columns to get a standardized column order across all
# outputs from {marginaleffects}

#' @keywords internal
.standardize_marginaleffects_columns <- function(x,
                                                 remove_columns,
                                                 model,
                                                 model_data,
                                                 info,
                                                 ci = 0.95,
                                                 estimate_name = NULL,
                                                 is_contrast_analysis = FALSE,
                                                 ...) {
  # tidy output - we want to tidy the output, using `model_parameters()` or
  # `describe_posterior()` for Bayesian models. We also need to know how the
  # coefficient column is named, because we replace that column name with an
  # appropriate name of the predictions (e.g. "Difference", "Probability" or
  # "Mean")
  if (is.null(attributes(x)$posterior_draws)) {
    # frequentist
    params <- suppressWarnings(parameters::model_parameters(x, verbose = FALSE))
    coefficient_name <- intersect(
      c(attributes(params)$coefficient_name, "Coefficient", "Slope", "Predicted"),
      colnames(params)
    )[1]
  } else {
    # Bayesian
    params <- suppressWarnings(bayestestR::describe_posterior(x, verbose = FALSE, ...))
    ## FIXME: needs to be fixed in bayestestR: categorical models don't return group column
    # see https://github.com/easystats/bayestestR/issues/692
    if (info$is_categorical) {
      params$group <- .safe(x$group)
    }
    coefficient_name <- intersect(
      c(attributes(params)$coefficient_name, "Median", "Mean", "MAP"),
      colnames(params)
    )[1]
    # we need to remove some more columns
    remove_columns <- c(remove_columns, "rowid")
    # and modify the estimate name - if it's not a dpar
    if (!is.null(estimate_name) && !tolower(estimate_name) %in% .brms_aux_elements()) {
      estimate_name <- coefficient_name
    }
  }
  # rename the "term" and "hypothesis" column (which we get from contrasts)
  colnames(params)[colnames(params) == "term"] <- "Parameter"
  colnames(params)[colnames(params) == "hypothesis"] <- "Parameter"

  # add back ci? these are missing when contrasts are computed
  params <- .add_contrasts_ci(is_contrast_analysis, params)

  # relocate columns - this is the standardized column order for all outputs
  relocate_columns <- intersect(
    unique(c(
      coefficient_name, "Coefficient", "Slope", "Predicted", "Median", "Mean",
      "MAP", "SE", "CI_low", "CI_high", "Statistic", "df", "df_error", "pd",
      "ps", "ROPE_low", "ROPE_high", "ROPE_Percentage", "p"
    )),
    colnames(params)
  )
  params <- params[c(setdiff(colnames(params), relocate_columns), relocate_columns)]

  # relocate focal terms to the beginning
  by <- attr(x, "focal_terms", exact = TRUE)
  if (!is.null(by) && all(by %in% colnames(params))) {
    params <- datawizard::data_reorder(params, by, verbose = FALSE)
  }

  # rename coefficient name and statistics columns
  if (!is.null(estimate_name)) {
    params <- datawizard::data_rename(
      params,
      select = coefficient_name,
      replacement = estimate_name
    )
  }
  if ("Statistic" %in% colnames(params)) {
    params <- datawizard::data_rename(
      params,
      select = "Statistic",
      replacement = gsub("-statistic", "", insight::find_statistic(model), fixed = TRUE)
    )
  }

  # remove redundant columns
  params <- datawizard::data_remove(params, remove_columns, verbose = FALSE) # nolint

  # Rename for Categorical family
  if (info$is_categorical || info$is_ordinal || info$is_cumulative) {
    params <- .safe(datawizard::data_rename(params, "group", "Response"), params)
  }

  # finally, make sure we have original data types
  data.frame(datawizard::data_restoretype(params, model_data))
}


# This function ensures that the formatted object still has all relevant
# information saved as attributes

#' @keywords internal
.set_back_attributes <- function(x, formatted_params) {
  attributes(formatted_params) <- utils::modifyList(
    attributes(formatted_params),
    attributes(x)[.info_elements()]
  )
  formatted_params
}


# for contrasts analysis, CIs are not computed automatically. The `format()`
# methods adds back those CIs by calling this function.

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


# this function tries to find the most approriate name of the estimates / predictions,
# based on on which scale predictions were requested

#' @keywords internal
.guess_estimate_name <- function(predict_type, info) {
  # estimate name
  if (!is.null(predict_type) && tolower(predict_type) %in% .brms_aux_elements()) {
    # for Bayesian models with distributional parameter
    estimate_name <- tools::toTitleCase(predict_type)
  } else if (!predict_type %in% c("none", "link") && (info$is_binomial || info$is_bernoulli)) {
    estimate_name <- "Probability"
  } else if (predict_type %in% c("zprob", "zero")) {
    estimate_name <- "Probability" ## TODO: could be renamed into ZI-Probability?
  } else {
    estimate_name <- "Mean"
  }
  estimate_name
}


# this function is used for "estimate_contrasts()", to split the Parameter
# column into single term columns. {marginaleffects} combines factor levels of
# different comparisons using "-". For factor levels containing "-", the groups
# are put in parentheses. This function splits a string at "-" if it's outside
# parentheses

#' @keywords internal
.split_at_minus_outside_parentheses <- function(input_string) {
  pattern <- "\\(([^()]*)\\)|-" # find all the parentheses and the -
  matches <- gregexpr(pattern, input_string, perl = TRUE)
  match_positions <- matches[[1]]
  match_lengths <- attr(matches[[1]], "match.length")

  split_positions <- 0
  for (i in seq_along(match_positions)) {
    if (substring(input_string, match_positions[i], match_positions[i]) == "-") {
      inside_parentheses <- FALSE
      for (j in seq_along(match_positions)) {
        if (i != j && match_positions[i] > match_positions[j] && match_positions[i] < (match_positions[j] + match_lengths[j])) {
          inside_parentheses <- TRUE
          break
        }
      }
      if (!inside_parentheses) {
        split_positions <- c(split_positions, match_positions[i])
      }
    }
  }
  split_positions <- c(split_positions, nchar(input_string) + 1)

  parts <- NULL
  for (i in 1:(length(split_positions) - 1)) {
    parts <- c(
      parts,
      substring(
        input_string,
        split_positions[i] + 1,
        split_positions[i + 1] - 1
      )
    )
  }
  parts <- insight::trim_ws(parts)
  gsub("(", "", gsub(")", "", parts, fixed = TRUE), fixed = TRUE)
}
