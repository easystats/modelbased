# special contrasts: inequality ---------------------------------------------
# ---------------------------------------------------------------------------

get_inequalitycontrasts <- function(
  model,
  model_data,
  my_args,
  comparison,
  ci,
  compute_slopes = FALSE,
  estimate = NULL,
  ...
) {
  dots <- list(...)
  # extract datagrid?
  if (!is.null(dots$newdata)) {
    datagrid <- dots$newdata
  } else if (identical(estimate, "typical")) {
    datagrid <- .get_datagrid_means(model, my_args, estimate, dots = dots)$datagrid
  } else {
    datagrid <- NULL
  }

  # warn user for very large datagrid
  if (!is.null(datagrid) && nrow(datagrid) > 200) {
    insight::format_warning(
      "The datagrid contains more than 200 rows. This may take a while to compute. To avoid this, consider using `estimate = \"average\"`."
    )
  }

  # `by` can contain one or two variables for inequality comparisons
  if (!is.null(my_args$by) && length(my_args$by) > 2) {
    insight::format_error(
      "`by` can only contain one or two variables for `comparison = \"inequality\"`."
    )
  }

  # Define the grouping variable for marginal effects inequalities:
  # - For slopes: The grouping variable is used only if there are at least two
  #   variables in `by`. This ensures pairwise comparisons of slopes are calculated
  #   across the specified groups.
  # - For categorical focal terms: The grouping variable is always set to the last
  #   variable in `by`, as it defines the categories for inequality comparisons.
  if (is.null(my_args$by) || (length(my_args$by) == 1 && compute_slopes)) {
    group <- NULL
  } else if (inherits(comparison, "formula")) {
    # if we have a formula like `~ inequality | grp1 + grp2`, we then use this
    # interface to allow grouping by mor than one variable for inequality
    # comparisons
    out <- .process_inequality_formula(comparison)
    comparison <- out$comparison
    group <- out$group
  } else {
    # For inequality comparisons, we usually average over all categories of the
    # focal predictors and only use one grouping variable. Sometimes, if we want
    # to include a second variable, but don't want to include it for pairwise
    # comparisons, we can use the `by` argument to specify the grouping variable.
    # In such cases, the first `by` variable is also averaged over, and only the
    # second `by` variable is used for grouping.
    group <- my_args$by[length(my_args$by)]
  }

  # -----------------------------------------------------------
  # inequality comparisons for slopes -------------------------
  # -----------------------------------------------------------

  if (compute_slopes) {
    # marginal effects inequalities for slopes.
    # we need a `by` argument, otherwise, pairwise comparisons of slopes for all
    # combinations of values of the `trend` variable would be calculated.
    if (is.null(my_args$by) || !length(my_args$by)) {
      insight::format_error(
        "`by` argument must be specified for `comparison = \"inequality\"`."
      )
    }
    formulas <- .inequality_formula(comparison, group)

    # unlike for categorical predictors, we have to use avg_slopes() here, because
    # `avg_comparisons()` does not support defining the `variables` argument as
    # named list, which we need to specify the pairwise-contrasts. However, we
    # can use the hypothesis argument to specify the pairwise contrasts first, and
    # then calculate the marginal effects inequalities in the second step.
    out <- marginaleffects::avg_slopes(
      model = model,
      variables = my_args$contrast,
      by = my_args$by,
      newdata = datagrid,
      hypothesis = formulas$f1
    )
    out <- marginaleffects::hypotheses(out, hypothesis = formulas$f2)
    # save some labels for printing
    attr(out, "by") <- my_args$by
    attr(out, "trend") <- my_args$contrast
    attr(out, "compute_slopes") <- TRUE
  } else {
    # -----------------------------------------------------------
    # inequality comparisons for categorical predictors ---------
    # -----------------------------------------------------------

    # to calculate marginal effects inequalities, all contrast predictors
    # must be factors
    check_factors <- .safe(vapply(model_data[my_args$contrast], is.factor, logical(1)), NULL)
    if (is.null(check_factors) || !all(check_factors)) {
      insight::format_error(
        "All variables specified in `contrast` must be factors for `comparison = \"inequality\"`."
      )
    }

    if (comparison %in% c("inequality_ratio", "inequality_ratio_pairwise")) {
      # ----------------------------------------------
      # relative inequality measures -----------------
      # ----------------------------------------------

      formulas <- .inequality_formula(comparison, group)

      out <- marginaleffects::avg_predictions(
        model = model,
        variables = c(my_args$contrast, my_args$by),
        newdata = datagrid,
        hypothesis = formulas$f1
      )
      out <- marginaleffects::hypotheses(out, hypothesis = formulas$f2)
    } else {
      # ----------------------------------------------
      # absolute inequality measures -----------------
      # ----------------------------------------------

      # setup formula for hypothesis argument. use "term" as grouping variable
      # when we don't have a "by" argument, else use the "by" argument as grouping
      # variable
      formulas <- .inequality_formula(comparison, group, "term")

      # update "by" if necessary - we don't use "ifelse()" here, because if
      # my_args$by is a vector of length > 1, we want to keep it as is. `ifelse()`
      # would vectorize and only return the first element.
      if (is.null(my_args$by)) {
        my_args$by <- TRUE
      }
      # for this special case, we need "avg_comparisons()", else we cannot specify
      # the "variables" argument as named list
      out <- marginaleffects::avg_comparisons(
        model = model,
        variables = as.list(stats::setNames(
          rep_len("pairwise", length(my_args$contrast)),
          my_args$contrast
        )),
        by = my_args$by,
        newdata = datagrid,
        hypothesis = formulas$f2,
        ...
      )
    }
  }

  # -----------------------------------------------------------------
  # difference between absolute / relative inequality measures ------
  # -----------------------------------------------------------------
  if (comparison %in% c("inequality_pairwise", "inequality_ratio_pairwise")) {
    if (nrow(out) < 2) {
      insight::format_error(
        "Pairwise comparisons require at least two marginal effects inequalities measures."
      )
    }
    out <- marginaleffects::hypotheses(out, hypothesis = ~revpairwise)
  }

  attr(out, "hypothesis_by") <- group
  class(out) <- unique(c("marginaleffects_means", class(out)))
  format(out, model, ci, hypothesis = comparison, ...)
}


# setup hypothesis formula  ------------------------------------------
# --------------------------------------------------------------------

.inequality_formula <- function(comparison, group = NULL, alternative = NULL) {
  # for some special cases, "group" cannot be NULL, but must be an alternative
  # string
  if (is.null(group) && !is.null(alternative)) {
    group <- alternative
  }
  # combine groups, when longer than one
  if (is.character(group) && length(group) > 1) {
    group <- paste(group, collapse = " + ")
  }
  # specify the pairwise contrasts for the hypothesis argument
  f1 <- switch(
    comparison,
    inequality_pairwise = ,
    inequality = stats::as.formula(paste(c("~ pairwise", group), collapse = " | ")),
    inequality_ratio_pairwise = ,
    inequality_ratio = stats::as.formula(paste(c("ratio ~ pairwise", group), collapse = " | ")),
  )
  f2 <- stats::as.formula(paste(c("~ I(mean(abs(x)))", group), collapse = " | "))
  list(f1 = f1, f2 = f2)
}


# process special formula interface ----------------------------------
# --------------------------------------------------------------------

# if we have a formula like `~ inequality | grp1 + grp2`, we then use this
# interface to allow grouping by two variables for inequality comparisons
# instead of only one (the last) variable.
.process_inequality_formula <- function(comparison) {
  f <- unlist(strsplit(insight::safe_deparse(comparison), "|", fixed = TRUE))
  # check parts left and right of the bar "|"
  left_part <- insight::trim_ws(f[[1]])
  right_part <- insight::trim_ws(f[[2]])
  # update arguments
  if (grepl("pairwise", left_part, fixed = TRUE)) {
    if (grepl("ratio", left_part, fixed = TRUE)) {
      comparison <- "inequality_ratio_pairwise"
    } else {
      comparison <- "inequality_pairwise"
    }
  } else if (grepl("ratio", left_part, fixed = TRUE)) {
    comparison <- "inequality_ratio"
  } else {
    comparison <- "inequality"
  }
  group <- insight::trim_ws(unlist(strsplit(right_part, "+", fixed = TRUE)))

  list(group = group, comparison = comparison)
}


# handle inequality hypothesis  --------------------------------------
# --------------------------------------------------------------------

# check whether we have a formula definition of inequality comparisons,
# and convert it to a string
.check_for_inequality_comparison <- function(comparison) {
  if (inherits(comparison, "formula")) {
    # parse variables into a string
    out <- paste(all.vars(comparison), collapse = "_")
    # handle special cases
    out <- switch(
      out,
      ratio_inequality = "inequality_ratio",
      ratio_inequality_pairwise = "inequality_ratio_pairwise",
      out
    )
    if (.is_inequality_comparison(out)) {
      return(out)
    }
  }
  comparison
}


# check whether we have a valid inequality comparison
.is_inequality_comparison <- function(comparison) {
  # "comparison" can be a string or a formula. If a string, we expect
  # one of the following values:
  inequality_comparisons <- c(
    "inequality",
    "inequality_pairwise",
    "inequality_ratio",
    "inequality_ratio_pairwise"
  )

  if (!is.null(comparison)) {
    if (length(comparison) == 1 && is.character(comparison) && comparison %in% inequality_comparisons) {
      return(TRUE)
    }
    # if we have a formula, we check whether it starts with "inequality". We
    # still may have a formula like `~ inequality | grp1 + grp2`, which is valid
    # to include more than one grouping variable.
    if (inherits(comparison, "formula")) {
      f <- insight::safe_deparse(comparison)
      if (any(startsWith(f, c("~inequality", "inequality")))) {
        return(TRUE)
      }
    }
  }
  FALSE
}


# return the valid inequality comparison value
.inequality_type <- function(comparison) {
  if (!.is_inequality_comparison(comparison)){
    return(NULL)
  }
  comparison
}
