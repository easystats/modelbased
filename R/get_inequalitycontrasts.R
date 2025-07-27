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
    # currently, we only support one grouping variable
    if (length(my_args$by) > 2) {
      insight::format_error(
        "`by` can only contain one or two variables for `comparison = \"inequality\"`."
      )
    }
    # setup hypothesis formulas
    if (length(my_args$by) > 1) {
      group <- my_args$by[2]
    } else {
      group <- NULL
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
    # sanity check - by can only be one variable
    if (!is.null(my_args$by) && length(my_args$by) > 1) {
      insight::format_error(
        "`by` can only contain one variable for `comparison = \"inequality\"`."
      )
    }

    if (comparison %in% c("inequality_ratio", "inequality_ratio_pairwise")) {
      # ----------------------------------------------
      # relative inequality measures -----------------
      # ----------------------------------------------

      formulas <- .inequality_formula(comparison, my_args$by)

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
      if (is.null(my_args$by) || !length(my_args$by)) {
        f <- ~ I(mean(abs(x))) | term
      } else {
        f <- stats::as.formula(paste("~I(mean(abs(x))) |", my_args$by))
      }
      # for this special case, we need "avg_comparisons()", else we cannot specify
      # the "variables" argument as named list
      out <- marginaleffects::avg_comparisons(
        model = model,
        variables = as.list(stats::setNames(
          rep_len("pairwise", length(my_args$contrast)),
          my_args$contrast
        )),
        by = ifelse(is.null(my_args$by), TRUE, my_args$by),
        newdata = datagrid,
        hypothesis = f,
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

  attr(out, "hypothesis_by") <- my_args$by
  class(out) <- unique(c("marginaleffects_means", class(out)))
  format(out, model, ci, hypothesis = comparison, ...)
}


# setup hypothesis formula  ------------------------------------------
# --------------------------------------------------------------------

.inequality_formula <- function(comparison, group = NULL) {
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
  !is.null(comparison) &&
    length(comparison) == 1 &&
    is.character(comparison) &&
    comparison %in% c(
      "inequality", "inequality_pairwise",
      "inequality_ratio", "inequality_ratio_pairwise"
    )
}


# return the valid inequality comparison value
.inequality_type <- function(comparison) {
  if (!.is_inequality_comparison(comparison)){
    return(NULL)
  }
  comparison
}
