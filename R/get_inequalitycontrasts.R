# special contrasts: inequality ---------------------------------------------
# ---------------------------------------------------------------------------

get_inequaliycontrasts <- function(
  model,
  model_data,
  my_args,
  comparison,
  ci,
  compute_slopes = FALSE,
  ...
) {
  if (compute_slopes) {
    # marginal effects inequalities for slopes. This does not yet support the
    # "inequality_pairwise" option
    if (comparison == "inequality_pairwise") {
      insight::format_error(
        "`comparison = \"inequality_pairwise\"` is not supported for contrasts of slopes."
      )
    }
    # unlike for categorical predictors, we have to use avg_slopes() here, because
    # `avg_comparisons()` does not support defining the `variables` argument as
    # named list, which we need to specify the pairwise-contrasts. However, we
    # can use the hypothesis argument to specify the pairwise contrasts first, and
    # then calculate the marginal effects inequalities in the second step.
    out <- marginaleffects::avg_slopes(
      model = model,
      variables = my_args$contrast,
      by = my_args$by,
      hypothesis = ~pairwise
    )
    out <- marginaleffects::hypotheses(out, hypothesis = ~ I(mean(abs(x))))
    # save some labels for printing
    attr(out, "hypothesis_by") <- my_args$by
    attr(out, "trend") <- my_args$contrast
    attr(out, "compute_slopes") <- TRUE
    hypothesis <- "inequality"
  } else {
    # to calculate marginal effects inequalities, all contrast predictors
    # must be factors
    check_factors <- .safe(vapply(model_data[my_args$contrast], is.factor, logical(1)), NULL)
    if (is.null(check_factors) || !all(check_factors)) {
      insight::format_error(
        "All variables specified in `contrast` must be factors for `comparison = \"inequality\"`."
      )
    }
    # setup formula for hypothesis argument. use "term" as grouping variable
    # when we don't have a "by" argument, else use the "by" argument as grouping
    # variable
    if (is.null(my_args$by) || !length(my_args$by)) {
      f <- ~ I(mean(abs(x))) | term
    } else {
      # sanity check - by can only be one variable
      if (length(my_args$by) > 1) {
        insight::format_error(
          "`by` can only contain one variable for `comparison = \"inequality\"`."
        )
      }
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
      hypothesis = f,
      ...
    )
    # for the total marginal effects, we need to call "hypothesis()" again, this
    # time with ~revpairwise option
    if (comparison == "inequality_pairwise") {
      if (nrow(out) < 2) {
        insight::format_error(
          "Pairwise comparisons require at least two marginal effects inequalities measures."
        )
      }
      out <- marginaleffects::hypotheses(out, hypothesis = ~revpairwise)
      attr(out, "hypothesis_by") <- my_args$by
      hypothesis <- "inequality_pairwise"
    } else {
      hypothesis <- "inequality"
    }
  }

  class(out) <- unique(c("marginaleffects_means", class(out)))
  format(out, model, ci, hypothesis = hypothesis, ...)
}


# handle inequality hypothesis  --------------------------------------
# --------------------------------------------------------------------

# check whether we have a formula definition of inequality comparisons,
# and convert it to a string
.check_for_inequality_comparison <- function(comparison) {
  if (inherits(comparison, "formula")) {
    out <- paste(all.vars(comparison), collapse = "_")
    if (out == "ratio_inequality") {
      out <- "inequality_ratio"
    }
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
    comparison %in% c("inequality", "inequality_pairwise", "inequality_ratio")
}
