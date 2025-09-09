# special contrasts: counterfactual -----------------------------------------
#
# For counterfactual contrasts, we use the "avg_comparisons()" function from the
# marginaleffects package, because we cannot simply calculate *pairwise*
# comparisons, but rather we need to calculate the difference between two sets of
# counterfactual predictions, which is what avg_comparisons() does (via the
# "comparison" argument).
get_counterfactualcontrasts <- function(
  model,
  model_info,
  predict,
  my_args,
  comparison,
  ci,
  p_adjust,
  verbose = TRUE,
  ...
) {
  dots <- list(...)

  # setup argument type -------------------------------------------
  # ---------------------------------------------------------------------------

  # find default response-type, and get information about back transformation
  predict_args <- .get_marginaleffects_type_argument(
    model,
    predict,
    comparison,
    model_info,
    verbose,
    ...
  )

  # create a data grid
  # ----------------------------------------------------------------------

  out <- .get_datagrid_means(
    model,
    c(my_args$contrast, my_args$by),
    estimate = "population",
    dots
  )
  counter_grid <- out$datagrid
  datagrid_info <- out$datagrid_info

  # filtered values in contrast and by variables? If the user did not provide
  # just the names of the variables, but also filtering information (e.g.,
  # "varname = c('a','b')"), we need to apply this filtering to the data grid
  # and then remove the filtering information from the variable names. In this
  # particular case, we *need* the newdata-argument
  if (any(grepl("=", my_args$contrast, fixed = TRUE)) || any(grepl("=", my_args$by, fixed = TRUE))) {
    my_args$contrast <- gsub("=.*", "\\1", my_args$contrast)
    my_args$by <- gsub("=.*", "\\1", my_args$by)
    dots$newdata <- counter_grid
  }

  # weights?
  # ---------------------------

  # handle weights - argument is named "wts" in marginal effects
  if (!is.null(dots$weights)) {
    dots$wts <- dots$weights
    dots$weights <- NULL
  }

  # setup arguments
  # -----------------------------------------------------------

  fun_args <- insight::compact_list(c(
    list(
      model,
      variables = my_args$contrast,
      by = my_args$by,
      conf_level = ci,
      type = predict_args$predict
    ),
    dots
  ))

  out <- do.call(marginaleffects::avg_comparisons, fun_args)

  out <- .add_attributes(
    out,
    by = my_args$by,
    model_info = model_info,
    info = c(
      datagrid_info,
      list(
        contrast = my_args$contrast,
        predict = predict,
        comparison = my_args$comparison,
        estimate = "population",
        p_adjust = p_adjust,
        contrast_filter = my_args$contrast_filter,
        datagrid = counter_grid
      )
    )
  )

  class(out) <- unique(c("marginaleffects_means", class(out)))
  format(out, model, ci, hypothesis = my_args$comparison, ...)
}
