# special contrasts: counterfactual -----------------------------------------
# ---------------------------------------------------------------------------

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

  # create a data grid -------------------------------------------
  # ---------------------------------------------------------------------------

  out <- .get_datagrid_means(
    model,
    c(my_args$contrast, my_args$by),
    estimate = "population",
    dots
  )
  counter_grid <- out$datagrid
  datagrid_info <- out$datagrid_info

  # weights?
  # ---------------------------

  # handle weights - argument is named "wts" in marginal effects
  if (!is.null(dots$weights)) {
    dots$wts <- dots$weights
    dots$weights <- NULL
  }

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
