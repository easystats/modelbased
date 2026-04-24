# special contrasts: context effects ----------------------------------------
# ---------------------------------------------------------------------------

.get_contexteffects <- function(
  model,
  my_args,
  predict = NULL,
  transform = NULL,
  model_info,
  ...
) {
  # if we have stratified by another group, we need the difference between
  # contrasts at each group level
  if (is.null(my_args$by)) {
    comparison <- my_args$comparison
  } else {
    comparison <- as.formula(paste("~I(diff(x)) |", my_args$by))
  }

  # prepare arguments
  dots <- list(...)
  fun_args <- insight::compact_list(list(
    model,
    variables = my_args$contrast,
    hypothesis = comparison,
    by = my_args$by
  ))

  # for non-Gaussian models, we need to modify arguments "type" and "transform"
  if (!model_info$is_linear) {
    # set default for "type" argument, if not provided
    if (is.null(predict)) {
      fun_args$type <- "link"
      # if "type" was not provided, also change transform argument. we do
      # this only when user did not provide "type", else - if user provided
      # "type" - we keep the default NULL
      if (is.null(transform)) {
        fun_args$transform <- "exp"
      }
    } else {
      fun_args$type <- predict
      fun_args$transform <- transform
    }
  }

  out <- do.call(marginaleffects::avg_comparisons, c(fun_args, dots))

  # save some labels for printing
  attr(out, "by") <- attr(out, "hypothesis_by") <- my_args$by
  attr(out, "contrast") <- my_args$contrast
  attr(out, "context_effects") <- TRUE
  class(out) <- unique(c("marginaleffects_means", class(out)))
  out
}
