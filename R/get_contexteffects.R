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
  if (model_info$is_linear) {
    out <- marginaleffects::avg_comparisons(
      model,
      variables = my_args$contrast,
      hypothesis = my_args$comparison,
      ...
    )
  } else {
    dots <- list(...)
    fun_args <- list(model, variables = my_args$contrast, hypothesis = my_args$comparison)
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
    out <- do.call(marginaleffects::avg_comparisons, c(fun_args, dots))
  }
  # save some labels for printing
  attr(out, "by") <- my_args$by
  attr(out, "contrast") <- my_args$contrast
  attr(out, "context_effects") <- TRUE
  class(out) <- unique(c("marginaleffects_means", class(out)))
  out
}
