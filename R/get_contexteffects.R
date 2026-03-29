# special contrasts: context effects ----------------------------------------
# ---------------------------------------------------------------------------

get_contexteffects <- function(model, my_args, model_info, ...) {
  if (model_info$is_linear) {
    out <- marginaleffects::avg_comparisons(
      model,
      variables = my_args$contrast,
      hypothesis = my_args$comparison,
      ...
    )
  } else {
    dots <- list(...)
    fun_args <- list(
      model,
      variables = my_args$contrast,
      hypothesis = my_args$comparison,
    )
    if (is.null(dots$type)) {
      fun_args$type <- "link"
    }
    if (is.null(dots$transform)) {
      fun_args$transform <- "exp"
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
