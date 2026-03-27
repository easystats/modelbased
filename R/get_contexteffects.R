# special contrasts: context effects ----------------------------------------
# ---------------------------------------------------------------------------

get_contexteffects <- function(model, my_args, ci, ...) {
  out <- marginaleffects::avg_comparisons(
    model,
    variables = my_args$contrast,
    hypothesis = my_args$comparison,
    ...
  )
  # save some labels for printing
  attr(out, "by") <- my_args$by
  attr(out, "contrast") <- my_args$contrast
  attr(out, "context_effects") <- TRUE
  class(out) <- unique(c("marginaleffects_means", class(out)))
  out
}
