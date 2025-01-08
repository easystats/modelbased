#' @rdname get_marginalmeans
#'
#' @param method Contrast method, respectively a formulation of the hypothesis
#' to test. See [this website](https://marginaleffects.com/bonus/hypothesis.html).
#' Will be passed to the `hypothesis` argument in `marginaleffects::avg_predictions()`.
#' @inheritParams get_marginalmeans
#' @inheritParams get_emcontrasts
#' @export
get_marginalcontrasts <- function(model,
                                  contrast = NULL,
                                  by = NULL,
                                  predict = NULL,
                                  method = "pairwise",
                                  ci = 0.95,
                                  p_adjust = NULL,
                                  ...) {
  # check if available
  insight::check_if_installed("marginaleffects")

  # set default, if NULL
  if (is.null(contrast)) {
    contrast <- "auto"
  }

  # Guess arguments
  my_args <- .guess_marginaleffects_arguments(model, by, contrast, ...)

  out <- estimate_means(
    model = model,
    ## TODO: once .format_marginaleffects_contrasts() is working, we have to
    ## pass only "contrast" to the `by` argument, and use `my_args$by` for
    ## filtering...
    by = unique(c(my_args$contrast, my_args$by)),
    ci = ci,
    hypothesis = method,
    predict = predict,
    backend = "marginaleffects",
    p_adjust = p_adjust,
    ...
  )

  attr(out, "contrast") <- my_args$contrast
  attr(out, "predict") <- predict
  attr(out, "at") <- my_args$by
  attr(out, "by") <- my_args$by

  out
}
