#' @rdname get_marginalmeans
#'
#' @param method Contrast method.
#' @inheritParams get_emcontrasts
#' @export
get_marginalcontrasts <- function(model,
                                  contrast = NULL,
                                  by = NULL,
                                  transform = "none",
                                  method = "pairwise",
                                  ci = 0.95,
                                  ...) {
  # check if available
  insight::check_if_installed("marginaleffects")

  # Guess arguments
  my_args <- .guess_marginalcontrasts_arguments(model, contrast, by, ...)

  out <- estimate_means(
    model = model,
    by = my_args$by,
    ci = ci,
    hypothesis = method,
    transform = transform,
    backend = "marginaleffects",
    ...
  )

  attr(out, "contrast") <- my_args$contrast
  attr(out, "at") <- my_args$by
  attr(out, "by") <- my_args$by
  out
}

#' @rdname get_marginalmeans
#' @export
model_marginalcontrasts <- get_marginalcontrasts
