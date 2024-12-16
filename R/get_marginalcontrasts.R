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

  out <- estimate_means(
    model = model,
    by = c(contrast, by),
    ci = ci,
    hypothesis = method,
    transform = "response",
    backend = "marginaleffects",
    ...
  )

  attr(out, "contrast") <- contrast
  out
}

#' @rdname get_marginalmeans
#' @export
model_marginalcontrasts <- get_marginalcontrasts
