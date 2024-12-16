#' @rdname get_marginalmeans
#'
#' @param method Contrast method.
#' @export
get_marginalcontrasts <- function(model,
                                  by = NULL,
                                  ci = 0.95,
                                  method = "pairwise",
                                  ...) {
  out <- estimate_means(
    model = model,
    by = by,
    ci = ci,
    hypothesis = method,
    backend = "marginaleffects",
    ...
  )

  attr(out, "table_title") <- c("Marginal Contrasts Analysis", "blue")
  attr(out, "table_footer") <- .estimate_means_footer(out, by = by, type = "contrasts", p_adjust = NULL)

  out
}

#' @rdname get_marginalmeans
#' @export
model_marginalcontrasts <- get_marginalcontrasts
