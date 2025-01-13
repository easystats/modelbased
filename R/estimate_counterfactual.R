#' Estimate Average Counterfactual Predictions
#'
#' Todo
#'
#' @inheritParams estimate_means
#'
#' @return A data frame of estimated counterfactual predictions.
#'
#' @references
#' Heiss, A. (2022). Marginal and conditional effects for GLMMs with
#' {marginaleffects}. Andrew Heiss. \doi{10.59350/xwnfm-x1827}
#'
#' @examplesIf all(insight::check_if_installed(c("marginaleffects", "glmmTMB"), quietly = TRUE))
#' library(modelbased)
#' data(Salamanders, package = "glmmTMB")
#' m <- glmmTMB::glmmTMB(
#'   count ~ spp + mined + (1 | site),
#'   ziformula = ~ spp + mined,
#'   family = poisson(),
#'   data = Salamanders
#' )
#'
#' # count component
#' estimate_means(m, "spp", predict = "conditional")
#'
#' # response scale ("full" model)
#' estimate_means(m, "spp")
#'
#' # counterfactual
#' estimate_counterfactuals(m, "spp")
#' @export
estimate_counterfactuals <- function(model,
                                     by = "auto",
                                     predict = NULL,
                                     ci = 0.95,
                                     verbose = TRUE,
                                     ...) {
  out <- estimate_means(
    model,
    by = by,
    predict = predict,
    ci = ci,
    backend = "marginaleffects",
    verbose = verbose,
    counterfactual = TRUE,
    ...
  )

  attr(out, "table_title") <- c("Estimated Average Effects", "blue")
  out
}
