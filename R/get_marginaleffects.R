#' @rdname get_marginalmeans
#'
#' @inheritParams get_emmeans
#'
#' @examplesIf insight::check_if_installed("marginaleffects", quietly = TRUE)
#' model <- lm(Sepal.Width ~ Species * Petal.Length, data = iris)
#'
#' get_marginaleffects(model, trend = "Petal.Length", by = "Species")
#' get_marginaleffects(model, trend = "Petal.Length", by = "Petal.Length")
#' get_marginaleffects(model, trend = "Petal.Length", by = c("Species", "Petal.Length"))
#' @export
get_marginaleffects <- function(model,
                                trend = NULL,
                                by = NULL,
                                ...) {
  # check if available
  insight::check_if_installed("marginaleffects")

  # Guess arguments
  if (is.null(trend)) {
    trend <- insight::find_predictors(model, effects = "fixed", flatten = TRUE)[1]
    insight::format_warning(
      paste0("`trend` cannot be NULL when using marginaleffects to compute overall effects. Selecting '", trend, "'.")
    )
  }

  if (is.null(by)) {
    by <- insight::find_predictors(model, effects = "fixed", flatten = TRUE)
    by <- by[!by %in% trend]
  }

  datagrid <- insight::get_datagrid(model, by = by, verbose = FALSE, ...)
  at_specs <- attributes(datagrid)$at_specs

  # Compute stuff
  estimated <- marginaleffects::avg_slopes(
    model,
    variables = trend,
    by = at_specs$varname,
    newdata = datagrid,
    ...
  )

  attr(estimated, "trend") <- trend
  attr(estimated, "at") <- by
  attr(estimated, "by") <- by
  estimated
}
