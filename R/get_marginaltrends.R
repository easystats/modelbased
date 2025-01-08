#' @rdname get_emmeans
#'
#' @examplesIf insight::check_if_installed("marginaleffects", quietly = TRUE)
#' model <- lm(Sepal.Width ~ Species * Petal.Length, data = iris)
#'
#' get_marginaltrends(model)
#' get_marginaltrends(model, by = "Species")
#' get_marginaltrends(model, by = "Petal.Length")
#' get_marginaltrends(model, by = c("Species", "Petal.Length"))
#'
#' model <- lm(Petal.Length ~ poly(Sepal.Width, 4), data = iris)
#' get_marginaltrends(model)
#' get_marginaltrends(model, by = "Sepal.Width")
#' @export
get_marginaltrends <- function(model,
                         trend = NULL,
                         by = NULL,
                         predict = NULL,
                         ci = 0.95,
                         ...) {
  # check if available
  insight::check_if_installed("marginaleffects")

  # Guess arguments
  my_args <- modelbased:::.guess_emtrends_arguments(model, trend, by)

  # setup arguments
  dg_args <- list(
    model,
    by = my_args$by,
    factors = "all",
    include_random = TRUE,
    verbose = FALSE
  )

  # add user-arguments from "...", but remove those arguments that are already set
  dots <- list(...)
  dots[c("by", "factors", "include_random", "verbose")] <- NULL
  dg_args <- insight::compact_list(c(dg_args, dots))

  # Get corresponding datagrid (and deal with particular ats)
  datagrid <- do.call(insight::get_datagrid, dg_args)
  at_specs <- attributes(datagrid)$at_specs

  # model df
  dof <- insight::get_df(model, verbose = FALSE)

  # setup arguments
  fun_args <- list(
    model,
    by = at_specs$varname,
    newdata = as.data.frame(datagrid),
    variables =  my_args$trend,
    conf_level = ci,
    df = dof
  )

  # handle distributional parameters
  if (predict %in% .brms_aux_elements() && inherits(model, "brmsfit")) {
    fun_args$dpar <- predict
  } else {
    fun_args$type <- predict
  }

  # add user-arguments from "...", but remove those arguments that are already set
  dots[c("by", "newdata", "conf_level", "df", "type", "verbose")] <- NULL
  fun_args <- insight::compact_list(c(fun_args, dots))

  ## TODO: need to check against different mixed models results from other packages
  # set to NULL
  fun_args$re.form <- NULL

  # Run marginaleffects
  slopes <- suppressWarnings(do.call(marginaleffects::avg_slopes, fun_args))

  attr(slopes, "trend") <- my_args$trend
  attr(slopes, "by") <- my_args$by
  slopes
}



# model <- lm(Sepal.Width ~ Species * Petal.Length, data = iris)
# get_marginalslopes(model, trend = "Petal.Length", by = NULL)
# trend <- "Petal.Length"
# by <- "Species"
# dots <- NULL
