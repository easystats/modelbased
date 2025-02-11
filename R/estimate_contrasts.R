#' Estimate Marginal Contrasts
#'
#' Run a contrast analysis by estimating the differences between each level of a
#' factor. See also other related functions such as [estimate_means()]
#' and [estimate_slopes()].
#'
#' @param contrast A character vector indicating the name of the variable(s)
#' for which to compute the contrasts.
#' @param p_adjust The p-values adjustment method for frequentist multiple
#' comparisons. Can be one of `"none"` (default), `"hochberg"`, `"hommel"`,
#' `"bonferroni"`, `"BH"`, `"BY"`, `"fdr"`, `"tukey"`, `"sidak"`, `"esarey"` or
#' `"holm"`. The `"esarey"` option is specifically for the case of Johnson-Neyman
#' intervals, i.e. when calling `estimate_slopes()` with two numeric predictors
#' in an interaction term. Details for the other options can be found in the
#' p-value adjustment section of the `emmeans::test` documentation or
#' `?stats::p.adjust`.
#' @param comparison Specify the type of contrasts or tests that should be
#' carried out.
#' * When `backend = "emmeans"`, can be one of `"pairwise"`, `"poly"`,
#'   `"consec"`, `"eff"`, `"del.eff"`, `"mean_chg"`, `"trt.vs.ctrl"`,
#'   `"dunnett"`, `"wtcon"` and some more. See also `method` argument in
#'   [emmeans::contrast] and the `?emmeans::emmc-functions`.
#' * For `backend = "marginaleffects"`, can be a numeric value, vector, or
#'   matrix, a string equation specifying the hypothesis to test, a string
#'   naming the comparison method, a formula, or a function. Strings, string
#'   equations and formula are probably the most common options and described
#'   below. For other options and detailed descriptions of those options, see
#'   also [marginaleffects::comparisons] and
#'   [this website](https://marginaleffects.com/bonus/hypothesis.html).
#'   * String: One of `"pairwise"`, `"reference"`, `"sequential"`, `"meandev"`
#'     `"meanotherdev"`, `"poly"`, `"helmert"`, or `"trt_vs_ctrl"`.
#'   * String equation: To identify parameters from the output, either specify
#'     the term name, or `"b1"`, `"b2"` etc. to indicate rows, e.g.:`"hp = drat"`,
#'     `"b1 = b2"`, or `"b1 + b2 + b3 = 0"`.
#'   * Formula: A formula like `comparison ~ pairs | group`, where the left-hand
#'     side indicates the type of comparison (`difference` or `ratio`), the
#'     right-hand side determines the pairs of estimates to compare (`reference`,
#'     `sequential`, `meandev`, etc., see string-options). Optionally, comparisons
#'     can be carried out within subsets by indicating the grouping variable
#'     after a vertical bar ( `|`).
#' @inheritParams estimate_means
#'
#' @inherit estimate_slopes details
#'
#' @examplesIf all(insight::check_if_installed(c("lme4", "marginaleffects", "rstanarm"), quietly = TRUE))
#' \dontrun{
#' # Basic usage
#' model <- lm(Sepal.Width ~ Species, data = iris)
#' estimate_contrasts(model)
#'
#' # Dealing with interactions
#' model <- lm(Sepal.Width ~ Species * Petal.Width, data = iris)
#'
#' # By default: selects first factor
#' estimate_contrasts(model)
#'
#' # Can also run contrasts between points of numeric, stratified by "Species"
#' estimate_contrasts(model, contrast = "Petal.Width", by = "Species")
#'
#' # Or both
#' estimate_contrasts(model, contrast = c("Species", "Petal.Width"), length = 2)
#'
#' # Or with custom specifications
#' estimate_contrasts(model, contrast = c("Species", "Petal.Width=c(1, 2)"))
#'
#' # Or modulate it
#' estimate_contrasts(model, by = "Petal.Width", length = 4)
#'
#' # Standardized differences
#' estimated <- estimate_contrasts(lm(Sepal.Width ~ Species, data = iris))
#' standardize(estimated)
#'
#' # Other models (mixed, Bayesian, ...)
#' data <- iris
#' data$Petal.Length_factor <- ifelse(data$Petal.Length < 4.2, "A", "B")
#'
#' model <- lme4::lmer(Sepal.Width ~ Species + (1 | Petal.Length_factor), data = data)
#' estimate_contrasts(model)
#'
#' data <- mtcars
#' data$cyl <- as.factor(data$cyl)
#' data$am <- as.factor(data$am)
#'
#' model <- rstanarm::stan_glm(mpg ~ cyl * wt, data = data, refresh = 0)
#' estimate_contrasts(model)
#' estimate_contrasts(model, by = "wt", length = 4)
#'
#' model <- rstanarm::stan_glm(
#'   Sepal.Width ~ Species + Petal.Width + Petal.Length,
#'   data = iris,
#'   refresh = 0
#' )
#' estimate_contrasts(model, by = "Petal.Length = [sd]", test = "bf")
#' }
#'
#' @return A data frame of estimated contrasts.
#' @export
estimate_contrasts <- function(model, ...) {
  UseMethod("estimate_contrasts")
}


#' @rdname estimate_contrasts
#' @export
estimate_contrasts.default <- function(model,
                                       contrast = NULL,
                                       by = NULL,
                                       predict = NULL,
                                       ci = 0.95,
                                       comparison = "pairwise",
                                       estimate = "typical",
                                       p_adjust = "none",
                                       transform = NULL,
                                       backend = getOption("modelbased_backend", "marginaleffects"),
                                       verbose = TRUE,
                                       ...) {
  if (backend == "emmeans") {
    # Emmeans ------------------------------------------------------------------
    estimated <- get_emcontrasts(model,
      contrast = contrast,
      by = by,
      predict = predict,
      comparison = comparison,
      adjust = p_adjust,
      verbose = verbose,
      ...
    )
    out <- .format_emmeans_contrasts(model, estimated, ci, p_adjust, ...)
  } else {
    # Marginalmeans ------------------------------------------------------------
    estimated <- get_marginalcontrasts(model,
      contrast = contrast,
      by = by,
      predict = predict,
      comparison = comparison,
      p_adjust = p_adjust,
      ci = ci,
      estimate = estimate,
      transform = transform,
      verbose = verbose,
      ...
    )
    out <- format(estimated, model, p_adjust, comparison, ...)
  }

  # restore attributes later
  info <- attributes(estimated)

  # Table formatting
  attr(out, "table_title") <- c(ifelse(
    estimate == "specific",
    "Model-based Contrasts Analysis",
    "Marginal Contrasts Analysis"
  ), "blue")
  attr(out, "table_footer") <- .table_footer(
    out,
    by = info$contrast,
    type = "contrasts",
    model = model,
    info = info
  )

  # Add attributes
  attr(out, "model") <- model
  attr(out, "response") <- insight::find_response(model)
  attr(out, "ci") <- ci
  attr(out, "p_adjust") <- p_adjust
  attr(out, "backend") <- backend

  # add attributes from workhorse function
  attributes(out) <- utils::modifyList(attributes(out), info[.info_elements()])

  # Output
  class(out) <- c("estimate_contrasts", "see_estimate_contrasts", class(out))
  out
}
