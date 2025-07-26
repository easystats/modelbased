#' Estimate Marginal Contrasts
#'
#' Run a contrast analysis by estimating the differences between each level of a
#' factor. See also other related functions such as [estimate_means()]
#' and [estimate_slopes()].
#'
#' @param contrast A character vector indicating the name of the variable(s) for
#' which to compute the contrasts, optionally including representative values or
#' levels at which contrasts are evaluated (e.g., `contrast="x=c('a','b')"`).
#' @param p_adjust The p-values adjustment method for frequentist multiple
#' comparisons. Can be one of `"none"` (default), `"hochberg"`, `"hommel"`,
#' `"bonferroni"`, `"BH"`, `"BY"`, `"fdr"`, `"tukey"`, `"sidak"`, `"sup-t"`,
#' `"esarey"` or `"holm"`. The `"esarey"` option is specifically for the case of
#' Johnson-Neyman intervals, i.e. when calling `estimate_slopes()` with two
#' numeric predictors in an interaction term. `"sup-t"` computes simultaneous
#' confidence bands, also called sup-t confidence band (Montiel Olea &
#' Plagborg-Møller, 2019). Details for the other options can be found in the
#' p-value adjustment section of the `emmeans::test` documentation or
#' `?stats::p.adjust`. Note that certain options provided by the **emmeans**
#' package are only available if you set `backend = "emmeans"`.
#' @param comparison Specify the type of contrasts or tests that should be
#' carried out.
#' * When `backend = "emmeans"`, can be one of `"pairwise"`, `"poly"`,
#'   `"consec"`, `"eff"`, `"del.eff"`, `"mean_chg"`, `"trt.vs.ctrl"`,
#'   `"dunnett"`, `"wtcon"` and some more. To test multiple hypotheses jointly
#'   (usually used for factorial designs), `comparison` can also be `"joint"`.
#'   See also `method` argument in [emmeans::contrast] and the
#'   `?emmeans::emmc-functions`.
#' * For `backend = "marginaleffects"`, can be a numeric value, vector, or
#'   matrix, a string equation specifying the hypothesis to test, a string
#'   naming the comparison method, a formula, or a function. For options not
#'   described below, see documentation of [marginaleffects::comparisons],
#'   [this website](https://marginaleffects.com/bonus/hypothesis.html) and
#'   section _Comparison options_ below.
#'   * String: One of `"pairwise"`, `"reference"`, `"sequential"`, `"meandev"`
#'     `"meanotherdev"`, `"poly"`, `"helmert"`, or `"trt_vs_ctrl"`. To test
#'     multiple hypotheses jointly (usually used for factorial designs),
#'     `comparison` can also be `"joint"`. In this case, use the `test` argument
#'     to specify which test should be conducted: `"F"` (default) or `"Chi2"`.
#'   * String: Special string options are `"inequality"`, `"inequality_ratio"`,
#'     and `"inequality_pairwise"`. `comparison = "inequality"` computes the
#'     marginal effect inequality summary of categorical predictors' overall
#'     effects, respectively, the comprehensive effect of an independent
#'     variable across all outcome categories of a nominal or ordinal dependent
#'     variable (also called *absolute inequality*, or total marginal effect,
#'     see _Mize and Han, 2025_). `"inequality_ratio"` computes the ratio of
#'     marginal effect inequality measures, also known as *relative inequality*.
#'     This is useful to compare the relative effects of different predictors on
#'     the dependent variable. It provides a measure of how much more or less
#'     inequality one predictor has compared to another.
#'     `comparison = "inequality_pairwise"` and `"inequality_pairwise_ratio"`
#'     compute the difference (pairwise comparisons) between marginal effects
#'     inequality measures.
#'   * String equation: To identify parameters from the output, either specify
#'     the term name, or `"b1"`, `"b2"` etc. to indicate rows, e.g.:`"hp = drat"`,
#'     `"b1 = b2"`, or `"b1 + b2 + b3 = 0"`.
#'   * Formula: A formula like `comparison ~ pairs | group`, where the left-hand
#'     side indicates the type of comparison (`difference` or `ratio`), the
#'     right-hand side determines the pairs of estimates to compare (`reference`,
#'     `sequential`, `meandev`, etc., see string-options). Optionally, comparisons
#'     can be carried out within subsets by indicating the grouping variable
#'     after a vertical bar ( `|`).
#'   * A custom function, e.g. `comparison = myfun`, or
#'     `comparison ~ I(my_fun(x)) | groups`.
#'   * If contrasts should be calculated (or grouped by) factors, `comparison`
#'     can also be a matrix that specifies factor contrasts (see 'Examples').
#' @param effectsize Desired measure of standardized effect size, one of
#' `"emmeans"`, `"marginal"`, or `"boot"`. Default is `NULL`, i.e. no effect
#' size will be computed.
#' @param es_type Specifies the type of effect-size measure to estimate when
#' using `effectsize = "boot"`. One of `"unstandardized"`, `"cohens.d"`,
#' `"hedges.g"`, `"cohens.d.sigma"`, `"r"`, or `"akp.robust.d"`. See`
#' effect.type` argument of [bootES::bootES] for details.
#' @param iterations The number of bootstrap resamples to perform.
#' @inheritParams estimate_means
#'
#' @inherit estimate_means details
#'
#' @inheritSection estimate_means Predictions and contrasts at meaningful values (data grids)
#'
#' @inheritSection estimate_means Predictions on different scales
#'
#' @section Comparison options:
#'
#' - `comparison = "pairwise"`: This method computes all possible unique
#'   differences between pairs of levels of the focal predictor. For example, if
#'   a factor has levels A, B, and C, it would compute A-B, A-C, and B-C.
#' - `comparison = "reference"`: This compares each level of the focal predictor
#'   to a specified reference level (by default, the first level). For example,
#'   if levels are A, B, C, and A is the reference, it computes B-A and C-A.
#' - `comparison = "sequential"`: This compares each level to the one
#'   immediately following it in the factor's order. For levels A, B, C, it
#'   would compute B-A and C-B.
#' - `comparison = "meandev"`: This contrasts each level's estimate against the
#'   grand mean of all estimates for the focal predictor.
#' - `comparison = "meanotherdev"`: Similar to `meandev`, but each level's
#'   estimate is compared against the mean of all *other* levels, excluding
#'   itself.
#' - `comparison = "poly"`: These are used for ordered categorical variables to
#'   test for linear, quadratic, cubic, etc., trends across the levels. They
#'   assume equal spacing between levels.
#' - `comparison = "helmert"`: Contrast 2nd level to the first, 3rd to the
#'   average of the first two, and so on. Each level (except the first) is
#'   compared to the mean of the preceding levels. For levels A, B, C, it would
#'   compute B-A and C-(A+B)/2.
#' - `comparison = "trt_vs_ctrl"`: This compares all levels (excluding the
#'   first, which is typically the control) against the first level. It's often
#'   used when comparing multiple treatment groups to a single control group.
#' - To test multiple hypotheses jointly (usually used for factorial designs),
#'   `comparison` can also be `"joint"`. In this case, use the `test` argument
#'   to specify which test should be conducted: `"F"` (default) or `"Chi2"`.
#' - `comparison = "inequality"` computes the *absolute inequality* of groups,
#'   or in other words, the marginal effect inequality summary of categorical
#'   predictors' overall effects, respectively, the comprehensive effect of an
#'   independent variable across all outcome categories of a nominal or ordinal
#'   dependent variable (total marginal effect, see _Mize and Han, 2025_). The
#'   marginal effect inequality focuses on the heterogeneity of the effects of a
#'   categorical *independent* variable. It helps understand how the effect of
#'   the variable differs across its categories or levels. When the *dependent*
#'   variable is categorical (e.g., logistic, ordinal or multinomial
#'   regression), marginal effect inequality provides a holistic view of how an
#'   independent variable affects a nominal or ordinal *dependent* variable. It
#'   summarizes the overall impact (absolute inequality, or total marginal
#'   effects) across all possible outcome categories.
#' - `comparison = "inequality_ratio"` is comparable to
#'   `comparison = "inequality"`, but instead of calculating the absolute
#'   inequality, it computes the *relative inequality* of groups. This is useful
#'   to compare the relative effects of different predictors on the dependent
#'   variable. It provides a measure of how much more or less inequality one
#'   predictor has
#'   compared to another.
#' - `comparison = "inequality_pairwise"` and `"inequality_pairwise_ratio"`
#'   compute the difference (pairwise comparisons) between marginal effects
#'   inequality measures. Depending on the sign, this measure indicates which of
#'   the predictors has a stronger impact on the dependent variable in terms of
#'   inequalities.
#'
#' @section Effect Size:
#'
#' By default, `estimate_contrasts()` reports no standardized effect size on
#' purpose. Should one request one, some things are to keep in mind. As the
#' authors of *emmeans* write, "There is substantial disagreement among
#' practitioners on what is the appropriate sigma to use in computing effect
#' sizes; or, indeed, whether any effect-size measure is appropriate for some
#' situations. The user is completely responsible for specifying appropriate
#' parameters (or for failing to do so)."
#'
#' In particular, effect size method `"boot"` does not correct for covariates
#' in the model, so should probably only be used when there is just one
#' categorical predictor (with however many levels). Some believe that if there
#' are multiple predictors or any covariates, it is important to re-compute
#' sigma adding back in the response variance associated with the variables that
#' aren't part of the contrast.
#'
#' `effectsize = "emmeans"` uses [emmeans::eff_size] with
#' `sigma = stats::sigma(model)`, `edf = stats::df.residual(model)` and
#' `method = "identity"`. This standardizes using the MSE (sigma). Some believe
#' this works when the contrasts are the only predictors in the model, but not
#' when there are covariates. The response variance accounted for by the
#' covariates should not be removed from the SD used to standardize. Otherwise,
#' _d_ will be overestimated.
#'
#' `effectsize = "marginal"` uses the following formula to compute effect
#' size: `d_adj <- difference * (1- R2)/ sigma`. This standardizes
#' using the response SD with only the between-groups variance on the focal
#' factor/contrast removed. This allows for groups to be equated on their
#' covariates, but creates an appropriate scale for standardizing the response.
#'
#' `effectsize = "boot"` uses bootstrapping (defaults to a low value of
#' 200) through [bootES::bootES]. Adjusts for contrasts, but not for covariates.
#'
#' @references
#' - Mize, T., & Han, B. (2025). Inequality and Total Effect Summary Measures for
#'   Nominal and Ordinal Variables. Sociological Science, 12, 115–157.
#'   \doi{10.15195/v12.a7}
#'
#' - Montiel Olea, J. L., and Plagborg-Møller, M. (2019). Simultaneous
#'   confidence bands: Theory, implementation, and an application to SVARs.
#'   Journal of Applied Econometrics, 34(1), 1–17. \doi{10.1002/jae.2656}
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
#' estimate_contrasts(model, contrast = c("Species", "Petal.Width = c(1, 2)"))
#'
#' # Or modulate it
#' estimate_contrasts(model, by = "Petal.Width", length = 4)
#'
#' # Standardized differences
#' estimated <- estimate_contrasts(lm(Sepal.Width ~ Species, data = iris))
#' standardize(estimated)
#'
#' # custom factor contrasts - contrasts the average effects of two levels
#' # against the remaining third level
#' data(puppy_love, package = "modelbased")
#' cond_tx <- cbind("no treatment" = c(1, 0, 0), "treatment" = c(0, 0.5, 0.5))
#' model <- lm(happiness ~ puppy_love * dose, data = puppy_love)
#' estimate_slopes(model, "puppy_love", by = "dose", comparison = cond_tx)
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
estimate_contrasts.default <- function(
  model,
  contrast = NULL,
  by = NULL,
  predict = NULL,
  ci = 0.95,
  comparison = "pairwise",
  estimate = NULL,
  p_adjust = "none",
  transform = NULL,
  keep_iterations = FALSE,
  effectsize = NULL,
  iterations = 200,
  es_type = "cohens.d",
  backend = NULL,
  verbose = TRUE,
  ...
) {
  # Process argument ---------------------------------------------------------
  # --------------------------------------------------------------------------

  # set defaults
  if (is.null(estimate)) {
    estimate <- getOption("modelbased_estimate", "typical")
  }
  if (is.null(backend)) {
    backend <- getOption("modelbased_backend", "marginaleffects")
  }
  comparison <- .check_for_inequality_comparison(comparison)

  if (backend == "emmeans") {
    # Emmeans ----------------------------------------------------------------
    estimated <- get_emcontrasts(
      model,
      contrast = contrast,
      by = by,
      predict = predict,
      comparison = comparison,
      keep_iterations = keep_iterations,
      adjust = p_adjust,
      verbose = verbose,
      ...
    )
    out <- .format_emmeans_contrasts(model, estimated, ci, p_adjust, ...)
  } else {
    # Marginalmeans ----------------------------------------------------------
    estimated <- get_marginalcontrasts(
      model,
      contrast = contrast,
      by = by,
      predict = predict,
      comparison = comparison,
      p_adjust = p_adjust,
      ci = ci,
      estimate = estimate,
      transform = transform,
      keep_iterations = keep_iterations,
      verbose = verbose,
      ...
    )
    out <- format(estimated, model, p_adjust, comparison, ...)
  }

  # add effect size ----------------------------------------------------------
  if (!is.null(effectsize)) {
    out <- .estimate_contrasts_effectsize(
      model = model,
      estimated = estimated,
      contrasts_results = out,
      effectsize = effectsize,
      bootstraps = iterations,
      bootES_type = es_type,
      backend = backend
    )
  }

  # sanity check - did method return standard errors?
  .check_standard_errors(
    out,
    by = by,
    contrast = contrast,
    model = model,
    model_name = deparse(substitute(model)),
    verbose = verbose
  )

  # restore attributes later
  info <- attributes(estimated)
  type <- "contrasts"

  # Table formatting
  if (isTRUE(info$joint_test)) {
    suffix <- "Joint Test"
  } else if (.is_inequality_comparison(comparison)) {
    suffix <- "Inequality Analysis"
    type <- "inequality"
  } else {
    suffix <- "Contrasts Analysis"
  }
  attr(out, "table_title") <- c(
    switch(
      estimate,
      specific = paste("Model-based", suffix),
      typical = paste("Marginal", suffix),
      average = paste("Averaged", suffix),
      population = paste("Counterfactual", suffix, "(G-computation)")
    ),
    "blue"
  )

  attr(out, "table_footer") <- .table_footer(
    out,
    by = info$contrast,
    type = type,
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
