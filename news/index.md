# Changelog

## modelbased 0.13.1

CRAN release: 2025-12-08

### Changes

- The `type` argument in
  [`estimate_grouplevel()`](https://easystats.github.io/modelbased/reference/estimate_grouplevel.md)
  gains a `"marginal"` option, to return marginal group-levels
  estimates.

- Added an
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) method
  for *modelbased* objects.

- Better formatting of the output for equivalence-tests, when the
  `equivalence` argument was used. Related docs were added. It is also
  possible to use
  [`parameters::equivalence_test()`](https://easystats.github.io/bayestestR/reference/equivalence_test.html)
  on *modelbased* objects.

- Function calls are now saved as `call` attribute in *modelbased*
  objects.

- More informative warnings and error messages were added to
  [`estimate_contrasts()`](https://easystats.github.io/modelbased/reference/estimate_contrasts.md)
  when computing effect sizes.

- Improved plotting methods for the *tinyplot* package. The related
  vignette was also updated.

## modelbased 0.13.0

CRAN release: 2025-08-30

### Changes

- Methods for the *tinyplot* package were added.

- [`estimate_slopes()`](https://easystats.github.io/modelbased/reference/estimate_slopes.md)
  now also gets the `estimate` argument, to specify how to estimate /
  marginalize over non-focal terms.

- Improvements to
  [`estimate_contrasts()`](https://easystats.github.io/modelbased/reference/estimate_contrasts.md):

  - `comparison = "inequality"` now also works when contrasting slopes
    for a numeric predictor.

  - New option `comparison = "inequality_ratio"` for
    `estimate_contrast()`, which computes the ratio of marginal effects
    inequality measures across groups.

  - For contrasts, the message about the units of contrasts (“in
    %-points”) has been removed, because this was slightly misleading.
    The units were in %-points if multiplied by 100, but this
    multiplication was not done in the output.

  - [`estimate_contrasts()`](https://easystats.github.io/modelbased/reference/estimate_contrasts.md)
    for slopes now allows additional grouping of contrasts using the
    `by` argument together with the `comparison` argument by specifying
    the grouping variable in the formula,
    e.g. `contrast = c("x", "group")` and `~ pairwise | group`.

- [`estimate_expectation()`](https://easystats.github.io/modelbased/reference/estimate_expectation.md)
  and
  [`estimate_relation()`](https://easystats.github.io/modelbased/reference/estimate_expectation.md)
  now support objects of class `htest`.

- [`estimate_grouplevel()`](https://easystats.github.io/modelbased/reference/estimate_grouplevel.md)
  now supports models from package *coxme*.

- New function
  [`residualize_over_grid()`](https://easystats.github.io/modelbased/reference/residualize_over_grid.md),
  which residualizes a model over a grid of predictors. This is useful
  to visualize the residuals of a model over a grid of predictors.

- [`visualisation_recipe()`](https://easystats.github.io/datawizard/reference/visualisation_recipe.html)
  and [`plot()`](https://rdrr.io/r/graphics/plot.default.html) get a
  `show_residuals` argument, to show the residuals of the model, related
  to the data grid, in the plot.

- Documentation of the
  [`display()`](https://easystats.github.io/insight/reference/display.html)
  method for *modelbased* objects has been added.

- Improved documentation and improved informative messages.

- Message about unreliable standard errors (for certain models, when
  predicting random effects) was removed for now, as it is uncertain
  whether the standard errors were unreliable.

- Modified code base to address changes in the *marginaleffects* package
  from version 0.29.0 onwards.

### Bug fixes

- Fixed issue with `by` in
  [`estimate_contrasts()`](https://easystats.github.io/modelbased/reference/estimate_contrasts.md)
  when `comparison` was `"inequality"`.

- Some comparison options, like `"helmert"` or `"poly"`, could not be
  specified as string-value, only as formula. This has been fixed, so
  they can now be specified as string-value, too.

- In-formula transformations of predictors in `by` could not be handled
  when `by` was not specified. This has been fixed, and automatic
  detection of `by` variables now also works with in-formula
  transformations.

## modelbased 0.12.0

CRAN release: 2025-07-10

### Changes

- The `comparison` argument gets two new option, `"inequality"` and
  `"inequality_pairwise"`, to compute the marginal effects inequality
  measure, which summarizes the the overall effect of categorical
  predictors or the comprehensive effect of a predictor across all
  outcome categories of a nominal or ordinal dependent variable.

- Added docs to show how to use *modelbased* with finite mixture models
  from package *brms*.

- Improved support for finite mixture models (currently only the
  [`mixture()`](https://paulbuerkner.com/brms/reference/mixture.html)
  family for model from package *brms* are supported).

- Improved printing for joint-tests with `backend = "emmeans"`.

- Improved handling when p-adjustment methods that are only available in
  the *emmeans* package were used for the *marginaleffects* backend.

- The column header for the predicted values in
  [`estimate_means()`](https://easystats.github.io/modelbased/reference/estimate_means.md)
  for multinomial models from packages *nnet* and *brglm2* has been
  changed to `Probability`, to better reflect the scale of the
  predictions.

- New vignettes (Case Studies) about using *modelbased* with finite
  mixture models and interrupted time series analysis.

- The `p_adjust` argument gets a new option, `"sup-t"`, to calculate
  simultaneous confidence intervals.

- Added a
  [`display()`](https://easystats.github.io/insight/reference/display.html)
  method for *modelbased* objects.

### Bug fixes

- Fixed printing and plotting for models from packages *nnet* and
  *brglm2*.

- Fixed issues with object of class `aov`.

- Fixed issue with the
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) method for
  [`estimate_slopes()`](https://easystats.github.io/modelbased/reference/estimate_slopes.md)
  for Bayesian models.

## modelbased 0.11.2

CRAN release: 2025-05-30

### Changes

- [`estimate_contrasts()`](https://easystats.github.io/modelbased/reference/estimate_contrasts.md)
  for results from
  [`estimate_relation()`](https://easystats.github.io/modelbased/reference/estimate_expectation.md)
  and alike is now more efficient for larger number of contrasts.

- Updated information of
  [`citation()`](https://rdrr.io/r/utils/citation.html). If you want to
  cite the `modelbased` package, please use the JOSS publication as
  reference (<https://joss.theoj.org/papers/10.21105/joss.07969>).

### Bug fixes

- Fixed issue with formatted labels in
  [`estimate_contrasts()`](https://easystats.github.io/modelbased/reference/estimate_contrasts.md)
  for results from
  [`estimate_relation()`](https://easystats.github.io/modelbased/reference/estimate_expectation.md).

## modelbased 0.11.1

### Changes

- The `comparison` argument can now also be a custom function, or a
  matrix (e.g., to define contrasts).

- The `comparison` argument can now also be `"joint"`, to jointly test
  hypotheses (i.e. conducting a joint test) in factorial designs.

- New vignette about user-defined contrasts and joint tests in
  [`estimate_contrasts()`](https://easystats.github.io/modelbased/reference/estimate_contrasts.md).

## modelbased 0.11.0

CRAN release: 2025-05-02

### New functions

- Added
  [`pool_slopes()`](https://easystats.github.io/modelbased/reference/pool_predictions.md),
  to pool results from
  [`estimate_slopes()`](https://easystats.github.io/modelbased/reference/estimate_slopes.md)
  applied to imputed data.

### Breaking Changes

- [`reshape_grouplevel()`](https://easystats.github.io/modelbased/reference/estimate_grouplevel.md)
  now takes the correct number of specified random effects groups into
  account when reshaping results.

### Changes

- In general, it is now possible to make estimate means, contrasts and
  slopes for distributional parameters for models from package *brms*
  using the `predict` argument.

- [`estimate_grouplevel()`](https://easystats.github.io/modelbased/reference/estimate_grouplevel.md)
  gets arguments `test`, `dispersion` and `diagnostic`, that are
  internally passed to
  [`parameters::model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.html),
  but with different defaults.

- [`estimate_prediction()`](https://easystats.github.io/modelbased/reference/estimate_expectation.md)
  and
  [`estimate_relation()`](https://easystats.github.io/modelbased/reference/estimate_expectation.md)
  now support Wiener-models (Drift Diffusion Models) from package
  *brms*.

- [`estimate_prediction()`](https://easystats.github.io/modelbased/reference/estimate_expectation.md),
  [`estimate_relation()`](https://easystats.github.io/modelbased/reference/estimate_expectation.md)
  and similar functions now include the `Row` column for models with
  ordinal or categorical response variables when the `data` argument was
  provided.

- [`estimate_slopes()`](https://easystats.github.io/modelbased/reference/estimate_slopes.md)
  can now also calculate average marginal effects of a predictor, just
  for the trend of that predictor within a certain range of values.

- [`estimate_slopes()`](https://easystats.github.io/modelbased/reference/estimate_slopes.md)
  gets a `predict` argument, to either select the scale of the estimates
  slopes, or to estimate slopes (marginal effects) for distributional
  parameters of *brms* models.

- [`estimate_contrasts()`](https://easystats.github.io/modelbased/reference/estimate_contrasts.md)
  gives an informative error message when arguments `by` and `contrast`
  have identical variables (which does not work).

- Column names of predicted values for `backend = "emmeans"` has changed
  for models like logistic regression, or beta regression. Formerly,
  name was `Mean`, now it is `Probability` or `Proportion`, depending on
  the model.

- Exposed `iterations` argument in
  [`estimate_prediction()`](https://easystats.github.io/modelbased/reference/estimate_expectation.md)
  and
  [`estimate_relation()`](https://easystats.github.io/modelbased/reference/estimate_expectation.md).

- Option `estimate = "average` no longer prints information on averaged
  predictors in the footer, because strictly, the *predictions* are
  averaged over, and not the non-focal variables.

- Better handling for models with offsets in
  [`estimate_means()`](https://easystats.github.io/modelbased/reference/estimate_means.md)
  and
  [`estimate_contrasts()`](https://easystats.github.io/modelbased/reference/estimate_contrasts.md).
  Informative messages are given when models include offset terms, and
  it is possible to fix the offset value using the `offset` argument.
  The `offset` argument is also available for
  [`estimate_relation()`](https://easystats.github.io/modelbased/reference/estimate_expectation.md),
  [`estimate_prediction()`](https://easystats.github.io/modelbased/reference/estimate_expectation.md)
  and similar.

- For consistency,
  [`estimate_slopes()`](https://easystats.github.io/modelbased/reference/estimate_slopes.md)
  now also uses the residual degrees of freedom by default (like
  [`estimate_means()`](https://easystats.github.io/modelbased/reference/estimate_means.md))
  when calculating confidence intervals and p-values.

- Minor improvements to the documentation.

### Bug fixes

- Fixed issues in
  [`estimate_grouplevel()`](https://easystats.github.io/modelbased/reference/estimate_grouplevel.md)
  for models from package *rstanarm*.

- Fixed issues in calculating correct confidence intervals (and possibly
  p-values) for pooling functions
  [`pool_parameters()`](https://easystats.github.io/parameters/reference/pool_parameters.html)
  and
  [`pool_predictions()`](https://easystats.github.io/modelbased/reference/pool_predictions.md).

- Fixed issue in
  [`estimate_means()`](https://easystats.github.io/modelbased/reference/estimate_means.md)
  for multivariate response models from package *brms*.

- Fixed issue with wrong y-axis label for plots from
  [`estimate_slopes()`](https://easystats.github.io/modelbased/reference/estimate_slopes.md).

- Fixed issue with weights in
  [`estimate_relation()`](https://easystats.github.io/modelbased/reference/estimate_expectation.md).

- Fixed issue in printed output for the statistic column, which should
  be `z` for the `marginaleffects` backend, when argument `df = Inf`.

## modelbased 0.10.0

CRAN release: 2025-03-10

### Breaking Changes

- The deprecated function `visualisation_matrix()` has been removed. Use
  [`insight::get_datagrid()`](https://easystats.github.io/insight/reference/get_datagrid.html)
  instead.

- The `"average"` option for argument `estimate` was renamed into
  `"typical"`. The former `"average"` option is still available, but now
  returns marginal means fully averaged across the sample.

### Changes

- The `transform` argument now also works for
  [`estimate_slopes()`](https://easystats.github.io/modelbased/reference/estimate_slopes.md)
  and for
  [`estimate_contrasts()`](https://easystats.github.io/modelbased/reference/estimate_contrasts.md)
  with numeric focal terms.

- [`estimate_contrasts()`](https://easystats.github.io/modelbased/reference/estimate_contrasts.md)
  no longer calls
  [`estimate_slopes()`](https://easystats.github.io/modelbased/reference/estimate_slopes.md)
  for numeric focal terms when these are integers with only few values.
  In this case, it is assumed that contrasts of values (“levels”) are
  desired, because integer variables with only two to five unique values
  are factor-alike.

- `estimate_contrasts`: now supports optional standardized effect sizes,
  one of “none” (default), “emmeans”, or “bootES”
  ([\#227](https://github.com/easystats/modelbased/issues/227),
  [@rempsyc](https://github.com/rempsyc)).

- The [`predict()`](https://rdrr.io/r/stats/predict.html) argument for
  [`estimate_means()`](https://easystats.github.io/modelbased/reference/estimate_means.md)
  gets an `"inverse_link"` option, to calculate predictions on the
  link-scale and back-transform them to the response scale after
  aggregation by groups.

- [`estimate_means()`](https://easystats.github.io/modelbased/reference/estimate_means.md),
  [`estimate_slopes()`](https://easystats.github.io/modelbased/reference/estimate_slopes.md)
  and
  [`estimate_contrasts()`](https://easystats.github.io/modelbased/reference/estimate_contrasts.md)
  get a `keep_iterations` argument, to keep all posterior draws from
  Bayesian models added as columns to the output.

- New functions
  [`pool_predictions()`](https://easystats.github.io/modelbased/reference/pool_predictions.md)
  and
  [`pool_contrasts()`](https://easystats.github.io/modelbased/reference/pool_contrasts.md),
  to deal with *modelbased* objects that were applied to imputed data
  sets. E.g., functions like
  [`estimate_means()`](https://easystats.github.io/modelbased/reference/estimate_means.md)
  can be run on several data sets where missing values were imputed, and
  the multiple results from
  [`estimate_means()`](https://easystats.github.io/modelbased/reference/estimate_means.md)
  can be pooled using
  [`pool_predictions()`](https://easystats.github.io/modelbased/reference/pool_predictions.md).

- The [`print()`](https://rdrr.io/r/base/print.html) method is now
  explicitly documented and gets some new options to customize the
  output for tables.

- [`estimate_grouplevel()`](https://easystats.github.io/modelbased/reference/estimate_grouplevel.md)
  gets a new option, `type = "total"`, to return the sum of fixed and
  random effects (similar to what
  [`coef()`](https://rdrr.io/r/stats/coef.html) returns for (Bayesian)
  mixed models).

- New option `"esarey"` for the `p_adjust` argument. The `"esarey"`
  option is specifically for the case of Johnson-Neyman intervals,
  i.e. when calling
  [`estimate_slopes()`](https://easystats.github.io/modelbased/reference/estimate_slopes.md)
  with two numeric predictors in an interaction term.

- [`print_html()`](https://easystats.github.io/insight/reference/display.html)
  and
  [`print_md()`](https://easystats.github.io/insight/reference/display.html)
  pass `...` to format-methods (e.g. to
  [`insight::format_table()`](https://easystats.github.io/insight/reference/format_table.html)),
  to tweak the output.

- The `show_data` argument in
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) is
  automatically set to `FALSE` when the models has a transformed
  response variable, but predictions were not back-transformed using the
  `transform` argument.

- The [`plot()`](https://rdrr.io/r/graphics/plot.default.html) method
  gets a `numeric_as_discrete` argument, to decide whether numeric
  predictors should be treated as factor or continuous, based on the of
  unique values in numeric predictors.

- Plots now use a probability scale for the y-axis for models whose
  response scale are probabilities (e.g., logistic regression).

- Improved printing for
  [`estimate_contrasts()`](https://easystats.github.io/modelbased/reference/estimate_contrasts.md)
  when one of the focal predictors was numeric.

### Bug fixes

- Fixed issue in the [`summary()`](https://rdrr.io/r/base/summary.html)
  method for
  [`estimate_slopes()`](https://easystats.github.io/modelbased/reference/estimate_slopes.md).

- Fixed issues with multivariate response models.

- Fixed issues with plotting ordinal or multinomial models.

- Fixed issues with `ci` argument, which was ignored for Bayesian
  models.

- Fixed issues with contrasting slopes when `backend` was `"emmeans"`.

- Fixed issues in
  [`estimate_contrasts()`](https://easystats.github.io/modelbased/reference/estimate_contrasts.md)
  when filtering numeric values in `by`.

- Fixed issues in
  [`estimate_grouplevel()`](https://easystats.github.io/modelbased/reference/estimate_grouplevel.md).

- Fixed issue in
  [`estimate_slopes()`](https://easystats.github.io/modelbased/reference/estimate_slopes.md)
  for models from package *lme4*.

## modelbased 0.9.0

CRAN release: 2025-02-05

### Breaking Changes

- The default package used for
  [`estimate_means()`](https://easystats.github.io/modelbased/reference/estimate_means.md),
  [`estimate_slopes()`](https://easystats.github.io/modelbased/reference/estimate_slopes.md)
  and
  [`estimate_contrasts()`](https://easystats.github.io/modelbased/reference/estimate_contrasts.md)
  is now *marginaleffects*. You can set your preferred package as
  backend using either the `backend` argument, or in general by setting
  `options(modelbased_backend = "marginaleffects")` or
  `options(modelbased_backend = "emmeans")`.

- Deprecated argument and function names have been removed.

- Argument `fixed` has been removed, as you can fix predictor at certain
  values using the `by` argument.

- Argument `transform` is no longer used to determine the scale of the
  predictions. Please use `predict` instead.

- Argument `transform` is now used to (back-) transform predictions and
  confidence intervals.

- Argument `method` in
  [`estimate_contrasts()`](https://easystats.github.io/modelbased/reference/estimate_contrasts.md)
  was renamed into `comparison`.

- All `model_*()` alias names have been removed. Use the related
  `get_*()` functions instead.

- The `show_data` argument in
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) defaults to
  `FALSE`.

### Major Changes

- The `"marginaleffects"` backend is now fully implemented and no longer
  work-in-progress. You can set your preferred package as backend using
  either the `backend` argument, or in general by setting
  `options(modelbased_backend = "marginaleffects")` or
  `options(modelbased_backend = "emmeans")`.

- All `estimate_*()` functions get a `predict` argument, which can be
  used to modulate the type of transformation applied to the predictions
  (i.e. whether predictions should be on the response scale, link scale,
  etc.). It can also be used to predict auxiliary (distributional)
  parameters.

- [`estimate_means()`](https://easystats.github.io/modelbased/reference/estimate_means.md)
  and
  [`estimate_contrasts()`](https://easystats.github.io/modelbased/reference/estimate_contrasts.md)
  get a `estimate` argument, to specify how to estimate over non-focal
  terms. This results in slightly different predicted values, each
  approach answering a different question.

- [`estimate_contrasts()`](https://easystats.github.io/modelbased/reference/estimate_contrasts.md)
  gains a `backend` argument. This defaults to `"marginaleffects"`, but
  can be set to `"emmeans"` to use features of that package to estimate
  contrasts and pairwise comparisons.

- [`estimate_expectation()`](https://easystats.github.io/modelbased/reference/estimate_expectation.md)
  and related functions also get a `by` argument, as alternative to
  create a datagrid for the `data` argument.

- Many functions get a `verbose` argument, to silence warnings and
  messages.

### Bug fixes

- [`estimate_contrasts()`](https://easystats.github.io/modelbased/reference/estimate_contrasts.md)
  did not calculate contrasts for all levels when the predictor of
  interest was converted to a factor inside the model formula.

- Fixed issue in
  [`estimate_contrasts()`](https://easystats.github.io/modelbased/reference/estimate_contrasts.md)
  when `comparsison` (formerly: `method`) was not `"pairwise"`.

## modelbased 0.8.9

CRAN release: 2024-10-26

- Fixed issues related to updates of other *easystats* packages.

## modelbased 0.8.6

CRAN release: 2023-01-13

### Breaking Changes

- The minimum needed R version has been bumped to `3.6`.

## modelbased 0.8.5

CRAN release: 2022-08-18

- Fixed issues with printing-methods.

- Maintenance release to fix failing tests in CRAN checks.

## modelbased 0.8.1

CRAN release: 2022-05-30

- Maintenance release to fix failing tests in CRAN checks.

## modelbased 0.8.0

CRAN release: 2022-03-31

- `visualisation_matrix()` has now become an alias (alternative name)
  for the
  [`get_datagrid()`](https://easystats.github.io/insight/reference/get_datagrid.html)
  function, which is implemented in the
  [`insight`](https://easystats.github.io/insight/) package.

## modelbased 0.7.2

CRAN release: 2022-02-27

- Patch release. This update fixes failing tests after updating the
  *insight* package.

## modelbased 0.7.1

CRAN release: 2022-01-13

- API changes: `levels` in `estimate_contrasts` has been replaced by
  `contrast`. `levels` and `modulate` are in general aggregated under
  `at`.

- [`estimate_prediction()`](https://easystats.github.io/modelbased/reference/estimate_expectation.md)
  deprecated in favour of `estimate_response()`.

- [`estimate_expectation()`](https://easystats.github.io/modelbased/reference/estimate_expectation.md)
  now has `data=NULL` by default.

## modelbased 0.7.0

CRAN release: 2021-06-06

- General overhaul of the package.

- Entire refactoring of `visualisation_matrix()`.

- Option of standardizing/unstandardizing predictions, contrasts and
  means is now available via
  [`standardize()`](https://easystats.github.io/datawizard/reference/standardize.html)
  instead of via options.

- Introduction of `model_emmeans()` as a wrapper to easily create
  `emmeans` objects.

- [`estimate_smooth()`](https://easystats.github.io/modelbased/reference/describe_nonlinear.md)
  transformed into
  [`describe_nonlinear()`](https://easystats.github.io/modelbased/reference/describe_nonlinear.md)
  and made more explicit.

## modelbased 0.6.0

CRAN release: 2021-04-12

- [`estimate_link()`](https://easystats.github.io/modelbased/reference/estimate_expectation.md)
  now does *not* transform predictions on the response scale for GLMs.
  To keep the previous behaviour, use the new
  [`estimate_relation()`](https://easystats.github.io/modelbased/reference/estimate_expectation.md)
  instead. This follows a change in how predictions are made internally
  (which now relies on
  [`get_predicted()`](https://easystats.github.io/insight/reference/get_predicted.html),
  so more details can be found there).

## modelbased 0.5.1

CRAN release: 2021-01-27

- Minor improvements.

## modelbased 0.3.0

CRAN release: 2020-09-26

### Breaking changes

- `Predicted` is now the name of the predicted column for Bayesian
  models (similarly to Frequentist ones), instead of the centrality
  index (e.g., `Median`).

### New supported models

- Models from package *glmmTMB* are now supported.

### Bug fixes

- `estimate_slope()` now gives an informative error when no numeric
  predictor is present.

## modelbased 0.2.0

- Partial support of formulas.

- Refactor the emmeans wrapping.

## modelbased 0.1.3

- Fix CRAN check issues.

## modelbased 0.1.2

CRAN release: 2020-03-12

- Minor code changes to address changes from the forthcoming
  `parameters` package update.

## modelbased 0.1.1

CRAN release: 2020-01-26

- Fix CRAN check issues.

## modelbased 0.1.0

CRAN release: 2020-01-12

- Added a `NEWS.md` file to track changes to the package
