# modelbased (devel)

## Changes

* Methods for the *tinyplot* package were added.

* `estimate_slopes()` now also gets the `estimate` argument, to specify how
  to estimate over non-focal terms.

* `comparison = "inequality"` now also works when contrasting slopes
  for a numeric predictor.

* New option `comparison = "inequality_ratio"` for `estimate_contrast()`, which
  computes the ratio of marginal effects inequality measures across groups.

* `estimate_expectation()` and `estimate_relation()` now support objects of
  class `htest`.

* Documentation of the `display()` method for *modelbased* objects has been
  added.

* For contrasts, the message about the units of contrasts ("in %-points") has
  been removed, because this was slightly misleading. The units were in %-points
  if multiplied by 100, but this multiplication was not done in the output.

* Improved documentation and improved informative messages.

## Bug fixes

* Fixed issue with `by` in `estimate_contrasts()` when `comparison` was
  `"inequality"`.

# modelbased 0.12.0

## Changes

* The `comparison` argument gets two new option, `"inequality"` and
  `"inequality_pairwise"`, to compute the marginal effects inequality measure,
  which summarizes the the overall effect of categorical predictors or the
  comprehensive effect of a predictor across all outcome categories of a nominal
  or ordinal dependent variable.

* Added docs to show how to use *modelbased* with finite mixture models from
  package *brms*.

* Improved support for finite mixture models (currently only the `mixture()`
  family for model from package *brms* are supported).

* Improved printing for joint-tests with `backend = "emmeans"`.

* Improved handling when p-adjustment methods that are only available in the
  *emmeans* package were used for the *marginaleffects* backend.

* The column header for the predicted values in `estimate_means()` for
  multinomial models from packages *nnet* and *brglm2* has been changed to
  `Probability`, to better reflect the scale of the predictions.

* New vignettes (Case Studies) about using *modelbased* with finite mixture models
  and interrupted time series analysis.

* The `p_adjust` argument gets a new option, `"sup-t"`, to calculate
  simultaneous confidence intervals.

* Added a `display()` method for *modelbased* objects.

## Bug fixes

* Fixed printing and plotting for models from packages *nnet* and *brglm2*.

* Fixed issues with object of class `aov`.

* Fixed issue with the `plot()` method for `estimate_slopes()` for Bayesian
  models.

# modelbased 0.11.2

## Changes

* `estimate_contrasts()` for results from `estimate_relation()` and alike is
  now more efficient for larger number of contrasts.

* Updated information of `citation()`. If you want to cite the `modelbased`
  package, please use the JOSS publication as reference
  (https://joss.theoj.org/papers/10.21105/joss.07969).

## Bug fixes

* Fixed issue with formatted labels in `estimate_contrasts()` for results from
  `estimate_relation()`.

# modelbased 0.11.1

## Changes

* The `comparison` argument can now also be a custom function, or a matrix
  (e.g., to define contrasts).

* The `comparison` argument can now also be `"joint"`, to jointly test
  hypotheses (i.e. conducting a joint test) in factorial designs.

* New vignette about user-defined contrasts and joint tests in
  `estimate_contrasts()`.

# modelbased 0.11.0

## New functions

* Added `pool_slopes()`, to pool results from `estimate_slopes()` applied to
  imputed data.

## Breaking Changes

* `reshape_grouplevel()` now takes the correct number of specified random effects
  groups into account when reshaping results.

## Changes

* In general, it is now possible to make estimate means, contrasts and slopes
  for distributional parameters for models from package *brms* using the
  `predict` argument.

* `estimate_grouplevel()` gets arguments `test`, `dispersion` and `diagnostic`,
  that are internally passed to `parameters::model_parameters()`, but with
  different defaults.

* `estimate_prediction()` and `estimate_relation()` now support Wiener-models
  (Drift Diffusion Models) from package *brms*.

* `estimate_prediction()`, `estimate_relation()` and similar functions now
  include the `Row` column for models with ordinal or categorical response
  variables when the `data` argument was provided.

* `estimate_slopes()` can now also calculate average marginal effects of a
  predictor, just for the trend of that predictor within a certain range of
  values.

* `estimate_slopes()` gets a `predict` argument, to either select the scale
  of the estimates slopes, or to estimate slopes (marginal effects) for
  distributional parameters of *brms* models.

* `estimate_contrasts()` gives an informative error message when arguments
  `by` and `contrast` have identical variables (which does not work).

* Column names of predicted values for `backend = "emmeans"` has changed for
  models like logistic regression, or beta regression. Formerly, name was
  `Mean`, now it is `Probability` or `Proportion`, depending on the model.

* Exposed `iterations` argument in `estimate_prediction()` and `estimate_relation()`.

* Option `estimate = "average` no longer prints information on averaged predictors
  in the footer, because strictly, the *predictions* are averaged over, and not
  the non-focal variables.

* Better handling for models with offsets in `estimate_means()` and
  `estimate_contrasts()`. Informative messages are given when models include
  offset terms, and it is possible to fix the offset value using the `offset`
  argument. The `offset` argument is also available for `estimate_relation()`,
  `estimate_prediction()` and similar.

* For consistency, `estimate_slopes()` now also uses the residual degrees of
  freedom by default (like `estimate_means()`) when calculating confidence
  intervals and p-values.

* Minor improvements to the documentation.

## Bug fixes

* Fixed issues in `estimate_grouplevel()` for models from package *rstanarm*.

* Fixed issues in calculating correct confidence intervals (and possibly p-values)
  for pooling functions `pool_parameters()` and `pool_predictions()`.

* Fixed issue in `estimate_means()` for multivariate response models from
  package *brms*.

* Fixed issue with wrong y-axis label for plots from `estimate_slopes()`.

* Fixed issue with weights in `estimate_relation()`.

* Fixed issue in printed output for the statistic column, which should be `z`
  for the `marginaleffects` backend, when argument `df = Inf`.

# modelbased 0.10.0

## Breaking Changes

* The deprecated function `visualisation_matrix()` has been removed. Use
  `insight::get_datagrid()` instead.

* The `"average"` option for argument `estimate` was renamed into `"typical"`.
  The former `"average"` option is still available, but now returns marginal
  means fully averaged across the sample.

## Changes

* The `transform` argument now also works for `estimate_slopes()` and for
  `estimate_contrasts()` with numeric focal terms.

* `estimate_contrasts()` no longer calls `estimate_slopes()` for numeric focal
  terms when these are integers with only few values. In this case, it is assumed
  that contrasts of values ("levels") are desired, because integer variables with
  only two to five unique values are factor-alike.

* `estimate_contrasts`: now supports optional standardized effect sizes, one of
  "none" (default), "emmeans", or "bootES" (#227, @rempsyc).

* The `predict()` argument for `estimate_means()` gets an `"inverse_link"` option,
  to calculate predictions on the link-scale and back-transform them to the
  response scale after aggregation by groups.

* `estimate_means()`, `estimate_slopes()` and `estimate_contrasts()` get a
  `keep_iterations` argument, to keep all posterior draws from Bayesian models
  added as columns to the output.

* New functions `pool_predictions()` and `pool_contrasts()`, to deal with
  *modelbased* objects that were applied to imputed data sets. E.g., functions
  like `estimate_means()` can be run on several data sets where missing values
  were imputed, and the multiple results from `estimate_means()` can be pooled
  using `pool_predictions()`.

* The `print()` method is now explicitly documented and gets some new options
  to customize the output for tables.

* `estimate_grouplevel()` gets a new option, `type = "total"`, to return the
  sum of fixed and random effects (similar to what `coef()` returns for (Bayesian)
  mixed models).

* New option `"esarey"` for the `p_adjust` argument. The `"esarey"` option is
  specifically for the case of Johnson-Neyman intervals, i.e. when calling
  `estimate_slopes()` with two numeric predictors in an interaction term.

* `print_html()` and `print_md()` pass `...` to format-methods (e.g. to
  `insight::format_table()`), to tweak the output.

* The `show_data` argument in `plot()` is automatically set to `FALSE` when
  the models has a transformed response variable, but predictions were not
  back-transformed using the `transform` argument.

* The `plot()` method gets a `numeric_as_discrete` argument, to decide whether
  numeric predictors should be treated as factor or continuous, based on the
  of unique values in numeric predictors.

* Plots now use a probability scale for the y-axis for models whose response
  scale are probabilities (e.g., logistic regression).

* Improved printing for `estimate_contrasts()` when one of the focal predictors
  was numeric.

## Bug fixes

* Fixed issue in the `summary()` method for `estimate_slopes()`.

* Fixed issues with multivariate response models.

* Fixed issues with plotting ordinal or multinomial models.

* Fixed issues with `ci` argument, which was ignored for Bayesian models.

* Fixed issues with contrasting slopes when `backend` was `"emmeans"`.

* Fixed issues in `estimate_contrasts()` when filtering numeric values in `by`.

* Fixed issues in `estimate_grouplevel()`.

* Fixed issue in `estimate_slopes()` for models from package *lme4*.

# modelbased 0.9.0

## Breaking Changes

- The default package used for `estimate_means()`, `estimate_slopes()` and
  `estimate_contrasts()` is now *marginaleffects*. You can set your preferred
  package as backend using either the `backend` argument, or in general by setting
  `options(modelbased_backend = "marginaleffects")` or
  `options(modelbased_backend = "emmeans")`.

- Deprecated argument and function names have been removed.

- Argument `fixed` has been removed, as you can fix predictor at certain values
  using the `by` argument.

- Argument `transform` is no longer used to determine the scale of the predictions.
  Please use `predict` instead.

- Argument `transform` is now used to (back-) transform predictions and confidence
  intervals.

- Argument `method` in `estimate_contrasts()` was renamed into `comparison`.

- All `model_*()` alias names have been removed. Use the related `get_*()`
  functions instead.

- The `show_data` argument in `plot()` defaults to `FALSE`.

## Major Changes

- The `"marginaleffects"` backend is now fully implemented and no longer
  work-in-progress. You can set your preferred package as backend using
  either the `backend` argument, or in general by setting
  `options(modelbased_backend = "marginaleffects")` or
  `options(modelbased_backend = "emmeans")`.

- All `estimate_*()` functions get a `predict` argument, which can be used
  to modulate the type of transformation applied to the predictions (i.e. whether
  predictions should be on the response scale, link scale, etc.). It can also
  be used to predict auxiliary (distributional) parameters.

- `estimate_means()` and `estimate_contrasts()` get a `estimate` argument,
  to specify how to estimate over non-focal terms. This results in slightly
  different predicted values, each approach answering a different question.

- `estimate_contrasts()` gains a `backend` argument. This defaults to
  `"marginaleffects"`, but can be set to `"emmeans"` to use features of that
  package to estimate contrasts and pairwise comparisons.

- `estimate_expectation()` and related functions also get a `by` argument, as
  alternative to create a datagrid for the `data` argument.

- Many functions get a `verbose` argument, to silence warnings and messages.

## Bug fixes

* `estimate_contrasts()` did not calculate contrasts for all levels when the
  predictor of interest was converted to a factor inside the model formula.

* Fixed issue in `estimate_contrasts()` when `comparsison` (formerly: `method`)
  was not `"pairwise"`.

# modelbased 0.8.9

- Fixed issues related to updates of other *easystats* packages.

# modelbased 0.8.6

## Breaking Changes

- The minimum needed R version has been bumped to `3.6`.

# modelbased 0.8.5

- Fixed issues with printing-methods.

- Maintenance release to fix failing tests in CRAN checks.

# modelbased 0.8.1

- Maintenance release to fix failing tests in CRAN checks.

# modelbased 0.8.0

- `visualisation_matrix()` has now become an alias (alternative name) for the [`get_datagrid()`](https://easystats.github.io/insight/reference/get_datagrid.html) function, which is implemented in the [`insight`](https://easystats.github.io/insight/) package.


# modelbased 0.7.2

- Patch release. This update fixes failing tests after updating the *insight*
  package.

# modelbased 0.7.1

- API changes: `levels` in `estimate_contrasts` has been replaced by `contrast`.
  `levels` and `modulate` are in general aggregated under `at`.

- `estimate_prediction()` deprecated in favour of `estimate_response()`.

- `estimate_expectation()` now has `data=NULL` by default.

# modelbased 0.7.0

- General overhaul of the package.

- Entire refactoring of `visualisation_matrix()`.

- Option of standardizing/unstandardizing predictions, contrasts and means is
  now available via `standardize()` instead of via options.

- Introduction of `model_emmeans()` as a wrapper to easily create `emmeans`
  objects.

- `estimate_smooth()` transformed into `describe_nonlinear()` and made more
  explicit.

# modelbased 0.6.0

- `estimate_link()` now does *not* transform predictions on the response scale
  for GLMs. To keep the previous behaviour, use the new `estimate_relation()`
  instead. This follows a change in how predictions are made internally (which
  now relies on
  [`get_predicted()`](https://easystats.github.io/insight/reference/get_predicted.html),
  so more details can be found there).

# modelbased 0.5.1

- Minor improvements.

# modelbased 0.3.0

## Breaking changes

- `Predicted` is now the name of the predicted column for Bayesian models
  (similarly to Frequentist ones), instead of the centrality index (e.g.,
  `Median`).

## New supported models

- Models from package *glmmTMB* are now supported.

## Bug fixes

- `estimate_slope()` now gives an informative error when no numeric predictor is
  present.

# modelbased 0.2.0

- Partial support of formulas.

- Refactor the emmeans wrapping.

# modelbased 0.1.3

- Fix CRAN check issues.

# modelbased 0.1.2

- Minor code changes to address changes from the forthcoming `parameters`
  package update.

# modelbased 0.1.1

- Fix CRAN check issues.

# modelbased 0.1.0

- Added a `NEWS.md` file to track changes to the package
