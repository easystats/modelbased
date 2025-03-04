# modelbased (devel)

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

* The `predict()` argument for `estimate_means()` gets an `"inverse_link"` option,
  to calculate predictions on the link-scale and back-transform them to the
  response scale after aggregation by groups.

* New functions `pool_predictions()` and `pool_contrasts()`, to deal with
  *modelbased* objects that were applied to imputed data sets. E.g., functions
  like `estimate_means()` can be run on several data sets where missing values
  were imputed, and the multiple results from `estimate_means()` can be pooled
  using `pool_predictions()`.

* The `print()` method is now explicitly documented and gets some new options
  to customize the output for tables.

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
