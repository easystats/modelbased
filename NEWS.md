# modelbased 0.9.0

## Breaking Changes

- Deprecated argument and function names have been removed.

- Argument `fixed` has been removed, as you can fix predictor at certain values
  using the `by` argument.

## Major Changes

- The `"marginaleffects"` backend for `estimate_means()` is now fully implemented
  and no longer work-in-progress.

- `estimate_contrasts()` gains a `backend` argument. This defaults to `"emmeans"`,
  but can be set to `"marginaleffects"` to use features of that package to estimate
  contrasts and pairwise comparisons.

- `estimate_expectation()` and related functions also get a `by` argument, as
  alternative to create a datagrid for the `data` argument.

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
