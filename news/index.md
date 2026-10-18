# Changelog

## modelbased 0.18.0

### Breaking Changes

- For ordinal models (for example from
  [`MASS::polr()`](https://rdrr.io/pkg/MASS/man/polr.html),
  [`ordinal::clm()`](https://rdrr.io/pkg/ordinal/man/clm.html) or
  [`ordinal::clmm()`](https://rdrr.io/pkg/ordinal/man/clmm.html)),
  [`estimate_contrasts()`](https://easystats.github.io/modelbased/reference/estimate_contrasts.md)
  and
  [`estimate_slopes()`](https://easystats.github.io/modelbased/reference/estimate_slopes.md)
  with `backend = "emmeans"` now return probabilities by default and for
  `predict = "response"`, as
  [`estimate_means()`](https://easystats.github.io/modelbased/reference/estimate_means.md)
  already does. Contrasts compare the focal levels within each response
  category, and slopes are the slopes of the probability of each
  response category. Before, both returned values on the latent scale,
  and the table footer of
  [`estimate_contrasts()`](https://easystats.github.io/modelbased/reference/estimate_contrasts.md)
  said they were on the response scale. The ordinal modes of *emmeans*
  (`"prob"`, `"cum.prob"`, `"exc.prob"`, `"linear.predictor"`,
  `"latent"` and `"mean.class"`) can be passed to `predict`. Use
  `predict = "latent"` to get the former results.

### Changes

- Post-processing in
  [`estimate_contrasts()`](https://easystats.github.io/modelbased/reference/estimate_contrasts.md)
  including group-variable (e.g.,
  `post_process = ~ pairwise | group_var`) now no longer prints levels
  from `group_var` in both the group and comparison columns
  (i.e. redundant labels have been removed from the output).

- [`estimate_contrasts()`](https://easystats.github.io/modelbased/reference/estimate_contrasts.md)
  with custom comparisons (e.g., when
  `comparison = "(b1 - b12) = (b4 - b18)"`) can yield incorrect results
  when `estimate = "average"` and predictions are filtered (e.g. when
  `contrast = c("x=c(2,4)")`). This does not happen in general, but may
  happen. Therefore, now an informative warning is printed in such
  edge-cases.

- Added a new vignette, “The Modelisation Approach”, which demonstrates
  a complete analytical workflow from setting up models using DAGs to
  extracting meaningful predictions, slopes, and interaction contrasts
  using modelbased post-estimation functions.

- [`estimate_means()`](https://easystats.github.io/modelbased/reference/estimate_means.md)
  with `backend = "emmeans"` now supports ordinal models, e.g. from
  [`MASS::polr()`](https://rdrr.io/pkg/MASS/man/polr.html) or
  [`ordinal::clm()`](https://rdrr.io/pkg/ordinal/man/clm.html).
  `predict` accepts the ordinal modes of *emmeans* (`"prob"`,
  `"cum.prob"`, `"exc.prob"`, `"linear.predictor"`, `"latent"` and
  `"mean.class"`). For `"prob"`, the output has one row per response
  category, in a `Response` column. For `"cum.prob"`, `"exc.prob"` and
  `"linear.predictor"`, the output has one row per threshold, in a
  `Threshold` column. For ordinal models, the default is now
  `predict = "prob"`, which returns the same probabilities as the
  `"marginaleffects"` backend.

- For ordinal models,
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) of
  [`estimate_means()`](https://easystats.github.io/modelbased/reference/estimate_means.md)
  output with `backend = "emmeans"` now maps the `Response` or
  `Threshold` column to the color with one `by` variable and to the
  facets with more. The `"marginaleffects"` backend already does this
  for `Response`.

- With `backend = "emmeans"`,
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) now shows
  predicted probabilities on a percent y-axis, `"cum.prob"` and
  `"exc.prob"` included. This applies to binomial, beta, ordinal, and
  multinomial models.

- `comparison = "ratio"` is a new shortcut to compute pairwise
  comparisons as ratios.

- The printed footer of
  [`estimate_prediction()`](https://easystats.github.io/modelbased/reference/estimate_expectation.md)
  and similar functions now names prediction intervals as such
  ([\#485](https://github.com/easystats/modelbased/issues/485)).

### Bug fixes

- [`estimate_means()`](https://easystats.github.io/modelbased/reference/estimate_means.md),
  [`estimate_contrasts()`](https://easystats.github.io/modelbased/reference/estimate_contrasts.md)
  and
  [`estimate_slopes()`](https://easystats.github.io/modelbased/reference/estimate_slopes.md)
  now filter the response categories of ordinal models, for example from
  [`MASS::polr()`](https://rdrr.io/pkg/MASS/man/polr.html),
  [`ordinal::clm()`](https://rdrr.io/pkg/ordinal/man/clm.html) or
  [`ordinal::clmm()`](https://rdrr.io/pkg/ordinal/man/clmm.html). The
  filter is a `by` term that names the response with values, for example
  `by = c("Infl", "Sat = 'Low'")`. The output then has only the rows of
  these response categories. The filter works with `backend = "emmeans"`
  and `predict = "prob"` (the default). Before, means and contrasts of
  factors ignored the filter, and slopes and contrasts of slopes stopped
  with an unclear error. If `predict` has another value, these functions
  now stop with a clear error. If a value is not a response category,
  they also stop with an error.

- For non-linear models, the footer of predictions or contrasts on the
  link-scale no longer says “FALSE-transformed” or “TRUE-transformed”.
  This footer appears, for example, with
  [`estimate_link()`](https://easystats.github.io/modelbased/reference/estimate_expectation.md),
  or with
  [`estimate_means()`](https://easystats.github.io/modelbased/reference/estimate_means.md)
  and `predict = "link"`. Without `transform`, the footer now says
  “link-scale”. Except with `backend = "emmeans"`, a function as
  `transform` gives “transformed link-scale”.
  [`estimate_contrasts()`](https://easystats.github.io/modelbased/reference/estimate_contrasts.md)
  with `predict = "link"` and a function as `transform` (for example
  `transform = exp`) no longer fails with an error. If the output of an
  earlier call is the `data` argument, the footer of
  [`estimate_expectation()`](https://easystats.github.io/modelbased/reference/estimate_expectation.md),
  [`estimate_prediction()`](https://easystats.github.io/modelbased/reference/estimate_expectation.md)
  and
  [`estimate_link()`](https://easystats.github.io/modelbased/reference/estimate_expectation.md)
  now names the right scale.

- With `backend = "marginaleffects"`,
  [`estimate_means()`](https://easystats.github.io/modelbased/reference/estimate_means.md),
  [`estimate_slopes()`](https://easystats.github.io/modelbased/reference/estimate_slopes.md)
  and
  [`estimate_contrasts()`](https://easystats.github.io/modelbased/reference/estimate_contrasts.md)
  no longer drop or rename a `by` variable with a name such as `type`,
  `n` or `value`. They also no longer mislabel other columns because of
  such a name. Before, for example,
  [`estimate_means()`](https://easystats.github.io/modelbased/reference/estimate_means.md)
  dropped a `by` variable named `type` and renamed `n` to `n_Obs`. It
  also labeled a variable named `Median` as `Mean`, and the means as
  `Coefficient`. This fix does not yet apply to Bayesian models or to
  inequality comparisons (for example `comparison = "inequality"`).

- Fixed printing error for brms-Wiener models without `component`
  column.

- With `backend = "marginaleffects"`, a `by` variable with the name of
  the estimate column (for example `Mean`) or of the `t` column is now
  returned as `<name>.1`. The output column keeps its name. This also
  holds for `p` with
  [`estimate_slopes()`](https://easystats.github.io/modelbased/reference/estimate_slopes.md)
  and
  [`estimate_contrasts()`](https://easystats.github.io/modelbased/reference/estimate_contrasts.md),
  and for `SE`, `CI_low` and `CI_high` with
  [`estimate_slopes()`](https://easystats.github.io/modelbased/reference/estimate_slopes.md).
  Before, for example,
  [`estimate_means()`](https://easystats.github.io/modelbased/reference/estimate_means.md)
  kept a `by` variable named `Mean` and renamed the means to `Mean.1`,
  and
  [`estimate_contrasts()`](https://easystats.github.io/modelbased/reference/estimate_contrasts.md)
  renamed the p-values to `p.1`. As for the fix above, this does not yet
  apply to Bayesian models or to inequality comparisons. It also does
  not apply to
  [`estimate_contrasts()`](https://easystats.github.io/modelbased/reference/estimate_contrasts.md)
  on the output of
  [`estimate_relation()`](https://easystats.github.io/modelbased/reference/estimate_expectation.md).

- With `backend = "marginaleffects"`,
  [`estimate_means()`](https://easystats.github.io/modelbased/reference/estimate_means.md),
  [`estimate_contrasts()`](https://easystats.github.io/modelbased/reference/estimate_contrasts.md)
  and
  [`estimate_slopes()`](https://easystats.github.io/modelbased/reference/estimate_slopes.md)
  now stop with an informative error when a variable has a name that
  *marginaleffects* uses for its own columns, and ask users to rename
  the variable. Before, these names gave wrong estimates, standard
  errors or confidence intervals, missing columns, or unclear errors.
  The names are:

  - `by` variables of
    [`estimate_means()`](https://easystats.github.io/modelbased/reference/estimate_means.md),
    and of
    [`estimate_contrasts()`](https://easystats.github.io/modelbased/reference/estimate_contrasts.md)
    when the contrast variable is a factor: `Predicted`, `SE`, `CI_low`,
    `CI_high`, `df`, `rowid` and `s.value`.
  - a factor `contrast` variable of
    [`estimate_contrasts()`](https://easystats.github.io/modelbased/reference/estimate_contrasts.md):
    `SE`, `CI_low`, `CI_high`, `df` and `rowid`.
  - other predictors of
    [`estimate_means()`](https://easystats.github.io/modelbased/reference/estimate_means.md)
    and
    [`estimate_contrasts()`](https://easystats.github.io/modelbased/reference/estimate_contrasts.md):
    `SE`, `CI_low`, `CI_high` and `df`.
  - `by` variables of
    [`estimate_slopes()`](https://easystats.github.io/modelbased/reference/estimate_slopes.md):
    `df`, `rowid`, `s.value`, `predicted`, `predicted_hi` and
    `predicted_lo`.

  Bayesian models are not checked yet. For them, these names can still
  give missing columns or unclear errors.

- With `backend = "marginaleffects"`,
  [`estimate_contrasts()`](https://easystats.github.io/modelbased/reference/estimate_contrasts.md)
  now returns a `by` variable named `Level1` or `Level2` as `Level1.1`
  or `Level2.1`, next to the `Level1` and `Level2` columns of the
  contrasts. It also returns the contrasts for a `by` variable named
  `hypothesis` and for a `contrast` variable named `Parameter`. Before,
  the output had two columns with the same name, the contrast labels
  were not split into `Level1` and `Level2`, or the call failed with an
  unclear error. If `<name>.1` is already a variable name, the suffix is
  the next free number, for example `p.2`. As for the fixes above, the
  `Level1`, `Level2` and `hypothesis` fixes do not yet apply to Bayesian
  models or to inequality comparisons.

- If a variable has the name of an output column,
  [`estimate_contrasts()`](https://easystats.github.io/modelbased/reference/estimate_contrasts.md)
  and
  [`estimate_slopes()`](https://easystats.github.io/modelbased/reference/estimate_slopes.md)
  with `backend = "marginaleffects"` now keep the values and the order
  of that column. This applies to the `contrast` variable, and to `by`
  variables of contrasts and of contrasts of slopes. It also applies to
  `by` variables of contrasts with a custom `comparison`, for example
  `comparison = "b2=b1"`. Before, a `contrast` variable named `t` turned
  the `t` column into a factor. A `contrast` variable named `p` replaced
  the p-values with missing values and moved the `p` column to the
  front. In contrasts, a `by` variable named `Slope` gave two
  `Difference` columns. In contrasts of slopes, it replaced the
  differences with missing values. With `comparison = "ratio"`, a `by`
  variable named `Ratio` or `Slope` gave two `Ratio` columns. Now a `by`
  variable named `Ratio` becomes `Ratio.1` there.

- [`estimate_slopes()`](https://easystats.github.io/modelbased/reference/estimate_slopes.md)
  with `backend = "marginaleffects"` now adjusts the p-values for
  `p_adjust = "holm"`, `"hochberg"`, `"hommel"`, `"bonferroni"`, `"BH"`,
  `"BY"`, `"fdr"`, `"sidak"` and `"tukey"`. Before, all but `"tukey"`
  failed with the error “replacement has 0 rows”, and `"tukey"` returned
  the unadjusted p-values. For `"tukey"`,
  [`estimate_slopes()`](https://easystats.github.io/modelbased/reference/estimate_slopes.md)
  and
  [`get_marginaltrends()`](https://easystats.github.io/modelbased/reference/get_emmeans.md)
  now use the Sidak adjustment and say so in a message. The Tukey
  adjustment is only appropriate for pairwise comparisons, but these
  slopes are each tested against 0.
  [`emmeans::test()`](https://rvlenth.github.io/emmeans/reference/summary.emmGrid.html)
  makes the same substitution. Slopes that are compared, for example
  with the `comparison` or `post_process` argument, keep the Tukey
  adjustment.

- [`estimate_means()`](https://easystats.github.io/modelbased/reference/estimate_means.md)
  with `backend = "emmeans"` now shows the response level names for
  models from
  [`ordinal::clmm()`](https://rdrr.io/pkg/ordinal/man/clmm.html).
  Before, the `Response` column showed `1`, `2`, `3`, and the
  `Threshold` column showed `1|2`, `2|3`.

- Fixed issue in
  [`estimate_contrasts()`](https://easystats.github.io/modelbased/reference/estimate_contrasts.md)
  with wrong assignment of estimates in custom comparisons (e.g., when
  `comparison = "(b1 - b12) = (b4 - b18)"`), when some of the indexed
  coefficients contained the same digit (in this case, `1` in `b1` and
  `b12`).

- Fixed issues in offset handling when the offset was specified via the
  `offset` argument in the model call instead of inside the formula.

- Fixed issues with using offsets and predicting grand mean (i.e. when
  model contains offset and `by = NULL`).

- [`plot()`](https://rdrr.io/r/graphics/plot.default.html) now gives an
  informative error when there is nothing to put on the x-axis, e.g. for
  predictions from intercept-only models, or for
  [`estimate_prediction()`](https://easystats.github.io/modelbased/reference/estimate_expectation.md)
  without the `by` argument.

- Fixed an “invalid regular expression” error in
  [`estimate_contrasts()`](https://easystats.github.io/modelbased/reference/estimate_contrasts.md)
  for multivariate models (for example from *brms*) with many
  observations.
  [`estimate_contrasts()`](https://easystats.github.io/modelbased/reference/estimate_contrasts.md)
  now uses the names of the response variables, not their observed
  values, to find the response in the contrast labels.

- [`estimate_contrasts()`](https://easystats.github.io/modelbased/reference/estimate_contrasts.md)
  now returns separate `Response1` and `Response2` columns for
  multivariate *brms* models with dots in the response names, for
  example `mvbind(Sepal.Length, Petal.Length) ~ Species`. Before, the
  response names stayed in the `Level1` and `Level2` columns, because
  *brms* removes the dots from the response names.

- The `grid` argument of
  [`visualisation_recipe()`](https://easystats.github.io/datawizard/reference/visualisation_recipe.html)
  and [`plot()`](https://rdrr.io/r/graphics/plot.default.html) now
  changes the facet grid of plots with four or more `by` variables.
  Before, these functions applied the `facet` argument to the facet grid
  instead. If `facet` was not set, `grid` gave an error.

## modelbased 0.17.0

CRAN release: 2026-08-28

### Breaking Changes

- The `comparison` argument in
  [`estimate_contrasts()`](https://easystats.github.io/modelbased/reference/estimate_contrasts.md)
  for `backend = "emmeans"` now defaults to `"revpairwise"`, to return
  consistent results regarding the sign of contrasts with the
  `"marginaleffects"` backend.

### Changes

- For contrasts of models where predictions vary by response category
  (e.g., categorical, ordinal/cumulative or multinomial models, as well
  as multivariate response models),
  [`estimate_contrasts()`](https://easystats.github.io/modelbased/reference/estimate_contrasts.md)
  now returns separate `Response1` and `Response2` columns instead of
  merging the response category label into the `Level1` and `Level2`
  columns ([\#646](https://github.com/easystats/modelbased/issues/646)).

- The `iterations` argument can now also be used for Bayesian model in
  [`estimate_means()`](https://easystats.github.io/modelbased/reference/estimate_means.md),
  [`estimate_slopes()`](https://easystats.github.io/modelbased/reference/estimate_slopes.md),
  and
  [`estimate_contrasts()`](https://easystats.github.io/modelbased/reference/estimate_contrasts.md),
  which is then passed to the `ndraws` argument and controls how many
  samples are drawn from the posterior when calculating marginal means,
  effects or contrasts.

- `ci = NULL` now suppresses calculation of standard errors and
  confidence intervals.

- The `data` argument can be used as an alias for `newdata` in
  [`estimate_means()`](https://easystats.github.io/modelbased/reference/estimate_means.md),
  [`estimate_slopes()`](https://easystats.github.io/modelbased/reference/estimate_slopes.md),
  and
  [`estimate_contrasts()`](https://easystats.github.io/modelbased/reference/estimate_contrasts.md),
  and is passed to the related *marginaleffects* functions.

- Improved default settings for plots using
  [`tinyplot()`](https://grantmcdermott.com/tinyplot/man/tinyplot.html):

  - `facet`s now automatically remove redundant axes. If you prefer to
    keep them, you can simply enable them by setting `frame = TRUE`.
  - The x-axis now automatically adjusts its limits when categorical
    predictors are used. By setting `xlim` to `c(0.5, n + 0.5)`, the
    geoms are moved closer together, resulting in a more compact
    appearance.

- `tinyplot`() for *modelbased* objects now supports standard arguments
  following easystats conventions, allowing for more fine-grained plot
  customizations (e.g., text/line sizes, color palettes, and axis title
  sizes).

- New options for printing (in particular, formatting CIs), see also
  documentation
  ([`?modelbased::"modelbased-options"`](https://easystats.github.io/modelbased/reference/modelbased-options.md)).

- Improved documentation for
  [`estimate_means()`](https://easystats.github.io/modelbased/reference/estimate_means.md)
  and vignettes about technical details of marginalization.

### Bug fixes

- [`estimate_contrasts()`](https://easystats.github.io/modelbased/reference/estimate_contrasts.md)
  with `backend = "emmeans"` now correctly parses factor levels that
  contain `" - "` in their names (e.g., `"A - High"`, `"B - Low"`).
  emmeans wraps such levels in parentheses (e.g.,
  `"(A - Low) - (A - High)"`), which is now handled properly
  ([\#649](https://github.com/easystats/modelbased/issues/649)).

- Fixed issue with `estimate = "average"` in
  [`estimate_slopes()`](https://easystats.github.io/modelbased/reference/estimate_slopes.md)
  when no `by` variable was specified and therefore no data grid
  created. This caused an issue with filtering the output.

## modelbased 0.16.0

CRAN release: 2026-06-30

### Changes

- Informative error message when the `trend` variable in
  [`estimate_slopes()`](https://easystats.github.io/modelbased/reference/estimate_slopes.md)
  is not numeric and `backend = "emmeans"`.

- [`estimate_contrasts()`](https://easystats.github.io/modelbased/reference/estimate_contrasts.md)
  gets a `post_process` argument, to process subsequent comparisons. It
  allows for complex, multi-step comparisons.

- The `comparison` argument in
  [`estimate_contrasts()`](https://easystats.github.io/modelbased/reference/estimate_contrasts.md)
  can now also be `"omnibus"`, to calculate a global omnibus test for
  differences between levels of a predictor.

- Offsets (using the `offset` argument) now also work when `estimate` is
  set to `"population"` or `"average"`.

- Argument `slope`, when specific values are defined, can now also be a
  list instead of only a string. E.g.,
  `slope="<variable> = seq(1, 3, 0.1)"` can now also be written as
  `slope=list(<variable> = seq(1, 3, 0.1))`.

- Clarified usage of `marginal` group-level estimates in
  [`estimate_grouplevel()`](https://easystats.github.io/modelbased/reference/estimate_grouplevel.md).
  The `type` argument can now be used in combination with the `estimate`
  argument, which is now also more explained in detail in the
  documentation.

- `by = NULL` now also works for `backend = "emmeans"`, to predict the
  grand mean.

### Bug fixes

- Minor bug fix when evaluating `slope` and `by` arguments in
  [`estimate_slopes()`](https://easystats.github.io/modelbased/reference/estimate_slopes.md).

- [`get_emcontrasts()`](https://easystats.github.io/modelbased/reference/get_emmeans.md),
  [`get_emmeans()`](https://easystats.github.io/modelbased/reference/get_emmeans.md)
  and
  [`get_emtrends()`](https://easystats.github.io/modelbased/reference/get_emmeans.md)
  (i.e., functions using `backend = "emmeans"`) now give an informative
  error message when `by`, `contrast`, or `trend` refer to a variable
  that is only used as random effects grouping factor (e.g., `Subject`
  in `(1 | Subject)`), instead of failing with a cryptic error. Such
  variables can only be used with `backend = "marginaleffects"`.

## modelbased 0.15.0

CRAN release: 2026-05-10

### Breaking Changes

- Argument `join_dots` in the
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) method, which
  is used to connect dots with lines, now defaults to `FALSE`.

### Changes

- Support for models of class `nestedLogit`.

- [`estimate_contrasts()`](https://easystats.github.io/modelbased/reference/estimate_contrasts.md)
  can now also calculate contrasts of average slopes, by defining two
  numeric predictors in the `contrast` argument. This can also be useful
  when estimating the context effects of within- and between-effects in
  a model.

## modelbased 0.14.0

CRAN release: 2026-02-17

### Changes

- The `trend` argument in
  [`estimate_slopes()`](https://easystats.github.io/modelbased/reference/estimate_slopes.md)
  was renamed into `slope`. `trend` will remain as alias.

- New function
  [`collapse_by_group()`](https://easystats.github.io/modelbased/reference/collapse_by_group.md),
  which extracts the raw data points and “averages” (i.e. “collapses”)
  the response variable over the levels of the grouping factor given in
  `collapse_by`. Only works with mixed models. Additionally, the
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) and
  `visualization_recipe()` methods get a `collapse_group` argument to
  use this feature when adding data points to plots using `show_data` or
  `show_residuals`.

### Bug fixes

- Fixed issue in `estimate_slope()` when `p_adjust = "esarey"`.

- Fixed issue in
  [`estimate_contrasts()`](https://easystats.github.io/modelbased/reference/estimate_contrasts.md)
  when `p_adjust = "tukey"`.

- Fixed error with truncated message when standard errors could not be
  calculated.

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
