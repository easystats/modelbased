# Consistent API for 'emmeans' and 'marginaleffects'

These functions are convenient wrappers around the **emmeans** and the
**marginaleffects** packages. They are mostly available for developers
who want to leverage a unified API for getting model-based estimates,
and regular users should use the `estimate_*` set of functions.

The `get_emmeans()`, `get_emcontrasts()` and `get_emtrends()` functions
are wrappers around
[`emmeans::emmeans()`](https://rvlenth.github.io/emmeans/reference/emmeans.html)
and
[`emmeans::emtrends()`](https://rvlenth.github.io/emmeans/reference/emtrends.html).

## Usage

``` r
get_emcontrasts(
  model,
  contrast = NULL,
  by = NULL,
  predict = NULL,
  comparison = "pairwise",
  keep_iterations = FALSE,
  verbose = TRUE,
  ...
)

get_emmeans(
  model,
  by = "auto",
  predict = NULL,
  keep_iterations = FALSE,
  verbose = TRUE,
  ...
)

get_emtrends(
  model,
  trend = NULL,
  by = NULL,
  predict = NULL,
  keep_iterations = FALSE,
  verbose = TRUE,
  ...
)

get_marginalcontrasts(
  model,
  contrast = NULL,
  by = NULL,
  predict = NULL,
  ci = 0.95,
  comparison = "pairwise",
  estimate = NULL,
  transform = NULL,
  p_adjust = "none",
  keep_iterations = FALSE,
  verbose = TRUE,
  ...
)

get_marginalmeans(
  model,
  by = "auto",
  predict = NULL,
  ci = 0.95,
  estimate = NULL,
  transform = NULL,
  keep_iterations = FALSE,
  verbose = TRUE,
  ...
)

get_marginaltrends(
  model,
  trend = NULL,
  by = NULL,
  predict = NULL,
  ci = 0.95,
  estimate = NULL,
  transform = NULL,
  p_adjust = "none",
  keep_iterations = FALSE,
  verbose = TRUE,
  ...
)
```

## Arguments

- model:

  A statistical model.

- contrast:

  A character vector indicating the name of the variable(s) for which to
  compute the contrasts, optionally including representative values or
  levels at which contrasts are evaluated (e.g.,
  `contrast="x=c('a','b')"`).

- by:

  The (focal) predictor variable(s) at which to evaluate the desired
  effect / mean / contrasts. Other predictors of the model that are not
  included here will be collapsed and "averaged" over (the effect will
  be estimated across them). `by` can be a character (vector) naming the
  focal predictors, optionally including representative values or levels
  at which focal predictors are evaluated (e.g., `by = "x = c(1, 2)"`).
  When `estimate` is *not* `"average"`, the `by` argument is used to
  create a "reference grid" or "data grid" with representative values
  for the focal predictors. In this case, `by` can also be list of named
  elements. See details in
  [`insight::get_datagrid()`](https://easystats.github.io/insight/reference/get_datagrid.html)
  to learn more about how to create data grids for predictors of
  interest.

- predict:

  Is passed to the `type` argument in
  [`emmeans::emmeans()`](https://rvlenth.github.io/emmeans/reference/emmeans.html)
  (when `backend = "emmeans"`) or in
  [`marginaleffects::avg_predictions()`](https://marginaleffects.com/man/r/predictions.html)
  (when `backend = "marginaleffects"`). Valid options for `predict` are:

  - `backend = "marginaleffects"`: `predict` can be `"response"`,
    `"link"`, `"inverse_link"` or any valid `type` option supported by
    model's class [`predict()`](https://rdrr.io/r/stats/predict.html)
    method (e.g., for zero-inflation models from package **glmmTMB**,
    you can choose `predict = "zprob"` or `predict = "conditional"`
    etc., see
    [glmmTMB::predict.glmmTMB](https://rdrr.io/pkg/glmmTMB/man/predict.glmmTMB.html)).
    By default, when `predict = NULL`, the most appropriate
    transformation is selected, which usually returns predictions or
    contrasts on the response-scale. The `"inverse_link"` is a special
    option, comparable to *marginaleffects*' `invlink(link)` option. It
    will calculate predictions on the link scale and then back-transform
    to the response scale.

  - `backend = "emmeans"`: `predict` can be `"response"`, `"link"`,
    `"mu"`, `"unlink"`, or `"log"`. If `predict = NULL` (default), the
    most appropriate transformation is selected (which usually is
    `"response"`). See also [this
    vignette](https://CRAN.R-project.org/package=emmeans/vignettes/transformations.html).

  See also section *Predictions on different scales*.

- comparison:

  Specify the type of contrasts or tests that should be carried out.

  - When `backend = "emmeans"`, can be one of `"pairwise"`, `"poly"`,
    `"consec"`, `"eff"`, `"del.eff"`, `"mean_chg"`, `"trt.vs.ctrl"`,
    `"dunnett"`, `"wtcon"` and some more. To test multiple hypotheses
    jointly (usually used for factorial designs), `comparison` can also
    be `"joint"`. See also `method` argument in
    [emmeans::contrast](https://rvlenth.github.io/emmeans/reference/contrast.html)
    and the `?emmeans::emmc-functions`.

  - For `backend = "marginaleffects"`, can be a numeric value, vector,
    or matrix, a string equation specifying the hypothesis to test, a
    string naming the comparison method, a formula, or a function. For
    options not described below, see documentation of
    [marginaleffects::comparisons](https://marginaleffects.com/man/r/comparisons.html),
    [this website](https://marginaleffects.com/bonus/hypothesis.html)
    and section *Comparison options* below.

    - String: One of `"pairwise"`, `"reference"`, `"sequential"`,
      `"meandev"` `"meanotherdev"`, `"poly"`, `"helmert"`, or
      `"trt_vs_ctrl"`. To test multiple hypotheses jointly (usually used
      for factorial designs), `comparison` can also be `"joint"`. In
      this case, use the `test` argument to specify which test should be
      conducted: `"F"` (default) or `"Chi2"`.

    - String: Special string options are `"inequality"`,
      `"inequality_ratio"`, and `"inequality_pairwise"`.
      `comparison = "inequality"` computes the marginal effect
      inequality summary of categorical predictors' overall effects,
      respectively, the comprehensive effect of an independent variable
      across all outcome categories of a nominal or ordinal dependent
      variable (also called *absolute inequality*, or total marginal
      effect, see *Mize and Han, 2025*). `"inequality_ratio"` computes
      the ratio of marginal effect inequality measures, also known as
      *relative inequality*. This is useful to compare the relative
      effects of different predictors on the dependent variable. It
      provides a measure of how much more or less inequality one
      predictor has compared to another.
      `comparison = "inequality_pairwise"` computes pairwise differences
      of absolute inequality measures, while
      `"inequality_ratio_pairwise"` computes pairwise differences of
      relative inequality measures (ratios). See an overview of
      applications in the related case study in the
      [vignettes](https://easystats.github.io/modelbased/articles/practical_inequalities.html).

    - String equation: To identify parameters from the output, either
      specify the term name, or `"b1"`, `"b2"` etc. to indicate rows,
      e.g.:`"hp = drat"`, `"b1 = b2"`, or `"b1 + b2 + b3 = 0"`.

    - Formula: A formula like `comparison ~ pairs | group`, where the
      left-hand side indicates the type of comparison (`difference` or
      `ratio`), the right-hand side determines the pairs of estimates to
      compare (`reference`, `sequential`, `meandev`, etc., see
      string-options). Optionally, comparisons can be carried out within
      subsets by indicating the grouping variable after a vertical bar (
      `|`).

    - A custom function, e.g. `comparison = myfun`, or
      `comparison ~ I(my_fun(x)) | groups`.

    - If contrasts should be calculated (or grouped by) factors,
      `comparison` can also be a matrix that specifies factor contrasts
      (see 'Examples').

- keep_iterations:

  If `TRUE`, will keep all iterations (draws) of bootstrapped or
  Bayesian models. They will be added as additional columns named
  `iter_1`, `iter_2`, and so on. If `keep_iterations` is a positive
  number, only as many columns as indicated in `keep_iterations` will be
  added to the output. You can reshape them to a long format by running
  [`bayestestR::reshape_iterations()`](https://easystats.github.io/bayestestR/reference/reshape_iterations.html).

- verbose:

  Use `FALSE` to silence messages and warnings.

- ...:

  Other arguments passed, for instance, to
  [`insight::get_datagrid()`](https://easystats.github.io/insight/reference/get_datagrid.html),
  to functions from the **emmeans** or **marginaleffects** package, or
  to process Bayesian models via
  [`bayestestR::describe_posterior()`](https://easystats.github.io/bayestestR/reference/describe_posterior.html).
  Examples:

  - [`insight::get_datagrid()`](https://easystats.github.io/insight/reference/get_datagrid.html):
    Argument such as `length`, `digits` or `range` can be used to
    control the (number of) representative values. For integer
    variables, `protect_integers` modulates whether these should also be
    treated as numerics, i.e. values can have fractions or not.

  - **marginaleffects**: Internally used functions are
    `avg_predictions()` for means and contrasts, and `avg_slope()` for
    slopes. Therefore, arguments for instance like `vcov`,
    `equivalence`, `df`, `slope`, `hypothesis` or even `newdata` can be
    passed to those functions. A `weights` argument is passed to the
    `wts` argument in `avg_predictions()` or `avg_slopes()`, however,
    weights can only be applied when `estimate` is `"average"` or
    `"population"` (i.e. for those marginalization options that do not
    use data grids). Other arguments, such as `re.form` or
    `allow.new.levels`, may be passed to
    [`predict()`](https://rdrr.io/r/stats/predict.html) (which is
    internally used by *marginaleffects*) if supported by that model
    class.

  - **emmeans**: Internally used functions are `emmeans()` and
    `emtrends()`. Additional arguments can be passed to these functions.

  - Bayesian models: For Bayesian models, parameters are cleaned using
    `describe_posterior()`, thus, arguments like, for example,
    `centrality`, `rope_range`, or `test` are passed to that function.

  - Especially for
    [`estimate_contrasts()`](https://easystats.github.io/modelbased/reference/estimate_contrasts.md)
    with integer focal predictors, for which contrasts should be
    calculated, use argument `integer_as_continuous` to set the maximum
    number of unique values in an integer predictor to treat that
    predictor as "discrete integer" or as numeric. For the first case,
    contrasts are calculated between values of the predictor, for the
    latter, contrasts of slopes are calculated. If the integer has more
    than `integer_as_continuous` unique values, it is treated as
    numeric. Defaults to `5`. Set to `TRUE` to always treat integer
    predictors as continuous.

  - For count regression models that use an offset term, use
    `offset = <value>` to fix the offset at a specific value. Or use
    `estimate = "average"`, to average predictions over the distribution
    of the offset (if appropriate).

- trend:

  A character indicating the name of the variable for which to compute
  the slopes. To get marginal effects at specific values, use
  `trend="<variable>"` along with the `by` argument, e.g.
  `by="<variable>=c(1, 3, 5)"`, or a combination of `by` and `length`,
  for instance, `by="<variable>", length=30`. To calculate average
  marginal effects over a range of values, use
  `trend="<variable>=seq(1, 3, 0.1)"` (or similar) and omit the variable
  provided in `trend` from the `by` argument.

- ci:

  Confidence Interval (CI) level. Default to `0.95` (`95%`).

- estimate:

  The `estimate` argument determines how predictions are averaged
  ("marginalized") over variables not specified in `by` or `contrast`
  (non-focal predictors). It controls whether predictions represent a
  "typical" individual, an "average" individual from the sample, or an
  "average" individual from a broader population.

  - `"typical"` (Default): Calculates predictions for a balanced data
    grid representing all combinations of focal predictor levels
    (specified in `by`). For non-focal numeric predictors, it uses the
    mean; for non-focal categorical predictors, it marginalizes
    (averages) over the levels. This represents a "typical" observation
    based on the data grid and is useful for comparing groups. It
    answers: "What would the average outcome be for a 'typical'
    observation?". This is the default approach when estimating marginal
    means using the *emmeans* package.

  - `"average"`: Calculates predictions for each observation in the
    sample and then averages these predictions within each group defined
    by the focal predictors. This reflects the sample's actual
    distribution of non-focal predictors, not a balanced grid. It
    answers: "What is the predicted value for an average observation in
    my data?"

  - `"population"`: "Clones" each observation, creating copies with all
    possible combinations of focal predictor levels. It then averages
    the predictions across these "counterfactual" observations
    (non-observed permutations) within each group. This extrapolates to
    a hypothetical broader population, considering "what if" scenarios.
    It answers: "What is the predicted response for the 'average'
    observation in a broader possible target population?" This approach
    entails more assumptions about the likelihood of different
    combinations, but can be more apt to generalize. This is also the
    option that should be used for **G-computation** (causal inference,
    see *Chatton and Rohrer 2024*). `"counterfactual"` is an alias for
    `"population"`.

  You can set a default option for the `estimate` argument via
  [`options()`](https://rdrr.io/r/base/options.html), e.g.
  `options(modelbased_estimate = "average")`.

  Note following limitations:

  - When you set `estimate` to `"average"`, it calculates the average
    based only on the data points that actually exist. This is in
    particular important for two or more focal predictors, because it
    doesn't generate a *complete* grid of all theoretical combinations
    of predictor values. Consequently, the output may not include all
    the values.

  - Filtering the output at values of continuous predictors, e.g.
    `by = "x=1:5"`, in combination with `estimate = "average"` may
    result in returning an empty data frame because of what was
    described above. In such case, you can use `estimate = "typical"` or
    use the `newdata` argument to provide a data grid of predictor
    values at which to evaluate predictions.

  - `estimate = "population"` is not available for
    [`estimate_slopes()`](https://easystats.github.io/modelbased/reference/estimate_slopes.md).

- transform:

  A function applied to predictions and confidence intervals to (back-)
  transform results, which can be useful in case the regression model
  has a transformed response variable (e.g., `lm(log(y) ~ x)`). For
  Bayesian models, this function is applied to individual draws from the
  posterior distribution, before computing summaries. Can also be
  `TRUE`, in which case
  [`insight::get_transformation()`](https://easystats.github.io/insight/reference/get_transformation.html)
  is called to determine the appropriate transformation-function. Note
  that no standard errors are returned when transformations are applied.

- p_adjust:

  The p-values adjustment method for frequentist multiple comparisons.
  For
  [`estimate_slopes()`](https://easystats.github.io/modelbased/reference/estimate_slopes.md),
  multiple comparison only occurs for Johnson-Neyman intervals, i.e. in
  case of interactions with two numeric predictors (one specified in
  `trend`, one in `by`). In this case, the `"esarey"` or `"sup-t"`
  options are recommended, but `p_adjust` can also be one of `"none"`
  (default), `"hochberg"`, `"hommel"`, `"bonferroni"`, `"BH"`, `"BY"`,
  `"fdr"`, `"tukey"`, `"sidak"`, or `"holm"`. `"sup-t"` computes
  simultaneous confidence bands, also called sup-t confidence band
  (Montiel Olea & Plagborg-Møller, 2019).

## Examples

``` r
# Basic usage
model <- lm(Sepal.Width ~ Species, data = iris)
get_emcontrasts(model)
#> No variable was specified for contrast estimation. Selecting `contrast =
#>   "Species"`.
#>  contrast               estimate     SE  df t.ratio p.value
#>  setosa - versicolor       0.658 0.0679 147   9.685  <.0001
#>  setosa - virginica        0.454 0.0679 147   6.683  <.0001
#>  versicolor - virginica   -0.204 0.0679 147  -3.003  0.0088
#> 
#> P value adjustment: tukey method for comparing a family of 3 estimates 

# \dontrun{
# Dealing with interactions
model <- lm(Sepal.Width ~ Species * Petal.Width, data = iris)
# By default: selects first factor
get_emcontrasts(model)
#> No variable was specified for contrast estimation. Selecting `contrast =
#>   "Species"`.
#>  contrast               estimate    SE  df t.ratio p.value
#>  setosa - versicolor       1.590 0.394 144   4.039  0.0003
#>  setosa - virginica        1.774 0.413 144   4.293  0.0001
#>  versicolor - virginica    0.184 0.145 144   1.272  0.4131
#> 
#> P value adjustment: tukey method for comparing a family of 3 estimates 
# Or both
get_emcontrasts(model, contrast = c("Species", "Petal.Width"), length = 2)
#>  contrast                                              estimate    SE  df
#>  setosa Petal.Width0.1 - versicolor Petal.Width0.1       1.8275 0.279 144
#>  setosa Petal.Width0.1 - virginica Petal.Width0.1        1.5479 0.312 144
#>  setosa Petal.Width0.1 - setosa Petal.Width2.5          -2.0093 0.977 144
#>  setosa Petal.Width0.1 - versicolor Petal.Width2.5      -0.7012 0.268 144
#>  setosa Petal.Width0.1 - virginica Petal.Width2.5        0.0325 0.112 144
#>  versicolor Petal.Width0.1 - virginica Petal.Width0.1   -0.2797 0.406 144
#>  versicolor Petal.Width0.1 - setosa Petal.Width2.5      -3.8368 0.957 144
#>  versicolor Petal.Width0.1 - versicolor Petal.Width2.5  -2.5288 0.521 144
#>  versicolor Petal.Width0.1 - virginica Petal.Width2.5   -1.7951 0.282 144
#>  virginica Petal.Width0.1 - setosa Petal.Width2.5       -3.5571 0.967 144
#>  virginica Petal.Width0.1 - versicolor Petal.Width2.5   -2.2491 0.399 144
#>  virginica Petal.Width0.1 - virginica Petal.Width2.5    -1.5154 0.375 144
#>  setosa Petal.Width2.5 - versicolor Petal.Width2.5       1.3080 0.954 144
#>  setosa Petal.Width2.5 - virginica Petal.Width2.5        2.0417 0.922 144
#>  versicolor Petal.Width2.5 - virginica Petal.Width2.5    0.7337 0.272 144
#>  t.ratio p.value
#>    6.550  <.0001
#>    4.955  <.0001
#>   -2.057  0.3158
#>   -2.614  0.1005
#>    0.289  0.9997
#>   -0.689  0.9829
#>   -4.009  0.0013
#>   -4.858  <.0001
#>   -6.355  <.0001
#>   -3.678  0.0044
#>   -5.642  <.0001
#>   -4.043  0.0012
#>    1.371  0.7441
#>    2.214  0.2379
#>    2.699  0.0817
#> 
#> P value adjustment: tukey method for comparing a family of 6 estimates 
# Or with custom specifications
get_emcontrasts(model, contrast = c("Species", "Petal.Width=c(1, 2)"))
#>  contrast                                          estimate     SE  df t.ratio
#>  setosa Petal.Width1 - versicolor Petal.Width1        1.633 0.3210 144   5.093
#>  setosa Petal.Width1 - virginica Petal.Width1         1.733 0.3510 144   4.933
#>  setosa Petal.Width1 - setosa Petal.Width2           -0.837 0.4070 144  -2.057
#>  setosa Petal.Width1 - versicolor Petal.Width2        0.579 0.3450 144   1.678
#>  setosa Petal.Width1 - virginica Petal.Width2         1.102 0.3130 144   3.523
#>  versicolor Petal.Width1 - virginica Petal.Width1     0.100 0.1850 144   0.542
#>  versicolor Petal.Width1 - setosa Petal.Width2       -2.470 0.7200 144  -3.431
#>  versicolor Petal.Width1 - versicolor Petal.Width2   -1.054 0.2170 144  -4.858
#>  versicolor Petal.Width1 - virginica Petal.Width2    -0.531 0.0928 144  -5.720
#>  virginica Petal.Width1 - setosa Petal.Width2        -2.570 0.7340 144  -3.501
#>  virginica Petal.Width1 - versicolor Petal.Width2    -1.154 0.2250 144  -5.128
#>  virginica Petal.Width1 - virginica Petal.Width2     -0.631 0.1560 144  -4.043
#>  setosa Petal.Width2 - versicolor Petal.Width2        1.416 0.7310 144   1.937
#>  setosa Petal.Width2 - virginica Petal.Width2         1.939 0.7160 144   2.706
#>  versicolor Petal.Width2 - virginica Petal.Width2     0.523 0.1580 144   3.306
#>  p.value
#>   <.0001
#>   <.0001
#>   0.3158
#>   0.5487
#>   0.0074
#>   0.9943
#>   0.0100
#>   <.0001
#>   <.0001
#>   0.0080
#>   <.0001
#>   0.0012
#>   0.3840
#>   0.0802
#>   0.0149
#> 
#> P value adjustment: tukey method for comparing a family of 6 estimates 
# Or modulate it
get_emcontrasts(model, by = "Petal.Width", length = 4)
#> No variable was specified for contrast estimation. Selecting `contrast =
#>   "Species"`.
#> Petal.Width = 0.1:
#>  contrast               estimate    SE  df t.ratio p.value
#>  setosa - versicolor      1.8275 0.279 144   6.550  <.0001
#>  setosa - virginica       1.5479 0.312 144   4.955  <.0001
#>  versicolor - virginica  -0.2797 0.406 144  -0.689  0.7703
#> 
#> Petal.Width = 0.9:
#>  contrast               estimate    SE  df t.ratio p.value
#>  setosa - versicolor      1.6544 0.288 144   5.743  <.0001
#>  setosa - virginica       1.7125 0.325 144   5.276  <.0001
#>  versicolor - virginica   0.0581 0.208 144   0.280  0.9577
#> 
#> Petal.Width = 1.7:
#>  contrast               estimate    SE  df t.ratio p.value
#>  setosa - versicolor      1.4812 0.600 144   2.467  0.0390
#>  setosa - virginica       1.8771 0.597 144   3.144  0.0057
#>  versicolor - virginica   0.3959 0.113 144   3.502  0.0018
#> 
#> Petal.Width = 2.5:
#>  contrast               estimate    SE  df t.ratio p.value
#>  setosa - versicolor      1.3080 0.954 144   1.371  0.3587
#>  setosa - virginica       2.0417 0.922 144   2.214  0.0722
#>  versicolor - virginica   0.7337 0.272 144   2.699  0.0212
#> 
#> P value adjustment: tukey method for comparing a family of 3 estimates 
# }
model <- lm(Sepal.Length ~ Species + Petal.Width, data = iris)

# By default, 'by' is set to "Species"
get_emmeans(model)
#> We selected `by = c("Species")`.
#>  Species    emmean     SE  df lower.CL upper.CL
#>  setosa       5.88 0.1970 146     5.49     6.27
#>  versicolor   5.82 0.0723 146     5.68     5.96
#>  virginica    5.83 0.1740 146     5.49     6.17
#> 
#> Confidence level used: 0.95 

# \dontrun{
# Overall mean (close to 'mean(iris$Sepal.Length)')
get_emmeans(model, by = NULL)
#>  1       emmean     SE  df lower.CL upper.CL
#>  overall   5.84 0.0393 146     5.77     5.92
#> 
#> Results are averaged over the levels of: Species 
#> Confidence level used: 0.95 

# One can estimate marginal means at several values of a 'modulate' variable
get_emmeans(model, by = "Petal.Width", length = 3)
#>  Petal.Width emmean     SE  df lower.CL upper.CL
#>          0.1   4.84 0.2170 146     4.41     5.26
#>          1.3   5.94 0.0439 146     5.85     6.02
#>          2.5   7.04 0.2550 146     6.53     7.54
#> 
#> Results are averaged over the levels of: Species 
#> Confidence level used: 0.95 

# Interactions
model <- lm(Sepal.Width ~ Species * Petal.Length, data = iris)

get_emmeans(model)
#> We selected `by = c("Species")`.
#>  Species    emmean     SE  df lower.CL upper.CL
#>  setosa       4.32 0.5990 144     3.13     5.50
#>  versicolor   2.58 0.0658 144     2.45     2.71
#>  virginica    2.55 0.1540 144     2.25     2.86
#> 
#> Confidence level used: 0.95 
get_emmeans(model, by = c("Species", "Petal.Length"), length = 2)
#>  Species    Petal.Length emmean    SE  df lower.CL upper.CL
#>  setosa              1.0   3.25 0.128 144    2.995     3.50
#>  versicolor          1.0   1.55 0.317 144    0.924     2.18
#>  virginica           1.0   1.91 0.375 144    1.165     2.65
#>  setosa              6.9   5.54 1.420 144    2.739     8.34
#>  versicolor          6.9   3.76 0.258 144    3.249     4.27
#>  virginica           6.9   3.29 0.119 144    3.055     3.53
#> 
#> Confidence level used: 0.95 
get_emmeans(model, by = c("Species", "Petal.Length = c(1, 3, 5)"), length = 2)
#>  Species    Petal.Length emmean     SE  df lower.CL upper.CL
#>  setosa                1   3.25 0.1280 144    2.995     3.50
#>  versicolor            1   1.55 0.3170 144    0.924     2.18
#>  virginica             1   1.91 0.3750 144    1.165     2.65
#>  setosa                3   4.02 0.4030 144    3.229     4.82
#>  versicolor            3   2.30 0.1290 144    2.043     2.55
#>  virginica             3   2.38 0.2140 144    1.954     2.80
#>  setosa                5   4.80 0.9220 144    2.979     6.62
#>  versicolor            5   3.05 0.0840 144    2.881     3.21
#>  virginica             5   2.84 0.0636 144    2.719     2.97
#> 
#> Confidence level used: 0.95 
# }
# \dontrun{
model <- lm(Sepal.Width ~ Species * Petal.Length, data = iris)

get_emtrends(model)
#> No numeric variable was specified for slope estimation. Selecting `trend
#>   = "Petal.Length"`.
#>  1       Petal.Length.trend     SE  df lower.CL upper.CL
#>  overall              0.332 0.0964 144    0.142    0.523
#> 
#> Results are averaged over the levels of: Species 
#> Confidence level used: 0.95 
get_emtrends(model, by = "Species")
#> No numeric variable was specified for slope estimation. Selecting `trend
#>   = "Petal.Length"`.
#>  Species    Petal.Length.trend     SE  df lower.CL upper.CL
#>  setosa                  0.388 0.2600 144  -0.1264    0.902
#>  versicolor              0.374 0.0961 144   0.1843    0.564
#>  virginica               0.234 0.0819 144   0.0725    0.396
#> 
#> Confidence level used: 0.95 
get_emtrends(model, by = "Petal.Length")
#> No numeric variable was specified for slope estimation. Selecting `trend
#>   = "Petal.Length"`.
#>  Petal.Length Petal.Length.trend     SE  df lower.CL upper.CL
#>          1.00              0.332 0.0964 144    0.142    0.523
#>          1.66              0.332 0.0964 144    0.142    0.523
#>          2.31              0.332 0.0964 144    0.142    0.523
#>          2.97              0.332 0.0964 144    0.142    0.523
#>          3.62              0.332 0.0964 144    0.142    0.523
#>          4.28              0.332 0.0964 144    0.142    0.523
#>          4.93              0.332 0.0964 144    0.142    0.523
#>          5.59              0.332 0.0964 144    0.142    0.523
#>          6.24              0.332 0.0964 144    0.142    0.523
#>          6.90              0.332 0.0964 144    0.142    0.523
#> 
#> Results are averaged over the levels of: Species 
#> Confidence level used: 0.95 
get_emtrends(model, by = c("Species", "Petal.Length"))
#> No numeric variable was specified for slope estimation. Selecting `trend
#>   = "Petal.Length"`.
#>  Species    Petal.Length Petal.Length.trend     SE  df lower.CL upper.CL
#>  setosa             1.00              0.388 0.2600 144  -0.1264    0.902
#>  versicolor         1.00              0.374 0.0961 144   0.1843    0.564
#>  virginica          1.00              0.234 0.0819 144   0.0725    0.396
#>  setosa             1.66              0.388 0.2600 144  -0.1264    0.902
#>  versicolor         1.66              0.374 0.0961 144   0.1843    0.564
#>  virginica          1.66              0.234 0.0819 144   0.0725    0.396
#>  setosa             2.31              0.388 0.2600 144  -0.1264    0.902
#>  versicolor         2.31              0.374 0.0961 144   0.1843    0.564
#>  virginica          2.31              0.234 0.0819 144   0.0725    0.396
#>  setosa             2.97              0.388 0.2600 144  -0.1264    0.902
#>  versicolor         2.97              0.374 0.0961 144   0.1843    0.564
#>  virginica          2.97              0.234 0.0819 144   0.0725    0.396
#>  setosa             3.62              0.388 0.2600 144  -0.1264    0.902
#>  versicolor         3.62              0.374 0.0961 144   0.1843    0.564
#>  virginica          3.62              0.234 0.0819 144   0.0725    0.396
#>  setosa             4.28              0.388 0.2600 144  -0.1264    0.902
#>  versicolor         4.28              0.374 0.0961 144   0.1843    0.564
#>  virginica          4.28              0.234 0.0819 144   0.0725    0.396
#>  setosa             4.93              0.388 0.2600 144  -0.1264    0.902
#>  versicolor         4.93              0.374 0.0961 144   0.1843    0.564
#>  virginica          4.93              0.234 0.0819 144   0.0725    0.396
#>  setosa             5.59              0.388 0.2600 144  -0.1264    0.902
#>  versicolor         5.59              0.374 0.0961 144   0.1843    0.564
#>  virginica          5.59              0.234 0.0819 144   0.0725    0.396
#>  setosa             6.24              0.388 0.2600 144  -0.1264    0.902
#>  versicolor         6.24              0.374 0.0961 144   0.1843    0.564
#>  virginica          6.24              0.234 0.0819 144   0.0725    0.396
#>  setosa             6.90              0.388 0.2600 144  -0.1264    0.902
#>  versicolor         6.90              0.374 0.0961 144   0.1843    0.564
#>  virginica          6.90              0.234 0.0819 144   0.0725    0.396
#> 
#> Confidence level used: 0.95 
# }

model <- lm(Petal.Length ~ poly(Sepal.Width, 4), data = iris)
get_emtrends(model)
#> No numeric variable was specified for slope estimation. Selecting `trend
#>   = "Sepal.Width"`.
#>  1       Sepal.Width.trend    SE  df lower.CL upper.CL
#>  overall             -2.67 0.548 145    -3.75    -1.58
#> 
#> Confidence level used: 0.95 
get_emtrends(model, by = "Sepal.Width")
#> No numeric variable was specified for slope estimation. Selecting `trend
#>   = "Sepal.Width"`.
#>  Sepal.Width Sepal.Width.trend    SE  df lower.CL upper.CL
#>         2.00             7.484 5.420 145   -3.225   18.192
#>         2.27             3.775 2.090 145   -0.357    7.906
#>         2.53             0.834 0.765 145   -0.678    2.346
#>         2.80            -1.337 0.706 145   -2.732    0.058
#>         3.07            -2.700 0.543 145   -3.773   -1.628
#>         3.33            -3.231 0.606 145   -4.430   -2.033
#>         3.60            -2.909 0.838 145   -4.564   -1.254
#>         3.87            -1.705 1.010 145   -3.701    0.290
#>         4.13             0.394 2.390 145   -4.327    5.116
#>         4.40             3.431 5.800 145   -8.028   14.890
#> 
#> Confidence level used: 0.95 
model <- lm(Sepal.Length ~ Species + Petal.Width, data = iris)

# By default, 'by' is set to "Species"
get_marginalmeans(model)
#> We selected `by=c("Species")`.
#> 
#>     Species Estimate Std. Error    t Pr(>|t|)     S 2.5 % 97.5 %  Df
#>  setosa         5.88     0.1969 29.9   <0.001 210.3  5.49   6.27 146
#>  versicolor     5.82     0.0723 80.5   <0.001 405.6  5.68   5.96 146
#>  virginica      5.83     0.1741 33.5   <0.001 231.4  5.49   6.17 146
#> 
#> Type: response
#> 

# Overall mean (close to 'mean(iris$Sepal.Length)')
get_marginalmeans(model, by = NULL)
#> 
#>  Estimate Std. Error   t Pr(>|t|)     S 2.5 % 97.5 %  Df
#>      5.84     0.0393 149   <0.001 533.4  5.77   5.92 146
#> 
#> Type: response
#> 

# \dontrun{
# One can estimate marginal means at several values of a 'modulate' variable
get_marginalmeans(model, by = "Petal.Width", length = 3)
#> 
#>  Petal.Width Estimate Std. Error     t Pr(>|t|)     S 2.5 % 97.5 %  Df
#>          0.1     4.84     0.2167  22.3   <0.001 160.0  4.41   5.26 146
#>          1.3     5.94     0.0439 135.3   <0.001 513.6  5.85   6.02 146
#>          2.5     7.04     0.2552  27.6   <0.001 196.1  6.53   7.54 146
#> 
#> Type: response
#> 

# Interactions
model <- lm(Sepal.Width ~ Species * Petal.Length, data = iris)

get_marginalmeans(model)
#> We selected `by=c("Species")`.
#> 
#>     Species Estimate Std. Error     t Pr(>|t|)     S 2.5 % 97.5 %  Df
#>  setosa         4.32     0.5990  7.21   <0.001  35.0  3.13   5.50 144
#>  versicolor     2.58     0.0658 39.24   <0.001 259.3  2.45   2.71 144
#>  virginica      2.55     0.1535 16.63   <0.001 115.0  2.25   2.86 144
#> 
#> Type: response
#> 
get_marginalmeans(model, by = c("Species", "Petal.Length"), length = 2)
#> 
#>     Species Petal.Length Estimate Std. Error     t Pr(>|t|)     S 2.5 % 97.5 %
#>  setosa              1.0     3.25      0.128 25.33   <0.001 180.0 2.995   3.50
#>  setosa              6.9     5.54      1.415  3.91   <0.001  12.8 2.739   8.34
#>  versicolor          1.0     1.55      0.317  4.89   <0.001  18.5 0.924   2.18
#>  versicolor          6.9     3.76      0.258 14.58   <0.001  97.7 3.249   4.27
#>  virginica           1.0     1.91      0.375  5.08   <0.001  19.7 1.165   2.65
#>  virginica           6.9     3.29      0.119 27.63   <0.001 195.0 3.055   3.53
#>   Df
#>  144
#>  144
#>  144
#>  144
#>  144
#>  144
#> 
#> Type: response
#> 
get_marginalmeans(model, by = c("Species", "Petal.Length = c(1, 3, 5)"), length = 2)
#> 
#>     Species Petal.Length Estimate Std. Error     t Pr(>|t|)     S 2.5 % 97.5 %
#>  setosa                1     3.25     0.1282 25.33   <0.001 180.0 2.995   3.50
#>  setosa                3     4.02     0.4026 10.00   <0.001  58.0 3.229   4.82
#>  setosa                5     4.80     0.9216  5.21   <0.001  20.6 2.979   6.62
#>  versicolor            1     1.55     0.3166  4.89   <0.001  18.5 0.924   2.18
#>  versicolor            3     2.30     0.1291 17.80   <0.001 124.5 2.043   2.55
#>  versicolor            5     3.05     0.0840 36.26   <0.001 244.3 2.881   3.21
#>  virginica             1     1.91     0.3753  5.08   <0.001  19.7 1.165   2.65
#>  virginica             3     2.38     0.2137 11.12   <0.001  67.8 1.954   2.80
#>  virginica             5     2.84     0.0636 44.74   <0.001 284.5 2.719   2.97
#>   Df
#>  144
#>  144
#>  144
#>  144
#>  144
#>  144
#>  144
#>  144
#>  144
#> 
#> Type: response
#> 
# }
model <- lm(Sepal.Width ~ Species * Petal.Length, data = iris)

get_marginaltrends(model, trend = "Petal.Length", by = "Species")
#> 
#>     Species Estimate Std. Error    t Pr(>|t|)    S   2.5 % 97.5 %  Df
#>  setosa        0.388     0.2602 1.49  0.13825  2.9 -0.1264  0.902 144
#>  versicolor    0.374     0.0963 3.89  < 0.001 12.7  0.1839  0.565 144
#>  virginica     0.234     0.0818 2.86  0.00482  7.7  0.0726  0.396 144
#> 
#> Term: Petal.Length
#> Type: response
#> Comparison: dY/dX
#> 
get_marginaltrends(model, trend = "Petal.Length", by = "Petal.Length")
#> 
#>  Petal.Length Estimate Std. Error    t Pr(>|t|)    S 2.5 % 97.5 %  Df
#>          1.00    0.332     0.0964 3.45   <0.001 10.4 0.142  0.523 144
#>          1.66    0.332     0.0963 3.45   <0.001 10.4 0.142  0.523 144
#>          2.31    0.332     0.0966 3.44   <0.001 10.4 0.141  0.523 144
#>          2.97    0.332     0.0966 3.44   <0.001 10.4 0.141  0.523 144
#>          3.62    0.332     0.0963 3.45   <0.001 10.4 0.142  0.523 144
#>          4.28    0.332     0.0963 3.45   <0.001 10.4 0.142  0.523 144
#>          4.93    0.332     0.0965 3.44   <0.001 10.4 0.141  0.523 144
#>          5.59    0.332     0.0963 3.45   <0.001 10.4 0.142  0.523 144
#>          6.24    0.332     0.0963 3.45   <0.001 10.4 0.142  0.522 144
#>          6.90    0.332     0.0966 3.44   <0.001 10.4 0.141  0.523 144
#> 
#> Term: Petal.Length
#> Type: response
#> Comparison: dY/dX
#> 
get_marginaltrends(model, trend = "Petal.Length", by = c("Species", "Petal.Length"))
#> 
#>     Species Petal.Length Estimate Std. Error    t Pr(>|t|)    S   2.5 % 97.5 %
#>  setosa             1.00    0.388     0.2602 1.49  0.13820  2.9 -0.1264  0.902
#>  setosa             1.66    0.388     0.2601 1.49  0.13815  2.9 -0.1263  0.902
#>  versicolor         3.62    0.374     0.0963 3.89  < 0.001 12.7  0.1839  0.565
#>  versicolor         4.28    0.374     0.0963 3.89  < 0.001 12.7  0.1840  0.565
#>  versicolor         4.93    0.374     0.0960 3.90  < 0.001 12.7  0.1846  0.564
#>  virginica          4.93    0.234     0.0819 2.86  0.00482  7.7  0.0726  0.396
#>  virginica          5.59    0.234     0.0819 2.86  0.00484  7.7  0.0725  0.396
#>  virginica          6.24    0.234     0.0819 2.86  0.00482  7.7  0.0726  0.396
#>  virginica          6.90    0.234     0.0819 2.86  0.00482  7.7  0.0726  0.396
#>   Df
#>  144
#>  144
#>  144
#>  144
#>  144
#>  144
#>  144
#>  144
#>  144
#> 
#> Term: Petal.Length
#> Type: response
#> Comparison: dY/dX
#> 
```
