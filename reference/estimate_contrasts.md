# Estimate Marginal Contrasts

Run a contrast analysis by estimating the differences between each level
of a factor. See also other related functions such as
[`estimate_means()`](https://easystats.github.io/modelbased/reference/estimate_means.md)
and
[`estimate_slopes()`](https://easystats.github.io/modelbased/reference/estimate_slopes.md).

## Usage

``` r
estimate_contrasts(model, ...)

# Default S3 method
estimate_contrasts(
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
  es_type = NULL,
  backend = NULL,
  verbose = TRUE,
  ...
)
```

## Arguments

- model:

  A statistical model.

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

  - Especially for `estimate_contrasts()` with integer focal predictors,
    for which contrasts should be calculated, use argument
    `integer_as_continuous` to set the maximum number of unique values
    in an integer predictor to treat that predictor as "discrete
    integer" or as numeric. For the first case, contrasts are calculated
    between values of the predictor, for the latter, contrasts of slopes
    are calculated. If the integer has more than `integer_as_continuous`
    unique values, it is treated as numeric. Defaults to `5`. Set to
    `TRUE` to always treat integer predictors as continuous.

  - For count regression models that use an offset term, use
    `offset = <value>` to fix the offset at a specific value. Or use
    `estimate = "average"`, to average predictions over the distribution
    of the offset (if appropriate).

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

- ci:

  Confidence Interval (CI) level. Default to `0.95` (`95%`).

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

- p_adjust:

  The p-values adjustment method for frequentist multiple comparisons.
  Can be one of `"none"` (default), `"hochberg"`, `"hommel"`,
  `"bonferroni"`, `"BH"`, `"BY"`, `"fdr"`, `"tukey"`, `"sidak"`,
  `"sup-t"`, `"esarey"` or `"holm"`. The `"esarey"` option is
  specifically for the case of Johnson-Neyman intervals, i.e. when
  calling
  [`estimate_slopes()`](https://easystats.github.io/modelbased/reference/estimate_slopes.md)
  with two numeric predictors in an interaction term. `"sup-t"` computes
  simultaneous confidence bands, also called sup-t confidence band
  (Montiel Olea & Plagborg-Møller, 2019). Details for the other options
  can be found in the p-value adjustment section of the
  [`emmeans::test`](https://rvlenth.github.io/emmeans/reference/summary.emmGrid.html)
  documentation or
  [`?stats::p.adjust`](https://rdrr.io/r/stats/p.adjust.html). Note that
  certain options provided by the **emmeans** package are only available
  if you set `backend = "emmeans"`.

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

- keep_iterations:

  If `TRUE`, will keep all iterations (draws) of bootstrapped or
  Bayesian models. They will be added as additional columns named
  `iter_1`, `iter_2`, and so on. If `keep_iterations` is a positive
  number, only as many columns as indicated in `keep_iterations` will be
  added to the output. You can reshape them to a long format by running
  [`bayestestR::reshape_iterations()`](https://easystats.github.io/bayestestR/reference/reshape_iterations.html).

- effectsize:

  Desired measure of standardized effect size, one of `"emmeans"`,
  `"marginal"`, or `"boot"`. Default is `NULL`, i.e. no effect size will
  be computed.

- iterations:

  The number of bootstrap resamples to perform.

- es_type:

  Specifies the type of effect-size measure to estimate when using
  `effectsize = "boot"`. One of `"unstandardized"`, `"cohens.d"`,
  `"hedges.g"`, `"cohens.d.sigma"`, `"r"`, or `"akp.robust.d"`. See
  `effect.type` argument of
  [`bootES::bootES()`](https://rdrr.io/pkg/bootES/man/bootES.html) for
  details. If not specified, defaults to `"cohens.d"`.

- backend:

  Whether to use `"marginaleffects"` (default) or `"emmeans"` as a
  backend. Results are usually very similar. The major difference will
  be found for mixed models, where `backend = "marginaleffects"` will
  also average across random effects levels, producing "marginal
  predictions" (instead of "conditional predictions", see Heiss 2022).

  Another difference is that `backend = "marginaleffects"` will be
  slower than `backend = "emmeans"`. For most models, this difference is
  negligible. However, in particular complex models or large data sets
  fitted with *glmmTMB* can be significantly slower.

  You can set a default backend via
  [`options()`](https://rdrr.io/r/base/options.html), e.g. use
  `options(modelbased_backend = "emmeans")` to use the **emmeans**
  package or `options(modelbased_backend = "marginaleffects")` to set
  **marginaleffects** as default backend.

- verbose:

  Use `FALSE` to silence messages and warnings.

## Value

A data frame of estimated contrasts.

## Details

The
[`estimate_slopes()`](https://easystats.github.io/modelbased/reference/estimate_slopes.md),
[`estimate_means()`](https://easystats.github.io/modelbased/reference/estimate_means.md)
and `estimate_contrasts()` functions are forming a group, as they are
all based on *marginal* estimations (estimations based on a model). All
three are built on the **emmeans** or **marginaleffects** package
(depending on the `backend` argument), so reading its documentation (for
instance
[`emmeans::emmeans()`](https://rvlenth.github.io/emmeans/reference/emmeans.html),
[`emmeans::emtrends()`](https://rvlenth.github.io/emmeans/reference/emtrends.html)
or this [website](https://marginaleffects.com/)) is recommended to
understand the idea behind these types of procedures.

- Model-based **predictions** is the basis for all that follows. Indeed,
  the first thing to understand is how models can be used to make
  predictions (see
  [`estimate_relation()`](https://easystats.github.io/modelbased/reference/estimate_expectation.md)).
  This corresponds to the predicted response (or "outcome variable")
  given specific predictor values of the predictors (i.e., given a
  specific data configuration). This is why the concept of the
  [reference
  grid](https://easystats.github.io/insight/reference/get_datagrid.html)
  is so important for direct predictions.

- **Marginal "means"**, obtained via
  [`estimate_means()`](https://easystats.github.io/modelbased/reference/estimate_means.md),
  are an extension of such predictions, allowing to "average" (collapse)
  some of the predictors, to obtain the average response value at a
  specific predictors configuration. This is typically used when some of
  the predictors of interest are factors. Indeed, the parameters of the
  model will usually give you the intercept value and then the "effect"
  of each factor level (how different it is from the intercept).
  Marginal means can be used to directly give you the mean value of the
  response variable at all the levels of a factor. Moreover, it can also
  be used to control, or average over predictors, which is useful in the
  case of multiple predictors with or without interactions.

- **Marginal contrasts**, obtained via `estimate_contrasts()`, are
  themselves at extension of marginal means, in that they allow to
  investigate the difference (i.e., the contrast) between the marginal
  means. This is, again, often used to get all pairwise differences
  between all levels of a factor. It works also for continuous
  predictors, for instance one could also be interested in whether the
  difference at two extremes of a continuous predictor is significant.

- Finally, **marginal effects**, obtained via
  [`estimate_slopes()`](https://easystats.github.io/modelbased/reference/estimate_slopes.md),
  are different in that their focus is not values on the response
  variable, but the model's parameters. The idea is to assess the effect
  of a predictor at a specific configuration of the other predictors.
  This is relevant in the case of interactions or non-linear
  relationships, when the effect of a predictor variable changes
  depending on the other predictors. Moreover, these effects can also be
  "averaged" over other predictors, to get for instance the "general
  trend" of a predictor over different factor levels.

**Example:** Let's imagine the following model `lm(y ~ condition * x)`
where `condition` is a factor with 3 levels A, B and C and `x` a
continuous variable (like age for example). One idea is to see how this
model performs, and compare the actual response y to the one predicted
by the model (using
[`estimate_expectation()`](https://easystats.github.io/modelbased/reference/estimate_expectation.md)).
Another idea is evaluate the average mean at each of the condition's
levels (using
[`estimate_means()`](https://easystats.github.io/modelbased/reference/estimate_means.md)),
which can be useful to visualize them. Another possibility is to
evaluate the difference between these levels (using
`estimate_contrasts()`). Finally, one could also estimate the effect of
x averaged over all conditions, or instead within each condition (using
[`estimate_slopes()`](https://easystats.github.io/modelbased/reference/estimate_slopes.md)).

## Comparison options

- `comparison = "pairwise"`: This method computes all possible unique
  differences between pairs of levels of the focal predictor. For
  example, if a factor has levels A, B, and C, it would compute A-B,
  A-C, and B-C.

- `comparison = "reference"`: This compares each level of the focal
  predictor to a specified reference level (by default, the first
  level). For example, if levels are A, B, C, and A is the reference, it
  computes B-A and C-A.

- `comparison = "sequential"`: This compares each level to the one
  immediately following it in the factor's order. For levels A, B, C, it
  would compute B-A and C-B.

- `comparison = "meandev"`: This contrasts each level's estimate against
  the grand mean of all estimates for the focal predictor.

- `comparison = "meanotherdev"`: Similar to `meandev`, but each level's
  estimate is compared against the mean of all *other* levels, excluding
  itself.

- `comparison = "poly"`: These are used for ordered categorical
  variables to test for linear, quadratic, cubic, etc., trends across
  the levels. They assume equal spacing between levels.

- `comparison = "helmert"`: Contrast 2nd level to the first, 3rd to the
  average of the first two, and so on. Each level (except the first) is
  compared to the mean of the preceding levels. For levels A, B, C, it
  would compute B-A and C-(A+B)/2.

- `comparison = "trt_vs_ctrl"`: This compares all levels (excluding the
  first, which is typically the control) against the first level. It's
  often used when comparing multiple treatment groups to a single
  control group.

- To test multiple hypotheses jointly (usually used for factorial
  designs), `comparison` can also be `"joint"`. In this case, use the
  `test` argument to specify which test should be conducted: `"F"`
  (default) or `"Chi2"`.

- `comparison = "inequality"` computes the *absolute inequality* of
  groups, or in other words, the marginal effect inequality summary of
  categorical predictors' overall effects, respectively, the
  comprehensive effect of an independent variable across all outcome
  categories of a nominal or ordinal dependent variable (total marginal
  effect, see *Mize and Han, 2025*). The marginal effect inequality
  focuses on the heterogeneity of the effects of a categorical
  *independent* variable. It helps understand how the effect of the
  variable differs across its categories or levels. When the *dependent*
  variable is categorical (e.g., logistic, ordinal or multinomial
  regression), marginal effect inequality provides a holistic view of
  how an independent variable affects a nominal or ordinal *dependent*
  variable. It summarizes the overall impact (absolute inequality, or
  total marginal effects) across all possible outcome categories.

- `comparison = "inequality_ratio"` is comparable to
  `comparison = "inequality"`, but instead of calculating the absolute
  inequality, it computes the *relative inequality* of groups. This is
  useful to compare the relative effects of different predictors on the
  dependent variable. It provides a measure of how much more or less
  inequality one predictor has compared to another.

- `comparison = "inequality_pairwise"` computes pairwise differences of
  absolute inequality measures, while `"inequality_ratio_pairwise"`
  computes pairwise differences of relative inequality measures
  (ratios). Depending on the sign, this measure indicates which of the
  predictors has a stronger impact on the dependent variable in terms of
  inequalities.

Examples for analysing inequalities are shown in the related
[vignette](https://easystats.github.io/modelbased/articles/practical_inequalities.html).

## Effect Size

By default, `estimate_contrasts()` reports no standardized effect size
on purpose. Should one request one, some things are to keep in mind. As
the authors of *emmeans* write, "There is substantial disagreement among
practitioners on what is the appropriate sigma to use in computing
effect sizes; or, indeed, whether any effect-size measure is appropriate
for some situations. The user is completely responsible for specifying
appropriate parameters (or for failing to do so)."

In particular, effect size method `"boot"` does not correct for
covariates in the model, so should probably only be used when there is
just one categorical predictor (with however many levels). Some believe
that if there are multiple predictors or any covariates, it is important
to re-compute sigma adding back in the response variance associated with
the variables that aren't part of the contrast.

`effectsize = "emmeans"` uses
[emmeans::eff_size](https://rvlenth.github.io/emmeans/reference/eff_size.html)
with `sigma = stats::sigma(model)`, `edf = stats::df.residual(model)`
and `method = "identity"`. This standardizes using the MSE (sigma). Some
believe this works when the contrasts are the only predictors in the
model, but not when there are covariates. The response variance
accounted for by the covariates should not be removed from the SD used
to standardize. Otherwise, *d* will be overestimated.

`effectsize = "marginal"` uses the following formula to compute effect
size: `d_adj <- difference * (1- R2)/ sigma`. This standardizes using
the response SD with only the between-groups variance on the focal
factor/contrast removed. This allows for groups to be equated on their
covariates, but creates an appropriate scale for standardizing the
response.

`effectsize = "boot"` uses bootstrapping (defaults to a low value of
200) through
[bootES::bootES](https://rdrr.io/pkg/bootES/man/bootES.html). Adjusts
for contrasts, but not for covariates.

## Predictions and contrasts at meaningful values (data grids)

To define representative values for focal predictors (specified in `by`,
`contrast`, and `trend`), you can use several methods. These values are
internally generated by
[`insight::get_datagrid()`](https://easystats.github.io/insight/reference/get_datagrid.html),
so consult its documentation for more details.

- You can directly specify values as strings or lists for `by`,
  `contrast`, and `trend`.

  - For numeric focal predictors, use examples like
    `by = "gear = c(4, 8)"`, `by = list(gear = c(4, 8))` or
    `by = "gear = 5:10"`

  - For factor or character predictors, use
    `by = "Species = c('setosa', 'virginica')"` or
    `by = list(Species = c('setosa', 'virginica'))`

- You can use "shortcuts" within square brackets, such as
  `by = "Sepal.Width = [sd]"` or `by = "Sepal.Width = [fivenum]"`

- For numeric focal predictors, if no representative values are
  specified (i.e., `by = "gear"` and *not* `by = "gear = c(4, 8)"`),
  `length` and `range` control the number and type of representative
  values for the focal predictors:

  - `length` determines how many equally spaced values are generated.

  - `range` specifies the type of values, like `"range"` or `"sd"`.

  - `length` and `range` apply to all numeric focal predictors.

  - If you have multiple numeric predictors, `length` and `range` can
    accept multiple elements, one for each predictor (see 'Examples').

- For integer variables, only values that appear in the data will be
  included in the data grid, independent from the `length` argument.
  This behaviour can be changed by setting `protect_integers = FALSE`,
  which will then treat integer variables as numerics (and possibly
  produce fractions).

See also [this
vignette](https://easystats.github.io/modelbased/articles/visualisation_matrix.html)
for some examples.

## Predictions on different scales

The `predict` argument allows to generate predictions on different
scales of the response variable. The `"link"` option does not apply to
all models, and usually not to Gaussian models. `"link"` will leave the
values on scale of the linear predictors. `"response"` (or `NULL`) will
transform them on scale of the response variable. Thus for a logistic
model, `"link"` will give estimations expressed in log-odds
(probabilities on logit scale) and `"response"` in terms of
probabilities.

To predict distributional parameters (called "dpar" in other packages),
for instance when using complex formulae in `brms` models, the `predict`
argument can take the value of the parameter you want to estimate, for
instance `"sigma"`, `"kappa"`, etc.

`"response"` and `"inverse_link"` both return predictions on the
response scale, however, `"response"` first calculates predictions on
the response scale for each observation and *then* aggregates them by
groups or levels defined in `by`. `"inverse_link"` first calculates
predictions on the link scale for each observation, then aggregates them
by groups or levels defined in `by`, and finally back-transforms the
predictions to the response scale. Both approaches have advantages and
disadvantages. `"response"` usually produces less biased predictions,
but confidence intervals might be outside reasonable bounds (i.e., for
instance can be negative for count data). The `"inverse_link"` approach
is more robust in terms of confidence intervals, but might produce
biased predictions. However, you can try to set
`bias_correction = TRUE`, to adjust for this bias.

In particular for mixed models, using `"response"` is recommended,
because averaging across random effects groups is then more accurate.

## References

- Mize, T., & Han, B. (2025). Inequality and Total Effect Summary
  Measures for Nominal and Ordinal Variables. Sociological Science, 12,
  115–157. [doi:10.15195/v12.a7](https://doi.org/10.15195/v12.a7)

- Montiel Olea, J. L., and Plagborg-Møller, M. (2019). Simultaneous
  confidence bands: Theory, implementation, and an application to SVARs.
  Journal of Applied Econometrics, 34(1), 1–17.
  [doi:10.1002/jae.2656](https://doi.org/10.1002/jae.2656)

## Examples

``` r
# \dontrun{
# Basic usage --------------------------------
# --------------------------------------------

model <- lm(Sepal.Width ~ Species, data = iris)
estimate_contrasts(model)
#> We selected `contrast=c("Species")`.
#> Marginal Contrasts Analysis
#> 
#> Level1     | Level2     | Difference |   SE |         95% CI | t(147) |      p
#> ------------------------------------------------------------------------------
#> versicolor | setosa     |      -0.66 | 0.07 | [-0.79, -0.52] |  -9.69 | < .001
#> virginica  | setosa     |      -0.45 | 0.07 | [-0.59, -0.32] |  -6.68 | < .001
#> virginica  | versicolor |       0.20 | 0.07 | [ 0.07,  0.34] |   3.00 |  0.003
#> 
#> Variable predicted: Sepal.Width
#> Predictors contrasted: Species
#> p-values are uncorrected.
#> 

# Dealing with interactions
model <- lm(Sepal.Width ~ Species * Petal.Width, data = iris)

# By default: selects first factor
estimate_contrasts(model)
#> We selected `contrast=c("Species")`.
#> Marginal Contrasts Analysis
#> 
#> Level1     | Level2     | Difference |   SE |         95% CI | t(144) |      p
#> ------------------------------------------------------------------------------
#> versicolor | setosa     |      -1.59 | 0.39 | [-2.37, -0.81] |  -4.04 | < .001
#> virginica  | setosa     |      -1.77 | 0.41 | [-2.59, -0.96] |  -4.29 | < .001
#> virginica  | versicolor |      -0.18 | 0.15 | [-0.47,  0.10] |  -1.27 |  0.205
#> 
#> Variable predicted: Sepal.Width
#> Predictors contrasted: Species
#> Predictors averaged: Petal.Width (1.2)
#> p-values are uncorrected.
#> 

# Can also run contrasts between points of numeric, stratified by "Species"
estimate_contrasts(model, contrast = "Petal.Width", by = "Species")
#> Marginal Contrasts Analysis
#> 
#> Level1     | Level2     | Difference |   SE |        95% CI | t(144) |     p
#> ----------------------------------------------------------------------------
#> versicolor | setosa     |       0.22 | 0.46 | [-0.69, 1.13] |   0.47 | 0.639
#> virginica  | setosa     |      -0.21 | 0.44 | [-1.07, 0.66] |  -0.47 | 0.638
#> virginica  | versicolor |      -0.42 | 0.27 | [-0.95, 0.11] |  -1.58 | 0.116
#> 
#> Variable predicted: Sepal.Width
#> Predictors contrasted: Petal.Width
#> Predictors averaged: Petal.Width (1.2)
#> p-values are uncorrected.
#> 

# Or both
estimate_contrasts(model, contrast = c("Species", "Petal.Width"), length = 2)
#> Marginal Contrasts Analysis
#> 
#> Level1          | Level2          | Difference |   SE |         95% CI | t(144) |      p
#> ----------------------------------------------------------------------------------------
#> setosa, 2.5     | setosa, 0.1     |       2.01 | 0.98 | [ 0.08,  3.94] |   2.06 |  0.041
#> versicolor, 0.1 | setosa, 0.1     |      -1.83 | 0.28 | [-2.38, -1.28] |  -6.55 | < .001
#> versicolor, 2.5 | setosa, 0.1     |       0.70 | 0.27 | [ 0.17,  1.23] |   2.61 |  0.010
#> virginica, 0.1  | setosa, 0.1     |      -1.55 | 0.31 | [-2.17, -0.93] |  -4.95 | < .001
#> virginica, 2.5  | setosa, 0.1     |      -0.03 | 0.11 | [-0.25,  0.19] |  -0.29 |  0.773
#> versicolor, 0.1 | setosa, 2.5     |      -3.84 | 0.96 | [-5.73, -1.95] |  -4.01 | < .001
#> versicolor, 2.5 | setosa, 2.5     |      -1.31 | 0.95 | [-3.19,  0.58] |  -1.37 |  0.172
#> virginica, 0.1  | setosa, 2.5     |      -3.56 | 0.97 | [-5.47, -1.65] |  -3.68 | < .001
#> virginica, 2.5  | setosa, 2.5     |      -2.04 | 0.92 | [-3.86, -0.22] |  -2.21 |  0.028
#> versicolor, 2.5 | versicolor, 0.1 |       2.53 | 0.52 | [ 1.50,  3.56] |   4.86 | < .001
#> virginica, 0.1  | versicolor, 0.1 |       0.28 | 0.41 | [-0.52,  1.08] |   0.69 |  0.492
#> virginica, 2.5  | versicolor, 0.1 |       1.80 | 0.28 | [ 1.24,  2.35] |   6.35 | < .001
#> virginica, 0.1  | versicolor, 2.5 |      -2.25 | 0.40 | [-3.04, -1.46] |  -5.64 | < .001
#> virginica, 2.5  | versicolor, 2.5 |      -0.73 | 0.27 | [-1.27, -0.20] |  -2.70 |  0.008
#> virginica, 2.5  | virginica, 0.1  |       1.52 | 0.37 | [ 0.77,  2.26] |   4.04 | < .001
#> 
#> Variable predicted: Sepal.Width
#> Predictors contrasted: Species, Petal.Width
#> p-values are uncorrected.
#> 

# Or with custom specifications
estimate_contrasts(model, contrast = c("Species", "Petal.Width = c(1, 2)"))
#> Marginal Contrasts Analysis
#> 
#> Level1        | Level2        | Difference |   SE |         95% CI | t(144) |      p
#> ------------------------------------------------------------------------------------
#> setosa, 2     | setosa, 1     |       0.84 | 0.41 | [ 0.03,  1.64] |   2.06 |  0.041
#> versicolor, 1 | setosa, 1     |      -1.63 | 0.32 | [-2.27, -1.00] |  -5.09 | < .001
#> versicolor, 2 | setosa, 1     |      -0.58 | 0.35 | [-1.26,  0.10] |  -1.68 |  0.096
#> virginica, 1  | setosa, 1     |      -1.73 | 0.35 | [-2.43, -1.04] |  -4.93 | < .001
#> virginica, 2  | setosa, 1     |      -1.10 | 0.31 | [-1.72, -0.48] |  -3.52 | < .001
#> versicolor, 1 | setosa, 2     |      -2.47 | 0.72 | [-3.89, -1.05] |  -3.43 | < .001
#> versicolor, 2 | setosa, 2     |      -1.42 | 0.73 | [-2.86,  0.03] |  -1.94 |  0.055
#> virginica, 1  | setosa, 2     |      -2.57 | 0.73 | [-4.02, -1.12] |  -3.50 | < .001
#> virginica, 2  | setosa, 2     |      -1.94 | 0.72 | [-3.35, -0.52] |  -2.71 |  0.008
#> versicolor, 2 | versicolor, 1 |       1.05 | 0.22 | [ 0.62,  1.48] |   4.86 | < .001
#> virginica, 1  | versicolor, 1 |      -0.10 | 0.19 | [-0.47,  0.27] |  -0.54 |  0.589
#> virginica, 2  | versicolor, 1 |       0.53 | 0.09 | [ 0.35,  0.71] |   5.72 | < .001
#> virginica, 1  | versicolor, 2 |      -1.15 | 0.23 | [-1.60, -0.71] |  -5.13 | < .001
#> virginica, 2  | versicolor, 2 |      -0.52 | 0.16 | [-0.84, -0.21] |  -3.31 |  0.001
#> virginica, 2  | virginica, 1  |       0.63 | 0.16 | [ 0.32,  0.94] |   4.04 | < .001
#> 
#> Variable predicted: Sepal.Width
#> Predictors contrasted: Species, Petal.Width = c(1, 2)
#> p-values are uncorrected.
#> 

# Or modulate it
estimate_contrasts(model, by = "Petal.Width", length = 4)
#> We selected `contrast=c("Species")`.
#> Marginal Contrasts Analysis
#> 
#> Level1     | Level2     | Petal.Width | Difference |   SE |         95% CI
#> --------------------------------------------------------------------------
#> versicolor | setosa     |        0.10 |      -1.83 | 0.28 | [-2.38, -1.28]
#> virginica  | setosa     |        0.10 |      -1.55 | 0.31 | [-2.17, -0.93]
#> virginica  | versicolor |        0.10 |       0.28 | 0.41 | [-0.52,  1.08]
#> versicolor | setosa     |        0.90 |      -1.65 | 0.29 | [-2.22, -1.08]
#> virginica  | setosa     |        0.90 |      -1.71 | 0.32 | [-2.35, -1.07]
#> virginica  | versicolor |        0.90 |      -0.06 | 0.21 | [-0.47,  0.35]
#> versicolor | setosa     |        1.70 |      -1.48 | 0.60 | [-2.67, -0.29]
#> virginica  | setosa     |        1.70 |      -1.88 | 0.60 | [-3.06, -0.70]
#> virginica  | versicolor |        1.70 |      -0.40 | 0.11 | [-0.62, -0.17]
#> versicolor | setosa     |        2.50 |      -1.31 | 0.95 | [-3.19,  0.58]
#> virginica  | setosa     |        2.50 |      -2.04 | 0.92 | [-3.86, -0.22]
#> virginica  | versicolor |        2.50 |      -0.73 | 0.27 | [-1.27, -0.20]
#> 
#> Level1     | t(144) |      p
#> ----------------------------
#> versicolor |  -6.55 | < .001
#> virginica  |  -4.95 | < .001
#> virginica  |   0.69 |  0.492
#> versicolor |  -5.74 | < .001
#> virginica  |  -5.28 | < .001
#> virginica  |  -0.28 |  0.780
#> versicolor |  -2.47 |  0.015
#> virginica  |  -3.14 |  0.002
#> virginica  |  -3.50 | < .001
#> versicolor |  -1.37 |  0.172
#> virginica  |  -2.21 |  0.028
#> virginica  |  -2.70 |  0.008
#> 
#> Variable predicted: Sepal.Width
#> Predictors contrasted: Species
#> p-values are uncorrected.
#> 

# Standardized differences
estimated <- estimate_contrasts(lm(Sepal.Width ~ Species, data = iris))
#> We selected `contrast=c("Species")`.
standardize(estimated)
#> Marginal Contrasts Analysis (standardized)
#> 
#> Level1     | Level2     | Difference |   SE |         95% CI | t(147) |      p
#> ------------------------------------------------------------------------------
#> versicolor | setosa     |      -1.51 | 0.16 | [-1.82, -1.20] |  -9.69 | < .001
#> virginica  | setosa     |      -1.04 | 0.16 | [-1.35, -0.73] |  -6.68 | < .001
#> virginica  | versicolor |       0.47 | 0.16 | [ 0.16,  0.78] |   3.00 |  0.003
#> 
#> Variable predicted: Sepal.Width
#> Predictors contrasted: Species
#> p-values are uncorrected.
#> 

# contrasts of slopes ------------------------
# --------------------------------------------

data(qol_cancer, package = "parameters")
qol_cancer$ID <- as.numeric(qol_cancer$ID)
qol_cancer$grp <- as.factor(ifelse(qol_cancer$ID < 100, "Group 1", "Group 2"))
model <- lm(QoL ~ time * education * grp, data = qol_cancer)

# "time" only has integer values and few values, so it's treated like a factor
estimate_contrasts(model, "time", by = "education")
#> Numeric variable appears to be ordinal or Likert-scale (integer values,
#>   no more than 5 unique values) and is treated as discrete variable. Set
#>   `integer_as_continuous = TRUE` to disable this check and always treat
#>   numeric variables as continuous.
#> Marginal Contrasts Analysis
#> 
#> Level1 | Level2 | education | Difference |   SE |          95% CI | t(552) |     p
#> ----------------------------------------------------------------------------------
#> 2      | 1      | low       |      -1.74 | 2.24 | [ -6.13,  2.66] |  -0.78 | 0.438
#> 3      | 1      | low       |      -3.47 | 4.47 | [-12.26,  5.32] |  -0.78 | 0.438
#> 3      | 2      | low       |      -1.74 | 2.24 | [ -6.13,  2.66] |  -0.78 | 0.438
#> 2      | 1      | mid       |       1.06 | 1.32 | [ -1.54,  3.66] |   0.80 | 0.424
#> 3      | 1      | mid       |       2.12 | 2.65 | [ -3.08,  7.31] |   0.80 | 0.424
#> 3      | 2      | mid       |       1.06 | 1.32 | [ -1.54,  3.66] |   0.80 | 0.424
#> 2      | 1      | high      |       3.02 | 1.95 | [ -0.81,  6.85] |   1.55 | 0.122
#> 3      | 1      | high      |       6.04 | 3.90 | [ -1.62, 13.71] |   1.55 | 0.122
#> 3      | 2      | high      |       3.02 | 1.95 | [ -0.81,  6.85] |   1.55 | 0.122
#> 
#> Variable predicted: QoL
#> Predictors contrasted: time
#> Predictors averaged: grp
#> p-values are uncorrected.
#> 

# we set `integer_as_continuous = TRUE` to treat integer as continuous
estimate_contrasts(model, "time", by = "education", integer_as_continuous = 1)
#> Marginal Contrasts Analysis
#> 
#> Level1 | Level2 | Difference |   SE |         95% CI | t(552) |     p
#> ---------------------------------------------------------------------
#> mid    | low    |       2.79 | 2.60 | [-2.31,  7.90] |   1.07 | 0.283
#> high   | low    |       4.76 | 2.97 | [-1.07, 10.58] |   1.60 | 0.109
#> high   | mid    |       1.96 | 2.36 | [-2.66,  6.59] |   0.83 | 0.405
#> 
#> Variable predicted: QoL
#> Predictors contrasted: time
#> Predictors averaged: time (2), grp
#> p-values are uncorrected.
#> 

# pairwise comparisons for multiple groups
estimate_contrasts(
  model,
  "time",
  by = c("education", "grp"),
  integer_as_continuous = TRUE
)
#> Marginal Contrasts Analysis
#> 
#> Level1        | Level2        | Difference |   SE |          95% CI | t(552) |     p
#> ------------------------------------------------------------------------------------
#> low, Group 2  | low, Group 1  |       9.95 | 4.48 | [  1.16, 18.75] |   2.22 | 0.027
#> mid, Group 1  | low, Group 1  |       7.11 | 3.67 | [ -0.10, 14.33] |   1.94 | 0.053
#> mid, Group 2  | low, Group 1  |       8.43 | 3.68 | [  1.20, 15.66] |   2.29 | 0.022
#> high, Group 1 | low, Group 1  |       6.71 | 4.03 | [ -1.19, 14.62] |   1.67 | 0.096
#> high, Group 2 | low, Group 1  |      12.75 | 4.36 | [  4.19, 21.32] |   2.92 | 0.004
#> mid, Group 1  | low, Group 2  |      -2.84 | 3.67 | [-10.05,  4.37] |  -0.77 | 0.440
#> mid, Group 2  | low, Group 2  |      -1.53 | 3.68 | [ -8.75,  5.70] |  -0.41 | 0.679
#> high, Group 1 | low, Group 2  |      -3.24 | 4.03 | [-11.15,  4.67] |  -0.80 | 0.421
#> high, Group 2 | low, Group 2  |       2.80 | 4.35 | [ -5.74, 11.34] |   0.64 | 0.520
#> mid, Group 2  | mid, Group 1  |       1.32 | 2.64 | [ -3.88,  6.51] |   0.50 | 0.619
#> high, Group 1 | mid, Group 1  |      -0.40 | 3.11 | [ -6.51,  5.71] |  -0.13 | 0.898
#> high, Group 2 | mid, Group 1  |       5.64 | 3.52 | [ -1.28, 12.56] |   1.60 | 0.110
#> high, Group 1 | mid, Group 2  |      -1.72 | 3.12 | [ -7.85,  4.41] |  -0.55 | 0.583
#> high, Group 2 | mid, Group 2  |       4.33 | 3.53 | [ -2.61, 11.26] |   1.22 | 0.221
#> high, Group 2 | high, Group 1 |       6.04 | 3.89 | [ -1.59, 13.68] |   1.55 | 0.121
#> 
#> Variable predicted: QoL
#> Predictors contrasted: time
#> Predictors averaged: time (2)
#> p-values are uncorrected.
#> 

# if we want pairwise comparisons only for one factor, but group by another,
# we need the formula specification and define the grouping variable after
# the vertical bar
estimate_contrasts(
  model,
  "time",
  by = c("education", "grp"),
  comparison = ~pairwise | grp,
  integer_as_continuous = TRUE
)
#> Marginal Contrasts Analysis
#> 
#> Parameter  | grp     | Difference |   SE |         95% CI | t(552) |     p
#> --------------------------------------------------------------------------
#> mid - low  | Group 1 |       7.11 | 3.67 | [-0.10, 14.33] |   1.94 | 0.053
#> high - low | Group 1 |       6.71 | 4.03 | [-1.19, 14.62] |   1.67 | 0.096
#> high - mid | Group 1 |      -0.40 | 3.11 | [-6.51,  5.71] |  -0.13 | 0.898
#> mid - low  | Group 2 |      -1.53 | 3.68 | [-8.75,  5.70] |  -0.41 | 0.679
#> high - low | Group 2 |       2.80 | 4.35 | [-5.74, 11.34] |   0.64 | 0.520
#> high - mid | Group 2 |       4.33 | 3.53 | [-2.61, 11.26] |   1.22 | 0.221
#> 
#> Variable predicted: QoL
#> Predictors contrasted: time
#> Predictors averaged: time (2)
#> p-values are uncorrected.
#> 

# custom factor contrasts - contrasts the average effects of two levels
# against the remaining third level
# ---------------------------------------------------------------------

data(puppy_love, package = "modelbased")
cond_tx <- cbind("no treatment" = c(1, 0, 0), "treatment" = c(0, 0.5, 0.5))
model <- lm(happiness ~ puppy_love * dose, data = puppy_love)
estimate_slopes(model, "puppy_love", by = "dose", comparison = cond_tx)
#> Estimated Marginal Effects
#> 
#> Parameter    | Slope |   SE |        95% CI | t(24) |     p
#> -----------------------------------------------------------
#> no treatment |  0.76 | 0.27 | [ 0.21, 1.31] |  2.86 | 0.009
#> treatment    |  0.30 | 0.22 | [-0.15, 0.75] |  1.37 | 0.184
#> 
#> Marginal effects estimated for puppy_love

# Other models (mixed, Bayesian, ...) --------
# --------------------------------------------
data <- iris
data$Petal.Length_factor <- ifelse(data$Petal.Length < 4.2, "A", "B")

model <- lme4::lmer(Sepal.Width ~ Species + (1 | Petal.Length_factor), data = data)
estimate_contrasts(model)
#> We selected `contrast=c("Species")`.
#> Marginal Contrasts Analysis
#> 
#> Level1     | Level2     | Difference |   SE |         95% CI | t(145) |      p
#> ------------------------------------------------------------------------------
#> versicolor | setosa     |      -0.87 | 0.09 | [-1.04, -0.70] | -10.11 | < .001
#> virginica  | setosa     |      -0.80 | 0.11 | [-1.02, -0.58] |  -7.11 | < .001
#> virginica  | versicolor |       0.07 | 0.07 | [-0.07,  0.22] |   1.00 |  0.319
#> 
#> Variable predicted: Sepal.Width
#> Predictors contrasted: Species
#> Predictors averaged: Petal.Length_factor
#> p-values are uncorrected.
#> 

data <- mtcars
data$cyl <- as.factor(data$cyl)
data$am <- as.factor(data$am)

model <- rstanarm::stan_glm(mpg ~ cyl * wt, data = data, refresh = 0)
estimate_contrasts(model)
#> We selected `contrast=c("cyl")`.
#> Marginal Contrasts Analysis
#> 
#> Level1 | Level2 | Median |         95% CI |     pd |          ROPE | % in ROPE
#> ------------------------------------------------------------------------------
#> 6      | 4      |  -2.20 | [-5.82,  1.47] | 88.67% | [-0.10, 0.10] |     1.82%
#> 8      | 4      |  -4.75 | [-8.44, -1.20] | 99.48% | [-0.10, 0.10] |        0%
#> 8      | 6      |  -2.54 | [-5.53,  0.37] | 95.97% | [-0.10, 0.10] |     0.97%
#> 
#> Variable predicted: mpg
#> Predictors contrasted: cyl
#> Predictors averaged: wt (3.2)
#> 
estimate_contrasts(model, by = "wt", length = 4)
#> We selected `contrast=c("cyl")`.
#> Marginal Contrasts Analysis
#> 
#> Level1 | Level2 |   wt | Median |          95% CI |     pd |          ROPE | % in ROPE
#> --------------------------------------------------------------------------------------
#> 6      | 4      | 1.51 |  -6.02 | [-15.80,  2.93] | 90.80% | [-0.10, 0.10] |     0.92%
#> 8      | 4      | 1.51 |  -9.99 | [-15.40, -4.25] | 99.95% | [-0.10, 0.10] |        0%
#> 8      | 6      | 1.51 |  -3.91 | [-13.89,  6.96] | 77.25% | [-0.10, 0.10] |     1.16%
#> 6      | 4      | 2.82 |  -3.11 | [ -6.60,  0.23] | 96.62% | [-0.10, 0.10] |     0.89%
#> 8      | 4      | 2.82 |  -5.96 | [ -9.46, -2.63] | 99.92% | [-0.10, 0.10] |        0%
#> 8      | 6      | 2.82 |  -2.87 | [ -6.56,  0.87] | 93.80% | [-0.10, 0.10] |     1.63%
#> 6      | 4      | 4.12 |  -0.15 | [ -7.95,  7.48] | 51.65% | [-0.10, 0.10] |     2.11%
#> 8      | 4      | 4.12 |  -1.98 | [ -7.49,  3.27] | 77.00% | [-0.10, 0.10] |     2.37%
#> 8      | 6      | 4.12 |  -1.86 | [ -8.30,  4.28] | 73.10% | [-0.10, 0.10] |     2.68%
#> 6      | 4      | 5.42 |   2.78 | [-12.08, 18.16] | 64.98% | [-0.10, 0.10] |     1.24%
#> 8      | 4      | 5.42 |   2.05 | [ -7.50, 10.97] | 67.27% | [-0.10, 0.10] |     1.95%
#> 8      | 6      | 5.42 |  -0.80 | [-14.84, 12.31] | 54.62% | [-0.10, 0.10] |     1.16%
#> 
#> Variable predicted: mpg
#> Predictors contrasted: cyl
#> 

model <- rstanarm::stan_glm(
  Sepal.Width ~ Species + Petal.Width + Petal.Length,
  data = iris,
  refresh = 0
)
estimate_contrasts(model, by = "Petal.Length = [sd]", test = "bf")
#> We selected `contrast=c("Species")`.
#> Marginal Contrasts Analysis
#> 
#> Level1     | Level2     |   BF | Petal.Length | Median |         95% CI
#> -----------------------------------------------------------------------
#> versicolor | setosa     | 1.00 |         1.99 |  -1.73 | [-2.09, -1.38]
#> virginica  | setosa     | 1.00 |         1.99 |  -2.15 | [-2.68, -1.64]
#> virginica  | versicolor | 1.00 |         1.99 |  -0.42 | [-0.63, -0.21]
#> versicolor | setosa     | 1.00 |         3.76 |  -1.73 | [-2.09, -1.38]
#> virginica  | setosa     | 1.00 |         3.76 |  -2.15 | [-2.68, -1.64]
#> virginica  | versicolor | 1.00 |         3.76 |  -0.42 | [-0.63, -0.21]
#> versicolor | setosa     | 1.00 |         5.52 |  -1.73 | [-2.09, -1.38]
#> virginica  | setosa     | 1.00 |         5.52 |  -2.15 | [-2.68, -1.64]
#> virginica  | versicolor | 1.00 |         5.52 |  -0.42 | [-0.63, -0.21]
#> 
#> Variable predicted: Sepal.Width
#> Predictors contrasted: Species
#> Predictors averaged: Petal.Width (1.2)
#> 
# }
```
