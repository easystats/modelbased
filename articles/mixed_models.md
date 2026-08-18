# Mixed effects models

### Estimated Marginal Means in Mixed Effects Models: Navigating Conditional and Marginal Effects

Mixed models, with their ability to account for hierarchical or
clustered data, offer powerful tools for understanding complex
relationships. A key aspect of interpreting these models is
understanding **estimated marginal means (EMMs)**, which represent the
predicted outcome for specific groups or conditions, while holding other
variables constant. However, calculating EMMs in mixed models is not
always straightforward, and the results can vary depending on the
approach taken.

One crucial distinction is between **conditional** and **marginal**
predictions (or effects). Conditional predictions are specific to a
particular level of the random effect (e.g., the predicted outcome for a
specific individual in a study). Marginal predictions, on the other
hand, average over the random effects, providing an overall estimate of
the effect in the population. This is a crucial difference, as the
marginal effect is often the quantity of interest when we want to
generalize to the population (Heiss 2022).

Based on the definitions from Heiss (2022), we can say that the
conditional vs. marginal distinction applies to any sort of hierarchical
structure (*clusters*) in multilevel models:

- **Conditional effect** = the effect of a variable in an *average
  cluster* (i.e., group-specific, subject-specific or cluster-specific
  effect, or an average or a typical cluster, `re.form = NA`)
- **Marginal effect** = effect of a variable *across clusters on
  average* (i.e., global/population-level effect, or clusters on
  average, `re.form = NULL`)

For `backend = "marginaleffects"`, the `re.form` argument is set to
`NULL` for mixed models by default, to calculate *marginal predictions*.
You can use for instance `re.form = NA` in your
[`estimate_means()`](https://easystats.github.io/modelbased/reference/estimate_means.md)
call to change the default value (`NA` will produce *conditional
predictions*).

As outlined in the [vignette on marginalization
methods](https://easystats.github.io/modelbased/articles/technical_marginalization.html),
marginalization in mixed models involves two distinct dimensions:
marginalizing across *random effects (clusters)* and marginalizing
across *covariates (observations)*.

We can produce *marginal predictions* regarding the higher-level units
(random effects) *conditioned on* a “typical” observation based on a
balanced data grid, or generate predictions that are *fully
marginalized* across both clusters and observations.

The default (`estimate = "typical"`) calculates predictions for a
balanced data grid representing all combinations of focal predictor
levels (specified in `by`), which is particularly useful for comparing
groups. Setting `estimate = "average"` or `estimate = "population"` is
useful when you want to calculate the average expected outcome across
the actual observations from the sample at hand.

Note that for *linear* mixed models, `re.form = NA` will only have an
effect when `estimate = "average"` or `estimate = "population"`.

### Linear mixed models

#### Balanced Data

This section demonstrates the calculation of estimated marginal means
(EMMs) in a linear mixed model using balanced data. We’ll use the
`sleepstudy` dataset from the **lme4** package.

In this example, we fit a linear mixed model predicting `Reaction` based
on `Days`, with random intercepts and slopes for `Subject`. Because the
data is balanced and we have a linear model, results are identical for
all marginalization methods.

[`library`](https://rdrr.io/r/base/library.html)`(`[`modelbased`](https://easystats.github.io/modelbased/)`)`` `[`data`](https://rdrr.io/r/utils/data.html)`(``sleepstudy``, package ``=`` ``"lme4"``)`` ``# for later, create a slightly imbalanced distributed predictor`` `[`set.seed`](https://rdrr.io/r/base/Random.html)`(``1234``)`` ``sleepstudy``$``x`` ``<-`` `[`as.factor`](https://rdrr.io/r/base/factor.html)`(`[`sample.int`](https://rdrr.io/r/base/sample.html)`(``3``, `[`nrow`](https://rdrr.io/r/base/nrow.html)`(``sleepstudy``)``, replace ``=`` ``TRUE``)``)`` `` ``model`` ``<-`` ``lme4``::`[`lmer`](https://rdrr.io/pkg/lme4/man/lmer.html)`(``Reaction`` ``~`` ``Days`` ``+`` ``(``1`` ``+`` ``Days`` ``|`` ``Subject``)``, data ``=`` ``sleepstudy``)`` `` ``` # `estimate = "typical"`, `re.form = NULL`: a "typical" observation ``` ``# across clusters on average`` `[`estimate_means`](https://easystats.github.io/modelbased/reference/estimate_means.md)`(``model``, ``"Days"``)`` ``#> Estimated Marginal Means`` ``#> `` ``#> Days | Mean (CI)`` ``#> ------------------------------`` ``#> 0 | 251.41 (237.94, 264.87)`` ``#> 1 | 261.87 (248.48, 275.27)`` ``#> 2 | 272.34 (258.34, 286.34)`` ``#> 3 | 282.81 (267.60, 298.02)`` ``#> 4 | 293.27 (276.39, 310.16)`` ``#> 5 | 303.74 (284.83, 322.65)`` ``#> 6 | 314.21 (293.03, 335.39)`` ``#> 7 | 324.68 (301.05, 348.31)`` ``#> 8 | 335.14 (308.94, 361.35)`` ``#> 9 | 345.61 (316.74, 374.48)`` ``#> `` ``#> Variable predicted: Reaction`` ``#> Predictors modulated: Days`` ``#> Predictors averaged: Subject`` `` ``` # `estimate = "typical"`, `re.form = NA`: a "typical" observation ``` ``` # for a "typical" (average) cluster - `re.form = NA` has no effect here ``` `[`estimate_means`](https://easystats.github.io/modelbased/reference/estimate_means.md)`(``model``, ``"Days"``, re.form ``=`` ``NA``)`` ``#> Estimated Marginal Means`` ``#> `` ``#> Days | Mean (CI)`` ``#> ------------------------------`` ``#> 0 | 251.41 (237.94, 264.87)`` ``#> 1 | 261.87 (248.48, 275.27)`` ``#> 2 | 272.34 (258.34, 286.34)`` ``#> 3 | 282.81 (267.60, 298.02)`` ``#> 4 | 293.27 (276.39, 310.16)`` ``#> 5 | 303.74 (284.83, 322.65)`` ``#> 6 | 314.21 (293.03, 335.39)`` ``#> 7 | 324.68 (301.05, 348.31)`` ``#> 8 | 335.14 (308.94, 361.35)`` ``#> 9 | 345.61 (316.74, 374.48)`` ``#> `` ``#> Variable predicted: Reaction`` ``#> Predictors modulated: Days`` ``#> Predictors averaged: Subject`` `` ``# fully marginalized predictions, still same result as above`` `[`estimate_means`](https://easystats.github.io/modelbased/reference/estimate_means.md)`(``model``, ``"Days"``, estimate ``=`` ``"average"``)`` ``#> Average Predictions`` ``#> `` ``#> Days | Mean (CI)`` ``#> ------------------------------`` ``#> 0 | 251.41 (237.94, 264.87)`` ``#> 1 | 261.87 (248.48, 275.27)`` ``#> 2 | 272.34 (258.34, 286.34)`` ``#> 3 | 282.81 (267.60, 298.02)`` ``#> 4 | 293.27 (276.39, 310.16)`` ``#> 5 | 303.74 (284.83, 322.65)`` ``#> 6 | 314.21 (293.03, 335.39)`` ``#> 7 | 324.68 (301.05, 348.31)`` ``#> 8 | 335.14 (308.94, 361.35)`` ``#> 9 | 345.61 (316.74, 374.48)`` ``#> `` ``#> Variable predicted: Reaction`` ``#> Predictors modulated: Days`

#### Imbalanced Data

This section explores the impact of imbalanced data on EMM calculations
in linear mixed models. We’ll use the `penguins` dataset, which has
imbalanced groups that we use as higher-level unit, as well as
imbalanced predictors.

Since `estimate = "typical"` creates a balanced data grid, *including*
the levels of the random effects, we get a “typical” observation
marginalized over a *balanced* grid of random effects levels. In this
case, setting `re.form = NA` has no effect when we have *linear* mixed
models.

[`data`](https://rdrr.io/r/utils/data.html)`(``penguins``)`` ``penguins``$``body_mass`` ``<-`` `[`as.factor`](https://rdrr.io/r/base/factor.html)`(``datawizard``::`[`categorize`](https://easystats.github.io/datawizard/reference/categorize.html)`(``penguins``$``body_mass``)``)`` ``model`` ``<-`` ``lme4``::`[`lmer`](https://rdrr.io/pkg/lme4/man/lmer.html)`(``bill_len`` ``~`` ``sex`` ``+`` ``body_mass`` ``+`` ``(``1`` ``|`` ``species``)``, data ``=`` ``penguins``)`` `` ``# conditional predictions marginalized over`` ``# a "balanced" grid of random effects`` `[`estimate_means`](https://easystats.github.io/modelbased/reference/estimate_means.md)`(``model``, ``"sex"``)`` ``#> Estimated Marginal Means`` ``#> `` ``#> sex | Mean (CI)`` ``#> -----------------------------`` ``#> female | 43.34 (37.28, 49.39)`` ``#> male | 46.81 (40.75, 52.86)`` ``#> `` ``#> Variable predicted: bill_len`` ``#> Predictors modulated: sex`` ``#> Predictors averaged: body_mass, species`` `` ``# (fully) marginal predictions`` `[`estimate_means`](https://easystats.github.io/modelbased/reference/estimate_means.md)`(``model``, ``"sex"``, estimate ``=`` ``"average"``)`` ``#> Average Predictions`` ``#> `` ``#> sex | Mean (CI)`` ``#> -----------------------------`` ``#> female | 42.10 (36.04, 48.15)`` ``#> male | 45.85 (39.80, 51.91)`` ``#> `` ``#> Variable predicted: bill_len`` ``#> Predictors modulated: sex`` `` ``# marginal predictions conditioning on a "typical" cluster`` `[`estimate_means`](https://easystats.github.io/modelbased/reference/estimate_means.md)`(``model``, ``"sex"``, estimate ``=`` ``"average"``, re.form ``=`` ``NA``)`` ``#> Average Predictions`` ``#> `` ``#> sex | Mean (CI)`` ``#> -----------------------------`` ``#> female | 43.22 (37.17, 49.28)`` ``#> male | 46.92 (40.87, 52.98)`` ``#> `` ``#> Variable predicted: bill_len`` ``#> Predictors modulated: sex`

#### Excourse: Technical Details

To better understand how these predictions are computed under the hood,
let’s break down the underlying workflow. The code below illustrates the
manual step-by-step procedures for grid-based versus sample-averaged
predictions.

Technically, `estimate_means(model, "sex")` is equivalent to

1.  creating a balanced data grid of all combinations of predictor
    levels including random effect levels
2.  predicting the outcome for the artificial observations represented
    by that data grid
3.  followed by averaging the predictions by the levels of the focal
    predictors.

`dg`` ``<-`` ``insight``::`[`get_datagrid`](https://easystats.github.io/insight/reference/get_datagrid.html)`(``model``, ``"sex"``, factors ``=`` ``"all"``, include_random ``=`` ``TRUE``)`` ``out`` ``<-`` `[`cbind`](https://rdrr.io/r/base/cbind.html)`(``predicted ``=`` `[`predict`](https://rdrr.io/r/stats/predict.html)`(``model``, newdata ``=`` ``dg``)``, ``dg``)`` `[`aggregate`](https://rdrr.io/r/stats/aggregate.html)`(``out``$``predicted``, `[`list`](https://rdrr.io/r/base/list.html)`(``out``$``sex``)``, ``mean``)`` ``#> Group.1 x`` ``#> 1 female 43`` ``#> 2 male 47`

`estimate_means(model, "sex", estimate = "average")` is equivalent to

1.  predicting the outcome for each observation in the data
2.  followed by averaging the predictions by the levels of the focal
    predictors.

`out`` ``<-`` `[`cbind`](https://rdrr.io/r/base/cbind.html)`(``predicted ``=`` `[`predict`](https://rdrr.io/r/stats/predict.html)`(``model``)``, ``insight``::`[`get_data`](https://easystats.github.io/insight/reference/get_data.html)`(``model``)``)`` `[`aggregate`](https://rdrr.io/r/stats/aggregate.html)`(``out``$``predicted``, `[`list`](https://rdrr.io/r/base/list.html)`(``out``$``sex``)``, ``mean``)`` ``#> Group.1 x`` ``#> 1 female 42`` ``#> 2 male 46`

### Generalized linear mixed models

The generalized linear mixed model (GLMM) example, using a Poisson
distribution to model fish counts, demonstrates the substantial
differences between marginal and conditional predictions in non-linear
models. When examining the effect of the `camper` variable, marginal
predictions yield higher estimated means (1.26 for `camper = 0` and 3.21
for `camper = 1`) compared to conditional predictions (using
`re.form = NA`), which are lower (0.66 for `camper = 0` and 1.68 for
`camper = 1`).

[`data`](https://rdrr.io/r/utils/data.html)`(``"fish"``, package ``=`` ``"insight"``)`` ``model`` ``<-`` ``lme4``::`[`glmer`](https://rdrr.io/pkg/lme4/man/glmer.html)`(`` `` ``count`` ``~`` ``child`` ``+`` ``camper`` ``+`` ``(``1`` ``|`` ``persons``)``,`` `` data ``=`` ``fish``,`` `` family ``=`` `[`poisson`](https://rdrr.io/r/stats/family.html)`(``)`` ``)`` `` ``# conditional predictions marginalized over`` ``# a "balanced" grid of random effects`` `[`estimate_means`](https://easystats.github.io/modelbased/reference/estimate_means.md)`(``model``, ``"camper"``)`` ``#> Estimated Marginal Means`` ``#> `` ``#> camper | Mean (CI)`` ``#> ---------------------------`` ``#> 0 | 1.26 (-0.28, 2.79)`` ``#> 1 | 3.21 (-0.68, 7.10)`` ``#> `` ``#> Variable predicted: count`` ``#> Predictors modulated: camper`` ``#> Predictors averaged: child (0.68), persons (1)`` ``#> Predictions are on the response-scale.`` `` ``# conditional predictions for a "typical" cluster`` `[`estimate_means`](https://easystats.github.io/modelbased/reference/estimate_means.md)`(``model``, ``"camper"``, re.form ``=`` ``NA``)`` ``#> Estimated Marginal Means`` ``#> `` ``#> camper | Mean (CI)`` ``#> ---------------------------`` ``#> 0 | 0.66 (-0.14, 1.46)`` ``#> 1 | 1.68 (-0.36, 3.71)`` ``#> `` ``#> Variable predicted: count`` ``#> Predictors modulated: camper`` ``#> Predictors averaged: child (0.68), persons (1)`` ``#> Predictions are on the response-scale.`

When using `estimate = "average"` to average across all observations and
clusters, the differences between marginal and conditional predictions
(regarding random effects) persist, with marginal predictions showing
higher means (1.52 and 4.54) than conditional predictions (1.20 and
3.19), but both results differ from the predictions for “typical”
observations based on the default `estimate` option, as shown in the
previous output.

`# (fully) marginal predictions`` `[`estimate_means`](https://easystats.github.io/modelbased/reference/estimate_means.md)`(``model``, ``"camper"``, estimate ``=`` ``"average"``)`` ``#> Average Predictions`` ``#> `` ``#> camper | Mean (CI)`` ``#> ----------------------------`` ``#> 0 | 1.52 (-0.33, 3.38)`` ``#> 1 | 4.54 (-0.95, 10.03)`` ``#> `` ``#> Variable predicted: count`` ``#> Predictors modulated: camper`` ``#> Predictions are on the response-scale.`` `` ``# marginal predictions, conditioning on a "typical" cluster`` `[`estimate_means`](https://easystats.github.io/modelbased/reference/estimate_means.md)`(``model``, ``"camper"``, estimate ``=`` ``"average"``, re.form ``=`` ``NA``)`` ``#> Average Predictions`` ``#> `` ``#> camper | Mean (CI)`` ``#> ---------------------------`` ``#> 0 | 1.20 (-0.26, 2.66)`` ``#> 1 | 3.19 (-0.67, 7.04)`` ``#> `` ``#> Variable predicted: count`` ``#> Predictors modulated: camper`` ``#> Predictions are on the response-scale.`

This highlights that in GLMMs, the choice between marginal and
conditional predictions significantly impacts the estimated means and,
consequently, the interpretation of the model’s results.

### Conclusion

This vignette has demonstrated the nuances of calculating estimated
marginal means (EMMs) in mixed models, highlighting the critical
distinction between conditional and marginal predictions. While
conditional predictions focus on specific levels of random effects,
marginal predictions average over them, providing a population-level
perspective. We’ve seen that in linear mixed models with balanced data,
these two approaches often yield similar results. However, in
generalized linear mixed models (GLMMs), the differences between
conditional and marginal predictions can be substantial.

Furthermore, the default approach of calculating predictions for a
balanced data grid, while useful for comparing groups, does not
necessarily reflect the actual distribution of observations in the
sample. For imbalanced data, this can yield significantly different
results even in linear mixed models in comparison to average predictions
(using `estimate = "average"`).

**Therefore, we recommend combining marginal predictions (using the
default `backend = "marginaleffects"`) with averaging across all
observations (by setting `estimate = "average"` or
`estimate = "population"`) when working with mixed models.** This
approach effectively incorporates the variation inherent in the random
effects (higher-level units) and provides EMMs that more accurately
reflect the overall patterns observed within the actual sample data. By
averaging over the observed data, we obtain a more robust and
representative estimate of the population-level effects, making it a
preferred strategy for interpreting mixed model results.

## References

Heiss, Andrew. 2022. “Marginal and Conditional Effects for GLMMs with
{Marginaleffects}.” In *Andrew Heiss’s Blog*.
<https://doi.org/10.59350/xwnfm-x1827>.
