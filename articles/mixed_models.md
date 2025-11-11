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

Based on the definitions from Heiss (2022), we can say:

> This conditional vs. marginal distinction applies to any sort of
> hierarchical structure in multilevel models:
>
> - **Conditional effect** = the effect of a variable in an *average
>   cluster* (i.e., group-specific, subject-specific or cluster-specific
>   effect, or an average or a typical cluster)
> - **Marginal effect** = effect of a variable *across clusters on
>   average* (i.e., global/population-level effect, or clusters on
>   average).

When working with mixed models, the `modelbased` package offers
flexibility in calculating EMMs through different backends. The
`"marginaleffects"` backend and the `"emmeans"` backend employ different
underlying methodologies. **marginaleffects** typically focuses on
marginal predictions by averaging over the random effects, while
**emmeans** provides conditional predictions. Consequently, the EMMs
obtained using these two backends may differ. As we will see,
conditional and marginal predictions for *fixed effects* focal
predictors…

- … are usually similar for linear mixed models
- … are usually different for generalized linear mixed models
- … are usually different for both linear and generalized linear models,
  when the data is “imbalanced” and “average” predictions are requested
  (i.e., `estimate = "average"`); by *imbalanced* we mean not equally
  distributed levels.

> **Note:**
>
> If you request unit-level predictions - that is, predictions specific
> to the individual levels of the random effects (achieved by including
> random effect variables in `by`) - conditional and marginal
> predictions will also differ in linear mixed models. We will not delve
> into unit-level predictions in this vignette.

In essence, the choice of backend and the understanding of whether we
are looking at conditional or marginal predictions are critical for
correctly interpreting the results of mixed models. Carefully
considering the research question and the nature of the random effects
will guide the selection of the appropriate approach.

> **Technical Notes:**
>
> - For `backend = "marginaleffects"`, the `re.form` argument is set to
>   `NULL` for mixed models by default, to calculate *marginal
>   predictions*. You can use for instance `re.form = NA` in your
>   [`estimate_means()`](https://easystats.github.io/modelbased/reference/estimate_means.md)
>   call to change the default value (`NA` will produce *conditional
>   predictions*).
>
> - By default, both backends calculate predictions for a balanced data
>   grid representing all combinations of focal predictor levels
>   (specified in `by`). This represents a “typical” observation based
>   on the data grid and is useful for comparing groups. Setting
>   `estimate = "average"` can be useful to calculate the average
>   expected outcome from those observations *from the sample* at hand,
>   however, this option is only available for
>   `backend = "marginaleffects"`.

This vignette shows some examples to demonstrate where results are
similar and where they differ.

### Linear mixed models

#### Balanced Data

This section demonstrates the calculation of estimated marginal means
(EMMs) in a linear mixed model using balanced data. We’ll use the
`sleepstudy` dataset from the **lme4** package.

In this example, we fit a linear mixed model predicting `Reaction` based
on `Days`, with random intercepts and slopes for `Subject`. We then
calculate the EMMs for `Days` using both the default `"marginaleffects"`
backend and the `"emmeans"` backend. Because the data is balanced and we
have a linear, the results are similar (**exception:** we find small
differences for the confidence intervals, but ignore this for now…).

``` r

library(modelbased)
data(sleepstudy, package = "lme4")
# for later, create a slightly imbalanced distributed predictor
set.seed(1234)
sleepstudy$x <- as.factor(sample.int(3, nrow(sleepstudy), replace = TRUE))

model <- lme4::lmer(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)

# default, marginaleffects backend (marginal predictions)
# in this case, same result as for conditional predictions
estimate_means(model, "Days")
#> Estimated Marginal Means
#> 
#> Days |               Mean (CI)
#> ------------------------------
#> 0    | 251.41 (237.94, 264.87)
#> 1    | 261.87 (248.48, 275.27)
#> 2    | 272.34 (258.34, 286.34)
#> 3    | 282.81 (267.60, 298.02)
#> 4    | 293.27 (276.39, 310.16)
#> 5    | 303.74 (284.83, 322.65)
#> 6    | 314.21 (293.03, 335.39)
#> 7    | 324.68 (301.05, 348.31)
#> 8    | 335.14 (308.94, 361.35)
#> 9    | 345.61 (316.74, 374.48)
#> 
#> Variable predicted: Reaction
#> Predictors modulated: Days
#> Predictors averaged: Subject

# emmeans backend, always conditional predictions
estimate_means(model, "Days", backend = "emmeans")
#> Estimated Marginal Means
#> 
#> Days |               Mean (CI)
#> ------------------------------
#> 0    | 251.41 (237.01, 265.80)
#> 1    | 261.87 (247.55, 276.19)
#> 2    | 272.34 (257.37, 287.31)
#> 3    | 282.81 (266.55, 299.06)
#> 4    | 293.27 (275.22, 311.32)
#> 5    | 303.74 (283.53, 323.96)
#> 6    | 314.21 (291.57, 336.85)
#> 7    | 324.68 (299.42, 349.94)
#> 8    | 335.14 (307.13, 363.16)
#> 9    | 345.61 (314.75, 376.47)
#> 
#> Variable predicted: Reaction
#> Predictors modulated: Days
```

Here, we calculate the marginal predictions for the `Days` variable
using the `estimate = "average"` argument. In this case, because the
model is linear and the data is balanced, the predictions are again the
same as the previous results, i.e., averaging across all observations
makes no difference here.

``` r

# marginal predictions, averaged across all observations,
# same as conditional predictions above
estimate_means(model, "Days", estimate = "average")
#> Average Predictions
#> 
#> Days |               Mean (CI)
#> ------------------------------
#> 0    | 251.41 (237.94, 264.87)
#> 1    | 261.87 (248.48, 275.27)
#> 2    | 272.34 (258.34, 286.34)
#> 3    | 282.81 (267.60, 298.02)
#> 4    | 293.27 (276.39, 310.16)
#> 5    | 303.74 (284.83, 322.65)
#> 6    | 314.21 (293.03, 335.39)
#> 7    | 324.68 (301.05, 348.31)
#> 8    | 335.14 (308.94, 361.35)
#> 9    | 345.61 (316.74, 374.48)
#> 
#> Variable predicted: Reaction
#> Predictors modulated: Days
```

#### Imbalanced Data

This section explores the impact of imbalanced data on EMM calculations
in linear mixed models. We’ll use the `penguins` dataset, which has
imbalanced groups that we use as higher-level unit, as well as
imbalanced predictors. Since we have still a linear mixed model,
marginal and conditional predictions are still similar.

``` r

data(penguins, package = "palmerpenguins")
model <- lme4::lmer(bill_length_mm ~ sex + island + (1 | species), data = penguins)

# marginal predictions
estimate_means(model, "sex")
#> Estimated Marginal Means
#> 
#> sex    |            Mean (CI)
#> -----------------------------
#> female | 43.29 (37.00, 49.58)
#> male   | 46.99 (40.70, 53.28)
#> 
#> Variable predicted: bill_length_mm
#> Predictors modulated: sex
#> Predictors averaged: island, species

# conditional predictions
estimate_means(model, "sex", backend = "emmeans")
#> Estimated Marginal Means
#> 
#> sex    |            Mean (CI)
#> -----------------------------
#> female | 43.29 (29.54, 57.04)
#> male   | 46.99 (33.24, 60.74)
#> 
#> Variable predicted: bill_length_mm
#> Predictors modulated: sex
```

Since we have imbalanced data, results for “average” predictions will
differ from the results above, because both the `"emmeans"` backend and
the default `estimate` setting for the `"marginaleffects"` backend
produce “data grid based” predictions, which do not average across all
observations (see also [the
documentation](https://easystats.github.io/modelbased/reference/estimate_means.html)
of the `estimate` argument for further details).

Furthermore, for imbalanced data, conditional and marginal predictions
will differ when they are averaged across all observations.

``` r

# average marginal predictions
estimate_means(model, "sex", estimate = "average")
#> Average Predictions
#> 
#> sex    |            Mean (CI)
#> -----------------------------
#> female | 42.10 (35.81, 48.39)
#> male   | 45.85 (39.57, 52.14)
#> 
#> Variable predicted: bill_length_mm
#> Predictors modulated: sex

# average conditional predictions
estimate_means(model, "sex", estimate = "average", re.form = NA)
#> Average Predictions
#> 
#> sex    |            Mean (CI)
#> -----------------------------
#> female | 43.26 (36.97, 49.54)
#> male   | 46.95 (40.66, 53.24)
#> 
#> Variable predicted: bill_length_mm
#> Predictors modulated: sex
```

### Generalized linear mixed models

The generalized linear mixed model (GLMM) example, using a Poisson
distribution to model fish counts, demonstrates the substantial
differences between marginal and conditional predictions in non-linear
models. When examining the effect of the `camper` variable, marginal
predictions yield higher estimated means (1.26 for `camper = 0` and 3.21
for `camper = 1`) compared to conditional predictions (using
`re.form = NA` or the `"emmeans"` backend), which are lower (0.66 for
`camper = 0` and 1.68 for `camper = 1`).

``` r

data("fish", package = "insight")
model <- lme4::glmer(
  count ~ child + camper + (1 | persons),
  data = fish,
  family = poisson()
)

# marginal predictions
estimate_means(model, "camper")
#> Estimated Marginal Means
#> 
#> camper |          Mean (CI)
#> ---------------------------
#> 0      | 1.26 (-0.28, 2.79)
#> 1      | 3.21 (-0.68, 7.10)
#> 
#> Variable predicted: count
#> Predictors modulated: camper
#> Predictors averaged: child (0.68), persons (1)
#> Predictions are on the response-scale.

# conditional predictions
estimate_means(model, "camper", re.form = NA)
#> Estimated Marginal Means
#> 
#> camper |          Mean (CI)
#> ---------------------------
#> 0      | 0.66 (-0.14, 1.46)
#> 1      | 1.68 (-0.36, 3.71)
#> 
#> Variable predicted: count
#> Predictors modulated: camper
#> Predictors averaged: child (0.68), persons (1)
#> Predictions are on the response-scale.

# conditional predictions
estimate_means(model, "camper", backend = "emmeans")
#> Estimated Marginal Means
#> 
#> camper |         Rate (CI) | Rate
#> ---------------------------------
#> 0      | 0.66 (0.19, 2.23) | 0.66
#> 1      | 1.68 (0.50, 5.64) | 1.68
#> 
#> Variable predicted: count
#> Predictors modulated: camper
#> Predictions are on the response-scale.
```

Furthermore, when using `estimate = "average"` to average across all
observations, the differences between marginal and conditional
predictions persist, with marginal predictions showing higher means
(1.52 and 4.54) than conditional predictions (1.20 and 3.19), but both
results differ from the predictions for “typical” observations based on
the default `estimate` option (or `"emmeans"` backend), as shown in the
previous output.

``` r

# average marginal predictions
estimate_means(model, "camper", estimate = "average")
#> Average Predictions
#> 
#> camper |           Mean (CI)
#> ----------------------------
#> 0      | 1.52 (-0.33,  3.38)
#> 1      | 4.54 (-0.95, 10.03)
#> 
#> Variable predicted: count
#> Predictors modulated: camper
#> Predictions are on the response-scale.

# average conditional predictions
estimate_means(model, "camper", estimate = "average", re.form = NA)
#> Average Predictions
#> 
#> camper |          Mean (CI)
#> ---------------------------
#> 0      | 1.20 (-0.26, 2.66)
#> 1      | 3.19 (-0.67, 7.04)
#> 
#> Variable predicted: count
#> Predictors modulated: camper
#> Predictions are on the response-scale.
```

This highlights that in GLMMs, the choice between marginal and
conditional predictions significantly impacts the estimated means and,
consequently, the interpretation of the model’s results. In this case,
the marginal predictions are higher than the conditional predictions.

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
observations (by setting `estimate = "average"`) when working with mixed
models.** This approach effectively incorporates the variation inherent
in the random effects (higher-level units) and provides EMMs that more
accurately reflect the overall patterns observed within the actual
sample data. By averaging over the observed data, we obtain a more
robust and representative estimate of the population-level effects,
making it a preferred strategy for interpreting mixed model results.

## References

Heiss, Andrew. 2022. “Marginal and Conditional Effects for GLMMs with
{Marginaleffects}.” In *Andrew Heiss’s Blog*.
<https://doi.org/10.59350/xwnfm-x1827>.
