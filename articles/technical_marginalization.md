# Understanding marginalization methods

This vignette explores the
[`estimate_means()`](https://easystats.github.io/modelbased/reference/estimate_means.md)
function, providing a practical guide to calculating and interpreting
estimated marginal means. Using the *penguins* dataset, we will
demonstrate how different estimation methods (triggered via the
`estimate` argument) - `"specific"`, `"typical"`, `"average"`, and
`"counterfactual"` - change the assumptions behind your model
predictions. By manually recreating these calculations alongside the
automated functions, you will gain a much clearer understanding of what
actually happens under the hood when interpreting (linear) models and
their estimated marginal means.

One crucial distinction is between *conditional* and *marginal*
predictions. This becomes even more important for mixed-effects models,
[as outlined in this
vignette](https://easystats.github.io/modelbased/articles/mixed_models.html).

First, let’s load the necessary packages and dataset.

[`library`](https://rdrr.io/r/base/library.html)`(`[`easystats`](https://easystats.github.io/easystats/)`)`` `[`data`](https://rdrr.io/r/utils/data.html)`(``penguins``)`

We fit a simple linear model with a continuous outcome (`flipper_len`),
a categorical focal predictor (`sex`), and a categorical non-focal
predictor (`species`).

`m`` ``<-`` `[`lm`](https://rdrr.io/r/stats/lm.html)`(``flipper_len`` ``~`` ``sex`` ``+`` ``species``, data ``=`` ``penguins``)`

Next, we extract the data used to fit the model. This does not perfectly
correspond to the original dataset because a few cases with missing
values were removed via list-wise deletion. We will need this data
later. Let’s also print the regression coefficients.

`model_data`` ``<-`` `[`get_data`](https://easystats.github.io/insight/reference/get_data.html)`(``m``)`` `` ``mp`` ``<-`` `[`model_parameters`](https://easystats.github.io/parameters/reference/model_parameters.html)`(``m``)`` ``mp`` ``#> Parameter | Coefficient (CI)`` ``#> ---------------------------------------------`` ``#> (Intercept) | 186.68 (185.56, 187.80)`` ``#> sex [male] | 6.85 ( 5.62, 8.08)`` ``#> species [Chinstrap] | 5.72 ( 4.07, 7.37)`` ``#> species [Gentoo] | 27.05 ( 25.65, 28.44)`

## 1. “Specific” Observation

What happens here? Numeric variables are set to their mean, and factors
are set to their reference level. Thus, a prediction is made for a very
“specific” observation in the dataset (here: `species = Adelie`).

[`estimate_means`](https://easystats.github.io/modelbased/reference/estimate_means.md)`(``m``, ``"sex"``, estimate ``=`` ``"specific"``)`` ``#> Model-based Predictions`` ``#> `` ``#> sex | Mean (CI)`` ``#> --------------------------------`` ``#> female | 186.68 (185.56, 187.80)`` ``#> male | 193.53 (192.41, 194.65)`` ``#> `` ``#> Variable predicted: flipper_len`` ``#> Predictors modulated: sex`` ``#> Predictors controlled: species (Adelie)`` `` ``# Alternative code (same result, different wrapper)`` `[`estimate_relation`](https://easystats.github.io/modelbased/reference/estimate_expectation.md)`(``m``, by ``=`` ``"sex"``)`` ``#> Model-based Predictions`` ``#> `` ``#> sex | Predicted (CI)`` ``#> --------------------------------`` ``#> female | 186.68 (185.56, 187.80)`` ``#> male | 193.53 (192.41, 194.65)`` ``#> `` ``#> Variable predicted: flipper_len`` ``#> Predictors modulated: sex`` ``#> Predictors controlled: species (Adelie)`

To illustrate, we can calculate this manually. “Female” is the reference
level for `sex` here. Since `species` is kept at its reference level
(Adelie), the estimated marginal mean corresponds exactly to the
intercept.

`# female (reference)`` ``mp``$``Coefficient``[``1``]`` ``#> [1] 186.6777`

For “male”, we add the main effect for `sexmale` to the intercept.
`species` still remains at the reference level (Adelie).

`# male`` ``mp``$``Coefficient``[``1``]`` ``+`` ``mp``$``Coefficient``[``2``]`` ``#> [1] 193.5278`

### Excursus: How does `predict()` work in comparison?

Let’s compare this logic to the standard
[`predict()`](https://rdrr.io/r/stats/predict.html) function.

`pred`` ``<-`` `[`predict`](https://rdrr.io/r/stats/predict.html)`(``m``)`` `` ``# Let's look at the real data of the 1st observation in the dataset:`` ``penguins``[``1``, `[`c`](https://rdrr.io/r/base/c.html)`(``"species"``, ``"sex"``)``]`` ``# Adelie, male`` ``#> species sex`` ``#> 1 Adelie male`` `` ``# Predicted value (predicted mean) for this 1st observation:`` ``pred``[``1``]`` ``#> 1 `` ``#> 193.5278`` `` ``# Manual calculation: Intercept + effect for "male" (since Adelie is the reference)`` ``mp``$``Coefficient``[``1``]`` ``+`` ``mp``$``Coefficient``[``2``]`` ``#> [1] 193.5278`` `` ``# Let's look at the real data of the 221st observation in the dataset:`` ``penguins``[``221``, `[`c`](https://rdrr.io/r/base/c.html)`(``"species"``, ``"sex"``)``]`` ``# Gentoo, female`` ``#> species sex`` ``#> 221 Gentoo female`` `` ``# Predicted value (predicted mean) for this 221st observation:`` ``pred``[``221``]`` ``#> 229 `` ``#> 213.7239`` `` ``# Manual calculation:`` ``# Intercept (female is reference) + effect for species "Gentoo" (4th coefficient)`` ``mp``$``Coefficient``[``1``]`` ``+`` ``mp``$``Coefficient``[``4``]`` ``#> [1] 213.7239`

## 2. “Typical” Observation

What happens here? A data grid is created across all combinations of
predictors. The predictions are then averaged across the levels of the
non-focal terms (here: `species`). Each level of `species` is weighted
*equally* in the calculation, regardless of how often it occurs in the
actual empirical data.

`# We manually create a reference grid with evenly distributed levels`` ``# of our non-focal ("controlled for") term "species".`` ``d`` ``<-`` `[`rbind`](https://rdrr.io/r/base/cbind.html)`(`` `` `[`get_datagrid`](https://easystats.github.io/insight/reference/get_datagrid.html)`(``m``, `[`c`](https://rdrr.io/r/base/c.html)`(``"sex"``, ``"species = 'Adelie'"``)``)``,`` `` `[`get_datagrid`](https://easystats.github.io/insight/reference/get_datagrid.html)`(``m``, `[`c`](https://rdrr.io/r/base/c.html)`(``"sex"``, ``"species = 'Chinstrap'"``)``)``,`` `` `[`get_datagrid`](https://easystats.github.io/insight/reference/get_datagrid.html)`(``m``, `[`c`](https://rdrr.io/r/base/c.html)`(``"sex"``, ``"species = 'Gentoo'"``)``)`` ``)`` `` ``# Now we calculate the predicted values for this balanced grid.`` ``# The non-focal terms are weighted exactly equally here`` ``# (1/3 Adelie, 1/3 Chinstrap, 1/3 Gentoo).`` ``d``$``predicted`` ``<-`` `[`predict`](https://rdrr.io/r/stats/predict.html)`(``m``, newdata ``=`` ``d``)`` `` ``# Manual calculation of the means grouped by "sex":`` `[`means_by_group`](https://easystats.github.io/datawizard/reference/means_by_group.html)`(``d``, ``"predicted"``, ``"sex"``)`` ``#> # Mean of predicted by sex`` ``#> `` ``#> Category | Mean | N | SD | 95% CI | p`` ``#> --------------------------------------------------------`` ``#> female | 197.60 | 3 | 14.25 | [174.75, 220.45] | 0.588`` ``#> male | 204.45 | 3 | 14.25 | [181.60, 227.30] | 0.588`` ``#> Total | 201.03 | 6 | 13.29 | | `` ``#> `` ``#> Anova: R2=0.080; adj.R2=-0.150; F=0.346; p=0.588`` `` ``# This corresponds to the easystats default (estimate = "typical").`` `[`estimate_means`](https://easystats.github.io/modelbased/reference/estimate_means.md)`(``m``, ``"sex"``)`` ``#> Estimated Marginal Means`` ``#> `` ``#> sex | Mean (CI)`` ``#> --------------------------------`` ``#> female | 197.60 (196.70, 198.50)`` ``#> male | 204.45 (203.56, 205.34)`` ``#> `` ``#> Variable predicted: flipper_len`` ``#> Predictors modulated: sex`` ``#> Predictors averaged: species`

## 3. “Average” Observation

What happens here? The prediction is made for *every* single observation
in the real dataset. Afterwards, the average of these predictions is
calculated per focal group. By doing this, the non-focal terms retain
their exact empirical distribution.

`d`` ``<-`` ``model_data`` `` ``# We use the predict() function for the entire original dataset`` ``d``$``predicted`` ``<-`` `[`predict`](https://rdrr.io/r/stats/predict.html)`(``m``)`` `` ``# Afterwards, we calculate the mean of the predictions, grouped by "sex"`` `[`means_by_group`](https://easystats.github.io/datawizard/reference/means_by_group.html)`(``d``, ``"predicted"``, ``"sex"``)`` ``#> # Mean of predicted by sex`` ``#> `` ``#> Category | Mean | N | SD | 95% CI | p`` ``#> -----------------------------------------------------------`` ``#> female | 197.36 | 165 | 12.27 | [195.48, 199.25] | < .001`` ``#> male | 204.51 | 168 | 12.35 | [202.64, 206.37] | < .001`` ``#> Total | 200.97 | 333 | 12.80 | | `` ``#> `` ``#> Anova: R2=0.078; adj.R2=0.075; F=28.008; p<.001`` `` ``# The corresponding easystats call:`` `[`estimate_means`](https://easystats.github.io/modelbased/reference/estimate_means.md)`(``m``, ``"sex"``, estimate ``=`` ``"average"``)`` ``#> Average Predictions`` ``#> `` ``#> sex | Mean (CI)`` ``#> --------------------------------`` ``#> female | 197.36 (196.49, 198.24)`` ``#> male | 204.51 (203.64, 205.38)`` ``#> `` ``#> Variable predicted: flipper_len`` ``#> Predictors modulated: sex`

## 4. “Counterfactual” Observation (also “Population”)

What happens here? To resolve confounding and simulate a
pseudo-randomization (G-Computation), we replicate the entire dataset
for each level of our focal predictor (`sex`).

We basically force every penguin to be female once, and male once, but
we retain its original `species` (and de facto all other unobserved
characteristics/confounding that we want to equalize through
randomization).

`# We duplicate ("clone") our dataset to imitate randomization.`` ``d`` ``<-`` `[`do.call`](https://rdrr.io/r/base/do.call.html)`(``rbind``, `[`replicate`](https://rdrr.io/r/base/lapply.html)`(``2``, ``model_data``, simplify ``=`` ``FALSE``)``)`` `` ``# For each cloned dataset, we set our focal term to one of the two levels.`` ``# The first half of the dataset becomes entirely "female", the second half entirely "male".`` ``d``$``sex`` ``<-`` `[`as.factor`](https://rdrr.io/r/base/factor.html)`(`[`rep`](https://rdrr.io/r/base/rep.html)`(`[`levels`](https://rdrr.io/r/base/levels.html)`(``model_data``$``sex``)``, each ``=`` `[`nrow`](https://rdrr.io/r/base/nrow.html)`(``model_data``)``)``)`` `` ``# We calculate the predicted values for this new "what-if" scenario,`` ``# i.e., for our pseudo-randomized sample.`` ``d``$``predicted`` ``<-`` `[`predict`](https://rdrr.io/r/stats/predict.html)`(``m``, newdata ``=`` ``d``)`` `` ``# Now we calculate the average predicted value for the levels of "sex".`` ``# The non-focal terms are weighted exactly proportional to their actual`` ``# occurrence in the real data here.`` `[`means_by_group`](https://easystats.github.io/datawizard/reference/means_by_group.html)`(``d``, ``"predicted"``, ``"sex"``)`` ``#> # Mean of predicted by sex`` ``#> `` ``#> Category | Mean | N | SD | 95% CI | p`` ``#> -----------------------------------------------------------`` ``#> female | 197.51 | 333 | 12.30 | [196.19, 198.83] | < .001`` ``#> male | 204.36 | 333 | 12.30 | [203.04, 205.68] | < .001`` ``#> Total | 200.94 | 666 | 12.76 | | `` ``#> `` ``#> Anova: R2=0.072; adj.R2=0.071; F=51.680; p<.001`` `` ``# The corresponding easystats call:`` `[`estimate_means`](https://easystats.github.io/modelbased/reference/estimate_means.md)`(``m``, ``"sex"``, estimate ``=`` ``"counterfactual"``)`` ``#> Average Counterfactual Predictions`` ``#> `` ``#> sex | Mean (CI)`` ``#> --------------------------------`` ``#> female | 197.51 (196.63, 198.39)`` ``#> male | 204.36 (203.49, 205.23)`` ``#> `` ``#> Variable predicted: flipper_len`` ``#> Predictors modulated: sex`` ``#> Predictors averaged: species`

------------------------------------------------------------------------

## Summary

Broadly speaking, these four methods can be grouped into two conceptual
categories: *conditional* predictions and *marginal* predictions. The
first two approaches rely on artificially constructed reference grids,
providing predictions *conditional* on specific, theoretical individual
levels. In contrast, the latter two approaches calculate predictions for
the actual empirical observations before averaging, thus reflecting
*marginal* predictions at the sample or population level.

### 1. Specific Observation (`estimate = "specific"`)

- **Calculation:** Numeric values are set to their mean. Factors are set
  to their reference level.
- **Meaning:** The estimated means represent a rather “theoretical” view
  of the data. A prediction is made for a specific individual from the
  sample (i.e., a very specific combination of predictor values). This
  answers the question about the expected average value of the target
  variable for a *specific* observation.

### 2. Typical Observation (`estimate = "typical"`, the default)

- **Calculation:** Predictions are made for observations represented by
  a data grid. This grid is formed from all combinations of the
  predictor levels of the focal terms. For non-focal numeric predictors,
  the mean is taken, and we marginalize over the factor levels of the
  non-focal terms (a kind of weighted average is calculated).
- **Meaning:** These predictions are useful to compare defined “groups”
  and represent the sample well. It answers the question: *“What would
  the average outcome be for a ‘typical’ observation?”*, where ‘typical’
  refers to subjects represented by the balanced data grid.

### 3. Average Observation (`estimate = "average"`)

- **Calculation:** Predictions are made for every observation in the
  sample. Afterwards, the average of all predictions within all groups
  (or levels) of the focal terms defined in `by` is calculated.
- **Meaning:** These predictions are the closest representation of the
  actual sample, as the average is taken over the entire dataset. The
  groups are not represented by a balanced data grid here, but by the
  empirical distributions of the sample characteristics. It answers the
  question about the predicted value for an average observation from a
  specific group within your own data.

### 4. Counterfactual Observation (`estimate = "population"` / `"counterfactual"`)

- **Calculation:** Non-focal predictors are marginalized over the
  observations of the sample, with the sample being replicated multiple
  times to create “counterfactuals” (what-if scenarios). Then, the
  average of these predicted values is calculated (aggregated/grouped by
  the focal terms).
- **Meaning:** This can be viewed as an extrapolation to a hypothetical
  target population. Counterfactual predictions are useful because the
  results can also be transferred to other contexts (G-Computation,
  causal inference). It answers the question: *“What is the predicted
  response value for the average observation in the broader target
  population?”* (or for my “pseudo-randomized” sample). Due to the
  pseudo-randomization approach, we’re allowed to make causal inferences
  even for observational data (note that there are certain assumptions
  that need to be fulfilled for causal inference; see section
  **Assumptions for causal identification** in [this
  vignette](https://easystats.github.io/modelbased/articles/practical_causality.html)).

### Conclusion: Conditional vs. Marginal Predictions

When deciding which method to use, the core question is whether your
estimand relates to a theoretical individual or the population as a
whole. Use *conditional* predictions (`"specific"`, `"typical"`) when
you want to evaluate expected outcomes for predefined, fixed
combinations of covariates (using a reference grid). Use *marginal*
predictions (`"average"`, `"counterfactual"`) when you want to account
for the actual empirical distribution of your sample. Marginal methods
average over the real-world variability of your subjects, making them
the recommended choice for making population-level generalizations or
drawing causal inferences.

For **visualizations**, *conditional* predictions or
`estimate = "population"` typically yield smoother curves for continuous
focal predictors, whereas *marginal* predictions with
`estimate = "average"` may introduce noisy visual artifacts.
`estimate = "average"` calculates the average based only on the data
points that actually exist and doesn’t generate a *complete* grid of all
theoretical combinations of predictor values. Consequently, the output
may not include all the values.

## Appendix: Code equivalent in different R packages

While this vignette focuses on the **easystats** ecosystem, it is
helpful to see how these concepts translate to other popular packages.
Below, we provide the equivalent R code to calculate the `"specific"`,
`"typical"`, `"average"`, and `"counterfactual"` marginal means using
the *marginaleffects* and *emmeans* packages. Notice how different
packages use different terminology - such as varying `newdata` arguments
or specific weighting schemes - to achieve the exact same conceptual
estimates under the hood.

We use the
[`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.html)
function to ensure the output displays enough decimal places for a
precise comparison.

### Specific Observation

`# easystats: Sets numeric variables to their mean and factors`` ``# to their reference level.`` `[`estimate_means`](https://easystats.github.io/modelbased/reference/estimate_means.md)`(``m``, ``"sex"``, estimate ``=`` ``"specific"``)`` ``#> Model-based Predictions`` ``#> `` ``#> sex | Mean (CI)`` ``#> --------------------------------`` ``#> female | 186.68 (185.56, 187.80)`` ``#> male | 193.53 (192.41, 194.65)`` ``#> `` ``#> Variable predicted: flipper_len`` ``#> Predictors modulated: sex`` ``#> Predictors controlled: species (Adelie)`` `` ``# marginaleffects: The 'newdata = "mean"' argument replicates this`` ``# "theoretical" observation behavior.`` ``marginaleffects``::`[`avg_predictions`](https://rdrr.io/pkg/marginaleffects/man/predictions.html)`(``m``, by ``=`` ``"sex"``, newdata ``=`` ``"mean"``)`` ``|>`` `[`model_parameters`](https://easystats.github.io/parameters/reference/model_parameters.html)`(``)`` ``#> Predicted (CI) | sex`` ``#> --------------------------------`` ``#> 186.68 (185.56, 187.79) | female`` ``#> 193.53 (192.41, 194.64) | male`

### Typical Observation

`# easystats: Builds a balanced reference grid and weights each group`` ``# equally (the default).`` `[`estimate_means`](https://easystats.github.io/modelbased/reference/estimate_means.md)`(``m``, ``"sex"``, estimate ``=`` ``"typical"``)`` ``#> Estimated Marginal Means`` ``#> `` ``#> sex | Mean (CI)`` ``#> --------------------------------`` ``#> female | 197.60 (196.70, 198.50)`` ``#> male | 204.45 (203.56, 205.34)`` ``#> `` ``#> Variable predicted: flipper_len`` ``#> Predictors modulated: sex`` ``#> Predictors averaged: species`` `` ``# marginaleffects: 'newdata = "grid"' creates a similar balanced`` ``# data grid.`` ``marginaleffects``::`[`avg_predictions`](https://rdrr.io/pkg/marginaleffects/man/predictions.html)`(``m``, by ``=`` ``"sex"``, newdata ``=`` ``"grid"``)`` ``|>`` `[`model_parameters`](https://easystats.github.io/parameters/reference/model_parameters.html)`(``)`` ``#> Predicted (CI) | sex`` ``#> --------------------------------`` ``#> 197.60 (196.70, 198.50) | female`` ``#> 204.45 (203.56, 205.34) | male`` `` ``# emmeans: Equal weighting across a reference grid is the default`` ``# behavior in emmeans.`` ``emmeans``::`[`emmeans`](https://rvlenth.github.io/emmeans/reference/emmeans.html)`(``m``, ``"sex"``)`` ``|>`` `[`model_parameters`](https://easystats.github.io/parameters/reference/model_parameters.html)`(``)`` ``#> Marginal Means (CI) | sex`` ``#> --------------------------------`` ``#> 197.60 (196.70, 198.50) | female`` ``#> 204.45 (203.56, 205.34) | male`

### Average Observation

`# easystats: Calculates predictions for every observation in the`` ``# empirical data and averages them.`` `[`estimate_means`](https://easystats.github.io/modelbased/reference/estimate_means.md)`(``m``, ``"sex"``, estimate ``=`` ``"average"``)`` ``#> Average Predictions`` ``#> `` ``#> sex | Mean (CI)`` ``#> --------------------------------`` ``#> female | 197.36 (196.49, 198.24)`` ``#> male | 204.51 (203.64, 205.38)`` ``#> `` ``#> Variable predicted: flipper_len`` ``#> Predictors modulated: sex`` `` ``# marginaleffects: Using 'by' without modifying 'newdata' averages`` ``# predictions over the original empirical dataset.`` ``marginaleffects``::`[`avg_predictions`](https://rdrr.io/pkg/marginaleffects/man/predictions.html)`(``m``, by ``=`` ``"sex"``)`` ``|>`` `[`model_parameters`](https://easystats.github.io/parameters/reference/model_parameters.html)`(``)`` ``#> Predicted (CI) | sex`` ``#> --------------------------------`` ``#> 197.36 (196.49, 198.24) | female`` ``#> 204.51 (203.64, 205.37) | male`` `` ``# emmeans: 'weights = "cell"' weights the reference grid based on`` ``# actual cell frequencies in the data.`` ``emmeans``::`[`emmeans`](https://rvlenth.github.io/emmeans/reference/emmeans.html)`(``m``, ``"sex"``, weights ``=`` ``"cell"``)`` ``|>`` `[`model_parameters`](https://easystats.github.io/parameters/reference/model_parameters.html)`(``)`` ``#> Marginal Means (CI) | sex`` ``#> --------------------------------`` ``#> 197.36 (196.49, 198.24) | female`` ``#> 204.51 (203.64, 205.38) | male`

### Counterfactual / Population Observation

`# easystats: Uses G-computation (cloning the dataset to`` ``# simulate pseudo-randomization).`` `[`estimate_means`](https://easystats.github.io/modelbased/reference/estimate_means.md)`(``m``, ``"sex"``, estimate ``=`` ``"population"``)`` ``#> Average Counterfactual Predictions`` ``#> `` ``#> sex | Mean (CI)`` ``#> --------------------------------`` ``#> female | 197.51 (196.63, 198.39)`` ``#> male | 204.36 (203.49, 205.23)`` ``#> `` ``#> Variable predicted: flipper_len`` ``#> Predictors modulated: sex`` ``#> Predictors averaged: species`` `` ``# marginaleffects: Using the 'variables' argument automatically`` ``# triggers this counterfactual approach.`` ``marginaleffects``::`[`avg_predictions`](https://rdrr.io/pkg/marginaleffects/man/predictions.html)`(``m``, variables ``=`` ``"sex"``)`` ``|>`` `[`model_parameters`](https://easystats.github.io/parameters/reference/model_parameters.html)`(``)`` ``#> Predicted (CI) | sex`` ``#> --------------------------------`` ``#> 197.51 (196.64, 198.38) | female`` ``#> 204.36 (203.50, 205.23) | male`` `` ``# emmeans: 'weights = "proportional"' weights the means`` ``# proportionally to the marginal frequencies of the sample.`` ``emmeans``::`[`emmeans`](https://rvlenth.github.io/emmeans/reference/emmeans.html)`(``m``, ``"sex"``, weights ``=`` ``"proportional"``)`` ``|>`` `[`model_parameters`](https://easystats.github.io/parameters/reference/model_parameters.html)`(``)`` ``#> Marginal Means (CI) | sex`` ``#> --------------------------------`` ``#> 197.51 (196.63, 198.39) | female`` ``#> 204.36 (203.49, 205.23) | male`
