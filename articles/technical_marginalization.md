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

First, let’s load the necessary packages and dataset.

``` r

library(easystats)
data(penguins)
```

We fit a simple linear model with a continuous outcome (`flipper_len`),
a categorical focal predictor (`sex`), and a categorical non-focal
predictor (`species`).

``` r

m <- lm(flipper_len ~ sex + species, data = penguins)
```

Next, we extract the data used to fit the model. This does not perfectly
correspond to the original dataset because a few cases with missing
values were removed via list-wise deletion. We will need this data
later. Let’s also print the regression coefficients.

``` r

model_data <- get_data(m)

mp <- model_parameters(m)
mp
#> Parameter           | Coefficient |   SE |           95% CI | t(329) |      p
#> -----------------------------------------------------------------------------
#> (Intercept)         |      186.68 | 0.57 | [185.56, 187.80] | 328.44 | < .001
#> sex [male]          |        6.85 | 0.63 | [  5.62,   8.08] |  10.91 | < .001
#> species [Chinstrap] |        5.72 | 0.84 | [  4.07,   7.37] |   6.80 | < .001
#> species [Gentoo]    |       27.05 | 0.71 | [ 25.65,  28.44] |  38.24 | < .001
```

## 1. “Specific” Observation

What happens here? Numeric variables are set to their mean, and factors
are set to their reference level. Thus, a prediction is made for a very
“specific” observation in the dataset (here: `species = Adelie`).

``` r

estimate_means(m, "sex", estimate = "specific")
#> Model-based Predictions
#> 
#> sex    |               Mean (CI)
#> --------------------------------
#> female | 186.68 (185.56, 187.80)
#> male   | 193.53 (192.41, 194.65)
#> 
#> Variable predicted: flipper_len
#> Predictors modulated: sex
#> Predictors controlled: species (Adelie)

# Alternative code (same result, different wrapper)
estimate_relation(m, by = "sex")
#> Model-based Predictions
#> 
#> sex    |          Predicted (CI)
#> --------------------------------
#> female | 186.68 (185.56, 187.80)
#> male   | 193.53 (192.41, 194.65)
#> 
#> Variable predicted: flipper_len
#> Predictors modulated: sex
#> Predictors controlled: species (Adelie)
```

To illustrate, we can calculate this manually. “Female” is the reference
level for `sex` here. Since `species` is kept at its reference level
(Adelie), the estimated marginal mean corresponds exactly to the
intercept.

``` r

# female (reference)
mp$Coefficient[1]
#> [1] 186.6777
```

For “male”, we add the main effect for `sexmale` to the intercept.
`species` still remains at the reference level (Adelie).

``` r

# male
mp$Coefficient[1] + mp$Coefficient[2]
#> [1] 193.5278
```

### Excursus: How does `predict()` work in comparison?

Let’s compare this logic to the standard
[`predict()`](https://rdrr.io/r/stats/predict.html) function.

``` r

pred <- predict(m)

# Let's look at the real data of the 1st observation in the dataset:
penguins[1, c("species", "sex")] # Adelie, male
#>   species  sex
#> 1  Adelie male

# Predicted value (predicted mean) for this 1st observation:
pred[1]
#>        1 
#> 193.5278

# Manual calculation: Intercept + effect for "male" (since Adelie is the reference)
mp$Coefficient[1] + mp$Coefficient[2]
#> [1] 193.5278

# Let's look at the real data of the 221st observation in the dataset:
penguins[221, c("species", "sex")] # Gentoo, female
#>     species    sex
#> 221  Gentoo female

# Predicted value (predicted mean) for this 221st observation:
pred[221]
#>      229 
#> 213.7239

# Manual calculation:
# Intercept (female is reference) + effect for species "Gentoo" (4th coefficient)
mp$Coefficient[1] + mp$Coefficient[4]
#> [1] 213.7239
```

## 2. “Typical” Observation

What happens here? A data grid is created across all combinations of
predictors. The predictions are then averaged across the levels of the
non-focal terms (here: `species`). Each level of `species` is weighted
*equally* in the calculation, regardless of how often it occurs in the
actual empirical data.

``` r

# We manually create a reference grid with evenly distributed levels
# of our non-focal ("controlled for") term "species".
d <- rbind(
  get_datagrid(m, c("sex", "species = 'Adelie'")),
  get_datagrid(m, c("sex", "species = 'Chinstrap'")),
  get_datagrid(m, c("sex", "species = 'Gentoo'"))
)

# Now we calculate the predicted values for this balanced grid.
# The non-focal terms are weighted exactly equally here
# (1/3 Adelie, 1/3 Chinstrap, 1/3 Gentoo).
d$predicted <- predict(m, newdata = d)

# Manual calculation of the means grouped by "sex":
means_by_group(d, "predicted", "sex")
#> # Mean of predicted by sex
#> 
#> Category |   Mean | N |    SD |           95% CI |     p
#> --------------------------------------------------------
#> female   | 197.60 | 3 | 14.25 | [174.75, 220.45] | 0.588
#> male     | 204.45 | 3 | 14.25 | [181.60, 227.30] | 0.588
#> Total    | 201.03 | 6 | 13.29 |                  |      
#> 
#> Anova: R2=0.080; adj.R2=-0.150; F=0.346; p=0.588

# This corresponds to the easystats default (estimate = "typical").
estimate_means(m, "sex")
#> Estimated Marginal Means
#> 
#> sex    |               Mean (CI)
#> --------------------------------
#> female | 197.60 (196.70, 198.50)
#> male   | 204.45 (203.56, 205.34)
#> 
#> Variable predicted: flipper_len
#> Predictors modulated: sex
#> Predictors averaged: species
```

## 3. “Average” Observation

What happens here? The prediction is made for *every* single observation
in the real dataset. Afterwards, the average of these predictions is
calculated per focal group. By doing this, the non-focal terms retain
their exact empirical distribution.

``` r

d <- model_data

# We use the predict() function for the entire original dataset
d$predicted <- predict(m)

# Afterwards, we calculate the mean of the predictions, grouped by "sex"
means_by_group(d, "predicted", "sex")
#> # Mean of predicted by sex
#> 
#> Category |   Mean |   N |    SD |           95% CI |      p
#> -----------------------------------------------------------
#> female   | 197.36 | 165 | 12.27 | [195.48, 199.25] | < .001
#> male     | 204.51 | 168 | 12.35 | [202.64, 206.37] | < .001
#> Total    | 200.97 | 333 | 12.80 |                  |       
#> 
#> Anova: R2=0.078; adj.R2=0.075; F=28.008; p<.001

# The corresponding easystats call:
estimate_means(m, "sex", estimate = "average")
#> Average Predictions
#> 
#> sex    |               Mean (CI)
#> --------------------------------
#> female | 197.36 (196.49, 198.24)
#> male   | 204.51 (203.64, 205.38)
#> 
#> Variable predicted: flipper_len
#> Predictors modulated: sex
```

## 4. “Counterfactual” Observation (also “Population”)

What happens here? To resolve confounding and simulate a
pseudo-randomization (G-Computation), we replicate the entire dataset
for each level of our focal predictor (`sex`).

We basically force every penguin to be female once, and male once, but
we retain its original `species` (and de facto all other unobserved
characteristics/confounding that we want to equalize through
randomization).

``` r

# We duplicate ("clone") our dataset to imitate randomization.
d <- do.call(rbind, replicate(2, model_data, simplify = FALSE))

# For each cloned dataset, we set our focal term to one of the two levels.
# The first half of the dataset becomes entirely "female", the second half entirely "male".
d$sex <- as.factor(rep(levels(model_data$sex), each = nrow(model_data)))

# We calculate the predicted values for this new "what-if" scenario,
# i.e., for our pseudo-randomized sample.
d$predicted <- predict(m, newdata = d)

# Now we calculate the average predicted value for the levels of "sex".
# The non-focal terms are weighted exactly proportional to their actual
# occurrence in the real data here.
means_by_group(d, "predicted", "sex")
#> # Mean of predicted by sex
#> 
#> Category |   Mean |   N |    SD |           95% CI |      p
#> -----------------------------------------------------------
#> female   | 197.51 | 333 | 12.30 | [196.19, 198.83] | < .001
#> male     | 204.36 | 333 | 12.30 | [203.04, 205.68] | < .001
#> Total    | 200.94 | 666 | 12.76 |                  |       
#> 
#> Anova: R2=0.072; adj.R2=0.071; F=51.680; p<.001

# The corresponding easystats call:
estimate_means(m, "sex", estimate = "counterfactual")
#> Average Counterfactual Predictions
#> 
#> sex    |               Mean (CI)
#> --------------------------------
#> female | 197.51 (196.63, 198.39)
#> male   | 204.36 (203.49, 205.23)
#> 
#> Variable predicted: flipper_len
#> Predictors modulated: sex
#> Predictors averaged: species
```

------------------------------------------------------------------------

## Summary

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
