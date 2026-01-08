# Contrasts and comparisons for zero-inflation models

    ## Warning in check_dep_version(dep_pkg = "TMB"): package version mismatch: 
    ## glmmTMB was built with TMB package version 1.9.18
    ## Current TMB package version is 1.9.19
    ## Please re-install glmmTMB from source or restore original 'TMB' package (see '?reinstalling' for more information)

This vignette is the last in a 5-part series:

1.  [**Contrasts and Pairwise
    Comparisons**](https://easystats.github.io/modelbased/articles/introduction_comparisons_1.html)

2.  [**User Defined Contrasts and Joint
    Tests**](https://easystats.github.io/modelbased/articles/introduction_comparisons_2.html)

3.  [**Comparisons of Slopes, Floodlight and Spotlight Analysis
    (Johnson-Neyman
    Intervals)**](https://easystats.github.io/modelbased/articles/introduction_comparisons_3.html)

4.  [**Contrasts and Comparisons for Generalized Linear
    Models**](https://easystats.github.io/modelbased/articles/introduction_comparisons_4.html)

5.  **Contrasts and Comparisons for Zero-Inflation Models**

## Contrasts and comparisons for Zero-Inflation Models

Lastly, we show an example for models with zero-inflation component.

### What is a zero-inflated model?

A zero-inflated model is a statistical approach used when dealing with
count data that has an excessive number of zero values. Imagine counting
something that can be zero, like the number of customers a store gets in
a day, and it happens that there are a lot more zeros in the data than a
typical count model (e.g., Poisson regression) would expect. That’s
where we need zero-inflated regression models. These models consider two
ways zeros can happen:

- True Zeros: These are days the store is naturally closed, or maybe
  there’s just no demand for the product.

- Counting Zeros: These are days the store is open but just happens to
  get no customers. Maybe it’s bad luck, or a random fluctuation.

The model treats these differently. It uses one part (the
*zero-inflation* component, a logistic regression) to predict the
probability of a true zero, based on things that make the store less
likely to be open at all. Then it uses another part (the *conditional*,
or *count* component, a count regression) to predict the number of
customers on days the store is actually open, considering other factors
like weather or discounts.

Consequently, such regression models usually have two parts in their
formula, or (depending on the package) separate formulas for the count
and the zero-inflation components. Adjusted predictions can be
calculated for both parts, and contrasts or comparisons can be
calculated for both parts, too.

### How to choose predictors for zero-inflation models?

The two model parts do not necessarily need to use the same predictors.
Therefore, it is not always straightforward to find predictors that can
be used in the zero-inflation model. Think about why you have excess
zeros in your data. Are they true zeros (inherently no counts) or due to
limitations (measurement limitations, biological process, …)? Choose
variables that explain why some data points have zero counts even when
conditions might allow for some count. For instance, if modeling
customer complaints, store location in a remote area might predict zero
complaints due to fewer customers.

## Zero-inflation models using the *glmmTMB* package

In the following example, we use the `Salamanders` dataset from the
`glmmTMB` package.We fit a zero-inflated Poisson regression model to the
data, with `mined` as the predictor variable.

Adjusted predictions using
[`estimate_means()`](https://easystats.github.io/modelbased/reference/estimate_means.md)
can be made for the different model components:

- The **conditional, or count component**, which predicts the average
  count of salamanders. Depending on the model-class (i.e. which package
  was used to fit the model), use `predict = "conditional"` (e.g.,
  package *glmmTMB*) or `predict = "count"` (e.g., package *pscl*). This
  would return the predicted mean from the count component only, which
  is the conditional mean (average counts) of the response only for
  “counting zeros”. It does not take into account the probability of
  “true zeros”.

- The **full model**, which predicts the average count of the response,
  including the zero-inflation component. This would return the expected
  value of the response for an average observation, which *can* be a
  “true zero” or a “count zero”. This is the default, which uses
  `predict = "response"`. For the above example, use this option if you
  want to predict the average number of customers per week, including
  days the store is closed.

- The **zero-inflation probabilities**, which predicts the probabilities
  whether an observation is a “true zero” or not. Again, this option
  depends on the model-class. Use option like `predict = "zprob"` or
  `predict = "zero"`. These predictions only related to the
  zero-inflation component of the model.

``` r

library(modelbased)
library(glmmTMB)

data(Salamanders)
m <- glmmTMB(count ~ mined + (1 | site),
  ziformula = ~mined,
  family = poisson(),
  data = Salamanders
)
```

### Contrasts and comparisons for the conditional model

We will start with the conditional mean. For zero-inflated models, the
conditional mean is predicted using `predict = "conditional"`. This is
the average count of the response, excluding the zero-inflation
component.

``` r

# predicting the conditional mean
estimate_means(m, "mined", predict = "conditional")
#> Estimated Marginal Means
#> 
#> mined | Mean |   SE |       95% CI |     z
#> ------------------------------------------
#> yes   | 1.12 | 0.26 | [0.61, 1.63] |  4.29
#> no    | 3.51 | 0.32 | [2.89, 4.14] | 11.00
#> 
#> Variable predicted: count
#> Predictors modulated: mined
#> Predictors averaged: site
#> Predictions are on the conditional-scale.

estimate_contrasts(m, "mined", predict = "conditional")
#> Marginal Contrasts Analysis
#> 
#> Level1 | Level2 | Difference |   SE |       95% CI |    z |      p
#> ------------------------------------------------------------------
#> no     | yes    |       2.39 | 0.40 | [1.60, 3.18] | 5.93 | < .001
#> 
#> Variable predicted: count
#> Predictors contrasted: mined
#> Predictors averaged: site
#> p-values are uncorrected.
#> Contrasts are on the conditional-scale.
```

### Contrasts and comparisons for the full model

By default, adjusted predictions are returned for the full model,
i.e. the average expected count of the *response*, including the
zero-inflation component.

``` r

# predicting the expected value of the response
estimate_means(m, "mined")
#> Estimated Marginal Means
#> 
#> mined | Mean |   SE |       95% CI |     z
#> ------------------------------------------
#> yes   | 0.27 | 0.05 | [0.17, 0.37] |  5.47
#> no    | 2.27 | 0.22 | [1.83, 2.70] | 10.19
#> 
#> Variable predicted: count
#> Predictors modulated: mined
#> Predictors averaged: site
#> Predictions are on the response-scale.

estimate_contrasts(m, "mined")
#> Marginal Contrasts Analysis
#> 
#> Level1 | Level2 | Difference |   SE |       95% CI |    z |      p
#> ------------------------------------------------------------------
#> no     | yes    |       1.99 | 0.23 | [1.55, 2.44] | 8.78 | < .001
#> 
#> Variable predicted: count
#> Predictors contrasted: mined
#> Predictors averaged: site
#> p-values are uncorrected.
#> Contrasts are on the response-scale.
```

### Contrasts and comparisons for the zero-inflation probabilities

If you’re interested in the probabilities of being a “true zero” or not,
use `predict = "zprob"`.

``` r

# predicting the zero-inflation probabilities
estimate_means(m, "mined", predict = "zprob")
#> Estimated Marginal Means
#> 
#> mined | Probability |   SE |       95% CI |     z
#> -------------------------------------------------
#> yes   |        0.76 | 0.04 | [0.67, 0.84] | 17.54
#> no    |        0.36 | 0.03 | [0.30, 0.41] | 12.75
#> 
#> Variable predicted: count
#> Predictors modulated: mined
#> Predictors averaged: site
#> Predictions are on the zprob-scale.

estimate_contrasts(m, "mined", predict = "zprob")
#> Marginal Contrasts Analysis
#> 
#> Level1 | Level2 | Difference |   SE |         95% CI |     z |      p
#> ---------------------------------------------------------------------
#> no     | yes    |      -0.40 | 0.05 | [-0.50, -0.30] | -7.92 | < .001
#> 
#> Variable predicted: count
#> Predictors contrasted: mined
#> Predictors averaged: site
#> p-values are uncorrected.
#> Contrasts are on the zprob-scale.
```
