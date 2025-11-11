# Contrasts and comparisons for generalized linear models

This vignette is the fourth in a 5-part series:

1.  [**Contrasts and Pairwise
    Comparisons**](https://easystats.github.io/modelbased/articles/introduction_comparisons_1.html)

2.  [**User Defined Contrasts and Joint
    Tests**](https://easystats.github.io/modelbased/articles/introduction_comparisons_2.html)

3.  [**Comparisons of Slopes, Floodlight and Spotlight Analysis
    (Johnson-Neyman
    Intervals)**](https://easystats.github.io/modelbased/articles/introduction_comparisons_3.html)

4.  **Contrasts and Comparisons for Generalized Linear Models**

5.  [**Contrasts and Comparisons for Zero-Inflation
    Models**](https://easystats.github.io/modelbased/articles/introduction_comparisons_5.html)

## Contrasts and comparisons for GLM - logistic regression example

We will now show an example for non-Gaussian models. For GLM’s
(generalized linear models) with (non-Gaussian) link-functions,
[`estimate_means()`](https://easystats.github.io/modelbased/reference/estimate_means.md)
by default returns predicted values on the *response* scale. For
example, predicted values for logistic regression models are shown as
*probabilities*.

Let’s look at a simple example.

``` r

library(modelbased)
set.seed(1234)
dat <- data.frame(
  outcome = rbinom(n = 100, size = 1, prob = 0.35),
  x1 = as.factor(sample(1:3, size = 100, TRUE, prob = c(0.5, 0.2, 0.3))),
  x2 = rnorm(n = 100, mean = 10, sd = 7),
  x3 = as.factor(sample(1:4, size = 100, TRUE, prob = c(0.1, 0.4, 0.2, 0.3)))
)

m <- glm(outcome ~ x1 + x2 + x3, data = dat, family = binomial())
estimate_means(m, "x1")
#> Estimated Marginal Means
#> 
#> x1 | Probability |       95% CI
#> -------------------------------
#> 1  |        0.21 | [0.11, 0.36]
#> 2  |        0.14 | [0.05, 0.34]
#> 3  |        0.31 | [0.16, 0.51]
#> 
#> Variable predicted: outcome
#> Predictors modulated: x1
#> Predictors averaged: x2 (10), x3
#> Predictions are on the response-scale.
```

### Contrasts and comparisons for categorical focal terms

Contrasts or comparisons - like predictions (see above) - are by default
on the *response* scale, i.e. they’re represented as difference between
probabilities (in percentage points).

``` r

estimate_contrasts(m, "x1")
#> Marginal Contrasts Analysis
#> 
#> Level1 | Level2 | Difference |   SE |        95% CI |     z |     p
#> -------------------------------------------------------------------
#> 2      | 1      |      -0.07 | 0.09 | [-0.25, 0.10] | -0.81 | 0.417
#> 3      | 1      |       0.09 | 0.10 | [-0.11, 0.30] |  0.92 | 0.357
#> 3      | 2      |       0.17 | 0.11 | [-0.05, 0.38] |  1.51 | 0.130
#> 
#> Variable predicted: outcome
#> Predictors contrasted: x1
#> Predictors averaged: x2 (10), x3
#> p-values are uncorrected.
#> Contrasts are on the response-scale.
```

The difference between the predicted probability of `x1 = 1` (21.2%) and
`x1 = 2` (13.9%) is roughly 7.3 percentage points. This difference is
not statistically significant (p = 0.417).

Contrasts or comparisons can also be represented on the link-scale, in
this case as *log-odds*. To do so, use `predict = "link"`.

``` r

estimate_contrasts(m, "x1", predict = "link")
#> Marginal Contrasts Analysis
#> 
#> Level1 | Level2 | Difference |   SE |        95% CI |     z |     p
#> -------------------------------------------------------------------
#> 2      | 1      |      -0.51 | 0.66 | [-1.80, 0.79] | -0.77 | 0.443
#> 3      | 1      |       0.50 | 0.53 | [-0.54, 1.55] |  0.94 | 0.345
#> 3      | 2      |       1.01 | 0.70 | [-0.36, 2.38] |  1.45 | 0.147
#> 
#> Variable predicted: outcome
#> Predictors contrasted: x1
#> Predictors averaged: x2 (10), x3
#> p-values are uncorrected.
#> Contrasts are on the link-scale.
```

The `transform` argument in
[`estimate_contrasts()`](https://easystats.github.io/modelbased/reference/estimate_contrasts.md)
can be used transform comparisons. For example, to transform contrasts
to *odds ratios*, we can use `transform = exp` in combination with
`predict = "link"`.

``` r

estimate_contrasts(m, "x1", predict = "link", transform = exp)
#> Marginal Contrasts Analysis
#> 
#> Level1 | Level2 | Difference |        95% CI |     p
#> ----------------------------------------------------
#> 2      | 1      |       0.60 | [0.16,  2.20] | 0.443
#> 3      | 1      |       1.65 | [0.58,  4.71] | 0.345
#> 3      | 2      |       2.75 | [0.70, 10.78] | 0.147
#> 
#> Variable predicted: outcome
#> Predictors contrasted: x1
#> Predictors averaged: x2 (10), x3
#> p-values are uncorrected.
#> Contrasts are on the link-scale.
```

[Go to next vignette: **Contrasts and Comparisons for Zero-Inflation
Models**](https://easystats.github.io/modelbased/articles/introduction_comparisons_5.html)
