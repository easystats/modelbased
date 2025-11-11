# Pool Predictions and Estimated Marginal Means

This function "pools" (i.e. combines) multiple `estimate_means` objects,
in a similar fashion as
[`mice::pool()`](https://amices.org/mice/reference/pool.html).

## Usage

``` r
pool_predictions(x, transform = NULL, ...)

pool_slopes(x, transform = NULL, ...)
```

## Arguments

- x:

  A list of `estimate_means` objects, as returned by
  [`estimate_means()`](https://easystats.github.io/modelbased/reference/estimate_means.md),
  or `estimate_predicted` objects, as returned by
  [`estimate_relation()`](https://easystats.github.io/modelbased/reference/estimate_expectation.md)
  and related functions. For `pool_slopes()`, must be a list of
  `estimate_slopes` objects, as returned by
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

- ...:

  Currently not used.

## Value

A data frame with pooled predictions.

## Details

Averaging of parameters follows Rubin's rules (*Rubin, 1987, p. 76*).
Pooling is applied to the predicted values and based on the standard
errors as they are calculated in the `estimate_means` or
`estimate_predicted` objects provided in `x`. For objects of class
`estimate_means`, the predicted values are on the response scale by
default, and standard errors are calculated using the delta method.
Then, pooling estimates and calculating standard errors for the pooled
estimates based ob Rubin's rule is carried out. There is no
back-transformation to the link-scale of predicted values before
applying Rubin's rule.

## References

Rubin, D.B. (1987). Multiple Imputation for Nonresponse in Surveys. New
York: John Wiley and Sons.

## Examples

``` r
# example for multiple imputed datasets
data("nhanes2", package = "mice")
imp <- mice::mice(nhanes2, printFlag = FALSE)

# estimated marginal means
predictions <- lapply(1:5, function(i) {
  m <- lm(bmi ~ age + hyp + chl, data = mice::complete(imp, action = i))
  estimate_means(m, "age")
})
pool_predictions(predictions)
#> Estimated Marginal Means
#> 
#> age   |  Mean |   SE |        95% CI |  t(1)
#> --------------------------------------------
#> 20-39 | 30.18 | 1.88 | [6.30, 54.06] | 16.06
#> 40-59 | 25.29 | 1.60 | [5.01, 45.56] | 15.85
#> 60-99 | 23.21 | 1.77 | [0.74, 45.69] | 13.12
#> 
#> Variable predicted: bmi
#> Predictors modulated: age
#> Predictors averaged: hyp, chl (1.9e+02)
#> 

# estimated slopes (marginal effects)
slopes <- lapply(1:5, function(i) {
  m <- lm(bmi ~ age + hyp + chl, data = mice::complete(imp, action = i))
  estimate_slopes(m, "chl")
})
pool_slopes(slopes)
#> Estimated Marginal Effects
#> 
#> Slope |   SE |        95% CI | t(20) |     p
#> --------------------------------------------
#> 0.05  | 0.03 | [ 0.00, 0.11] |  2.02 | 0.057
#> 
#> Marginal effects estimated for chl
#> Type of slope was dY/dX
```
