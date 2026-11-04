# Pool contrasts and comparisons from `estimate_contrasts()`

This function "pools" (i.e. combines) multiple `estimate_contrasts`
objects, returned by
[`estimate_contrasts()`](https://easystats.github.io/modelbased/reference/estimate_contrasts.md),
in a similar fashion as
[`mice::pool()`](https://amices.org/mice/reference/pool.html).

## Usage

``` r
pool_contrasts(x, ...)
```

## Arguments

- x:

  A list of `estimate_contrasts` objects, as returned by
  [`estimate_contrasts()`](https://easystats.github.io/modelbased/reference/estimate_contrasts.md).

- ...:

  Currently not used.

## Value

A data frame with pooled comparisons or contrasts of predictions.

## Details

Averaging of parameters follows Rubin's rules (*Rubin, 1987, p. 76*).

## References

Rubin, D.B. (1987). Multiple Imputation for Nonresponse in Surveys. New
York: John Wiley and Sons.

## Examples

``` r
data("nhanes2", package = "mice")
imp <- mice::mice(nhanes2, printFlag = FALSE)
comparisons <- lapply(1:5, function(i) {
  m <- lm(bmi ~ age + hyp + chl, data = mice::complete(imp, action = i))
  estimate_contrasts(m, "age")
})
pool_contrasts(comparisons)
#> Marginal Contrasts Analysis
#> 
#> Level1 | Level2 | Difference |   SE |          95% CI |     t |    df |     p
#> -----------------------------------------------------------------------------
#> 40-59  | 20-39  |      -4.56 | 2.20 | [ -9.55,  0.43] | -2.07 |  8.82 | 0.069
#> 60-99  | 20-39  |      -6.49 | 2.70 | [-12.86, -0.13] | -2.40 |  7.10 | 0.047
#> 60-99  | 40-59  |      -1.93 | 2.17 | [ -6.58,  2.72] | -0.89 | 14.40 | 0.388
#> 
#> Variable predicted: bmi
#> Predictors contrasted: age
#> Predictors averaged: hyp, chl (2e+02)
#> p-values are uncorrected.
#> 
```
