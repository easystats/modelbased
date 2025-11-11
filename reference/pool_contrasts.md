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
#> Level1 | Level2 | Difference |   SE |          95% CI |  t(1) |     p
#> ---------------------------------------------------------------------
#> 40-59  | 20-39  |      -4.36 | 2.37 | [-34.41, 25.70] | -1.84 | 0.317
#> 60-99  | 20-39  |      -5.28 | 2.77 | [-40.43, 29.88] | -1.91 | 0.308
#> 60-99  | 40-59  |      -0.92 | 2.16 | [-28.32, 26.48] | -0.43 | 0.743
#> 
#> Variable predicted: bmi
#> Predictors contrasted: age
#> Predictors averaged: hyp, chl (2e+02)
#> p-values are uncorrected.
#> 
```
