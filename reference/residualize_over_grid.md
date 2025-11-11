# Compute partial residuals from a data grid

This function computes partial residuals based on a data grid, where the
data grid is usually a data frame from all combinations of factor
variables or certain values of numeric vectors. This data grid is
usually used as `newdata` argument in
[`predict()`](https://rdrr.io/r/stats/predict.html), and can be created
with
[`insight::get_datagrid()`](https://easystats.github.io/insight/reference/get_datagrid.html).

## Usage

``` r
residualize_over_grid(grid, model, ...)

# S3 method for class 'data.frame'
residualize_over_grid(grid, model, predictor_name, ...)
```

## Arguments

- grid:

  A data frame representing the data grid, or an object of class
  `estimate_means` or `estimate_predicted`, as returned by the different
  `estimate_*()` functions.

- model:

  The model for which to compute partial residuals. The data grid `grid`
  should match to predictors in the model.

- ...:

  Currently not used.

- predictor_name:

  The name of the focal predictor, for which partial residuals are
  computed.

## Value

A data frame with residuals for the focal predictor.

## Partial Residuals

For **generalized linear models** (glms), residualized scores are
computed as `inv.link(link(Y) + r)` where `Y` are the predicted values
on the response scale, and `r` are the *working* residuals.

For (generalized) linear **mixed models**, the random effect are also
partialled out.

## References

Fox J, Weisberg S. Visualizing Fit and Lack of Fit in Complex Regression
Models with Predictor Effect Plots and Partial Residuals. Journal of
Statistical Software 2018;87.

## Examples

``` r
set.seed(1234)
x1 <- rnorm(200)
x2 <- rnorm(200)
# quadratic relationship
y <- 2 * x1 + x1^2 + 4 * x2 + rnorm(200)

d <- data.frame(x1, x2, y)
model <- lm(y ~ x1 + x2, data = d)

pr <- estimate_means(model, c("x1", "x2"))
head(residualize_over_grid(pr, model))
#>          x1    x2     Mean
#> 37   -0.889 0.814 1.668084
#> 57    0.422 0.814 4.203154
#> 66    1.077 0.112 3.374450
#> 17   -2.200 0.814 3.450142
#> 56    0.422 0.112 1.943842
#> 57.1  0.422 0.814 4.484100
```
