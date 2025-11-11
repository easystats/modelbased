# Describe the smooth term (for GAMs) or non-linear predictors

This function summarises the smooth term trend in terms of linear
segments. Using the approximate derivative, it separates a non-linear
vector into quasi-linear segments (in which the trend is either positive
or negative). Each of this segment its characterized by its beginning,
end, size (in proportion, relative to the total size) trend (the linear
regression coefficient) and linearity (the R2 of the linear regression).

## Usage

``` r
describe_nonlinear(data, ...)

# S3 method for class 'data.frame'
describe_nonlinear(data, x = NULL, y = NULL, ...)

estimate_smooth(data, ...)
```

## Arguments

- data:

  The data containing the link, as for instance obtained by
  [`estimate_relation()`](https://easystats.github.io/modelbased/reference/estimate_expectation.md).

- ...:

  Other arguments to be passed to or from.

- x, y:

  The name of the responses variable (`y`) predicting variable (`x`).

## Value

A data frame of linear description of non-linear terms.

## Examples

``` r
# Create data
data <- data.frame(x = rnorm(200))
data$y <- data$x^2 + rnorm(200, 0, 0.5)

model <<- lm(y ~ poly(x, 2), data = data)
link_data <- estimate_relation(model, length = 100)

describe_nonlinear(link_data, x = "x")
#> Start |   End | Length | Change | Slope |   R2
#> ----------------------------------------------
#> -2.61 | -0.06 |   0.47 |  -6.58 | -2.58 | 0.02
#> -0.06 |  2.75 |   0.52 |   7.53 |  2.67 | 0.02
```
