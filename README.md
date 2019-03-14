
# estimate <img src='man/figures/logo.png' align="right" height="139" />

[![Build
Status](https://travis-ci.org/easystats/estimate.svg?branch=master)](https://travis-ci.org/easystats/estimate)
[![codecov](https://codecov.io/gh/easystats/estimate/branch/master/graph/badge.svg)](https://codecov.io/gh/easystats/estimate)
[![HitCount](http://hits.dwyl.io/easystats/estimate.svg)](http://hits.dwyl.io/easystats/estimate)
[![Documentation](https://img.shields.io/badge/documentation-estimate-orange.svg?colorB=E91E63)](https://easystats.github.io/estimate/)

`estimate` is a lightweight package helping with model-based
estimations, used in the computation of marginal means, contrast
analysis, predictions and such.

## Installation

Run the following:

``` r
install.packages("devtools")
devtools::install_github("easystats/estimate")
```

``` r
library("estimate")
```

## Documentation

The package documentation can be found
[**here**](https://easystats.github.io/estimate/).

# Features

The package is built around 7 main
    functions:

  - [`estimate_means()`](https://easystats.github.io/estimate/reference/estimate_means.html):
    Estimates the average values at each factor
    levels
  - [`estimate_contrasts()`](https://easystats.github.io/estimate/reference/estimate_contrasts.html):
    Estimates and tests contrasts between different factor
    levels
  - [`estimate_slopes()`](https://easystats.github.io/estimate/reference/estimate_slopes.html):
    Estimates the slopes of numeric predictors at different factor
    levels
  - [`estimate_response()`](https://easystats.github.io/estimate/reference/estimate_response.html):
    Predict the response variable based on the
    model
  - [`estimate_fit()`](https://easystats.github.io/estimate/reference/estimate_response.html):
    Estimates the link between variables of the model and the
    response
  - [`estimate_smooth()`](https://easystats.github.io/estimate/reference/estimate_smooth.html):
    Describes a non-linear term (*e.g.* in GAMs) by its linear
    parts
  - [`data_grid()`](https://easystats.github.io/estimate/reference/data_grid.html):
    Creates a reference grid of provided data or model

## Examples

### Estimate marginal means

``` r
library(rstanarm)

model <- stan_glm(Sepal.Width ~ Species, data=iris)

estimate_means(model)
```

| Species    | Median |  MAD | CI\_low | CI\_high |
| :--------- | -----: | ---: | ------: | -------: |
| setosa     |   3.43 | 0.05 |    3.35 |     3.50 |
| versicolor |   2.77 | 0.05 |    2.69 |     2.85 |
| virginica  |   2.97 | 0.05 |    2.90 |     3.06 |

### Contrast analysis

``` r
estimate_contrasts(model)
```

| Level1     | Level2     | Median |  MAD | CI\_low | CI\_high |     pd | ROPE\_Percentage | ROPE\_Equivalence |
| :--------- | :--------- | -----: | ---: | ------: | -------: | -----: | ---------------: | :---------------- |
| setosa     | versicolor |   0.66 | 0.07 |    0.54 |     0.77 | 100.00 |             0.00 | rejected          |
| setosa     | virginica  |   0.45 | 0.07 |    0.33 |     0.56 | 100.00 |             0.00 | rejected          |
| versicolor | virginica  | \-0.21 | 0.07 |  \-0.32 |   \-0.09 |  99.85 |             6.25 | undecided         |

### check the contrasts at different points of another linear predictor

``` r
model <- stan_glm(Sepal.Width ~ Species * Petal.Length, data=iris)

estimate_contrasts(model, modulate="Petal.Length", length=3)
```

| Level1     | Level2     | Petal.Length | Median |  MAD | CI\_low | CI\_high |     pd | ROPE\_Percentage | ROPE\_Equivalence |
| :--------- | :--------- | -----------: | -----: | ---: | ------: | -------: | -----: | ---------------: | :---------------- |
| setosa     | versicolor |         1.00 |   1.53 | 0.31 |    1.05 |     2.04 | 100.00 |             0.00 | rejected          |
| setosa     | virginica  |         1.00 |   1.22 | 0.35 |    0.65 |     1.80 | 100.00 |             0.05 | rejected          |
| versicolor | virginica  |         1.00 | \-0.32 | 0.46 |  \-1.05 |     0.46 |  75.98 |            12.62 | undecided         |
| setosa     | versicolor |         3.95 |   1.78 | 0.48 |    1.01 |     2.58 | 100.00 |             0.00 | rejected          |
| setosa     | virginica  |         3.95 |   1.81 | 0.52 |    0.96 |     2.62 |  99.98 |             0.05 | rejected          |
| versicolor | virginica  |         3.95 |   0.03 | 0.14 |  \-0.19 |     0.27 |  59.05 |            51.62 | undecided         |
| setosa     | versicolor |         6.90 |   2.00 | 1.02 |    0.43 |     3.77 |  97.58 |             1.15 | undecided         |
| setosa     | virginica  |         6.90 |   2.39 | 1.01 |    0.84 |     4.18 |  99.02 |             0.45 | rejected          |
| versicolor | virginica  |         6.90 |   0.39 | 0.28 |  \-0.03 |     0.83 |  93.17 |            10.53 | undecided         |

### Find a predictor’s slopes at each factor level

``` r
estimate_slopes(model)
```

| Species    | Median |  MAD | CI\_low | CI\_high |    pd | ROPE\_Percentage | ROPE\_Equivalence |
| :--------- | -----: | ---: | ------: | -------: | ----: | ---------------: | :---------------- |
| setosa     |   0.41 | 0.19 |    0.12 |     0.75 | 98.72 |             4.47 | undecided         |
| versicolor |   0.33 | 0.10 |    0.18 |     0.48 | 99.95 |             0.45 | rejected          |
| virginica  |   0.21 | 0.08 |    0.08 |     0.33 | 99.72 |             7.32 | undecided         |

### Generate predictions from your model to compare it with original data

``` r
estimate_response(model)
```

| Species | Petal.Length | Median |  MAD | CI\_low | CI\_high |
| :------ | -----------: | -----: | ---: | ------: | -------: |
| setosa  |          1.4 |   3.39 | 0.32 |    2.86 |     3.92 |
| setosa  |          1.4 |   3.41 | 0.32 |    2.85 |     3.88 |
| setosa  |          1.3 |   3.36 | 0.31 |    2.86 |     3.87 |
| setosa  |          1.5 |   3.43 | 0.32 |    2.93 |     3.99 |
| setosa  |          1.4 |   3.39 | 0.32 |    2.89 |     3.93 |
| setosa  |          1.7 |   3.52 | 0.31 |    3.00 |     4.04 |

### Estimate the link between the response and a predictor

``` r
model <- stan_glm(Sepal.Width ~ poly(Petal.Length, 2), data=iris)

estimate_fit(model)
```

| Petal.Length | Median |  MAD | CI\_low | CI\_high |
| -----------: | -----: | ---: | ------: | -------: |
|         1.00 |   3.62 | 0.07 |    3.51 |     3.73 |
|         1.98 |   3.18 | 0.04 |    3.11 |     3.24 |
|         2.97 |   2.90 | 0.05 |    2.82 |     2.98 |
|         3.95 |   2.78 | 0.05 |    2.71 |     2.86 |
|         4.93 |   2.83 | 0.04 |    2.77 |     2.89 |
|         5.92 |   3.05 | 0.05 |    2.96 |     3.14 |
|         6.90 |   3.44 | 0.12 |    3.24 |     3.63 |

### Describe the smooth term

``` r
estimate_smooth(model)
```

| Part |    Start |      End |  Size |       Trend | Linearity |
| ---: | -------: | -------: | ----: | ----------: | --------: |
|    1 | 1.000000 | 4.083417 | 0.525 | \-0.0080528 | 0.9404634 |
|    2 | 4.083417 | 6.900000 | 0.475 |   0.0068805 | 0.9318788 |
