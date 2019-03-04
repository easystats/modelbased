
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

The package is built around 5 main functions:

  - `estimate_means()`: Estimates the average values at each factor
    levels
  - `estimate_contrasts()`: Estimates and tests contrasts between
    different factor levels
  - `estimate_slopes()`: Estimates the slopes of numeric predictors at
    different factor levels
  - `estimate_response()`: Estimates the slopes of numeric predictors at
    different factor levels
  - `data_grid()`: Creates a reference grid of provided data or model

## Examples

#### Estimate marginal means

``` r
library(rstanarm)
model <- stan_glm(Sepal.Width ~ Species, data=iris)

estimate_means(model)
```

| Species    | Median |  MAD | CI\_low | CI\_high |
| :--------- | -----: | ---: | ------: | -------: |
| setosa     |   3.43 | 0.05 |    3.35 |     3.50 |
| versicolor |   2.77 | 0.05 |    2.69 |     2.85 |
| virginica  |   2.98 | 0.05 |    2.90 |     3.05 |

#### Contrast analysis

``` r
estimate_contrasts(model)
```

| Level1     | Level2     | Median |  MAD | CI\_low | CI\_high |    pd | ROPE\_Percentage | ROPE\_Equivalence |
| :--------- | :--------- | -----: | ---: | ------: | -------: | ----: | ---------------: | :---------------- |
| setosa     | versicolor |   0.65 | 0.07 |    0.55 |     0.77 | 100.0 |             0.00 | rejected          |
| setosa     | virginica  |   0.45 | 0.07 |    0.34 |     0.56 | 100.0 |             0.00 | rejected          |
| versicolor | virginica  | \-0.20 | 0.07 |  \-0.32 |   \-0.09 |  99.9 |             6.78 | undecided         |

#### Find a predictor’s slopes at each factor level

``` r
model <- stan_glm(Sepal.Width ~ Species * Petal.Length, data=iris)

estimate_slopes(model)
```

| params     | Median |  MAD | CI\_low | CI\_high |    pd | ROPE\_Percentage | ROPE\_Equivalence |
| :--------- | -----: | ---: | ------: | -------: | ----: | ---------------: | :---------------- |
| setosa     |   0.41 | 0.19 |    0.11 |     0.73 | 98.75 |             4.42 | undecided         |
| versicolor |   0.33 | 0.09 |    0.18 |     0.47 | 99.98 |             0.65 | rejected          |
| virginica  |   0.21 | 0.07 |    0.09 |     0.33 | 99.70 |             7.15 | undecided         |

#### Make predictions based on your model to compare it with original data

``` r
model %>% 
  estimate_response()
```

| Species | Petal.Length | Median |  MAD | CI\_low | CI\_high |
| :------ | -----------: | -----: | ---: | ------: | -------: |
| setosa  |          1.4 |   3.39 | 0.32 |    2.87 |     3.93 |
| setosa  |          1.4 |   3.39 | 0.33 |    2.85 |     3.91 |
| setosa  |          1.3 |   3.35 | 0.32 |    2.85 |     3.93 |
| setosa  |          1.5 |   3.44 | 0.31 |    2.92 |     3.96 |
| setosa  |          1.4 |   3.40 | 0.32 |    2.90 |     3.95 |
| setosa  |          1.7 |   3.52 | 0.32 |    3.03 |     4.05 |

#### Make predictions on a reference grid to help visualising the effects

``` r
model %>% 
  estimate_response(data="grid", length=3, predict="link")
```

| Species    | Petal.Length | Median |  MAD | CI\_low | CI\_high |
| :--------- | -----------: | -----: | ---: | ------: | -------: |
| setosa     |         1.00 |   3.23 | 0.10 |    3.07 |     3.39 |
| versicolor |         1.00 |   1.70 | 0.29 |    1.20 |     2.16 |
| virginica  |         1.00 |   2.01 | 0.34 |    1.46 |     2.60 |
| setosa     |         3.95 |   4.45 | 0.48 |    3.64 |     5.20 |
| versicolor |         3.95 |   2.67 | 0.05 |    2.58 |     2.76 |
| virginica  |         3.95 |   2.64 | 0.13 |    2.42 |     2.84 |
| setosa     |         6.90 |   5.66 | 1.03 |    3.90 |     7.29 |
| versicolor |         6.90 |   3.64 | 0.24 |    3.26 |     4.04 |
| virginica  |         6.90 |   3.26 | 0.11 |    3.08 |     3.45 |
