
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

The package is built around 7 main functions:

  - `estimate_means()`: Estimates the average values at each factor
    levels
  - `estimate_contrasts()`: Estimates and tests contrasts between
    different factor levels
  - `estimate_slopes()`: Estimates the slopes of numeric predictors at
    different factor levels
  - `estimate_response()`: Predict the response variable based on the
    model
  - `estimate_fit()`: Estimates the link between variables of the model
    and the response
  - `estimate_smooth()`: Describes a non-linear term (*e.g.* in GAMs)
    through its linear parts
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
| virginica  |   2.97 | 0.05 |    2.90 |     3.06 |

#### Contrast analysis

``` r
estimate_contrasts(model)
```

| Level1     | Level2     | Median |  MAD | CI\_low | CI\_high |     pd | ROPE\_Percentage | ROPE\_Equivalence |
| :--------- | :--------- | -----: | ---: | ------: | -------: | -----: | ---------------: | :---------------- |
| setosa     | versicolor |   0.65 | 0.07 |    0.55 |     0.77 | 100.00 |             0.00 | rejected          |
| setosa     | virginica  |   0.45 | 0.07 |    0.34 |     0.56 | 100.00 |             0.00 | rejected          |
| versicolor | virginica  | \-0.20 | 0.07 |  \-0.32 |   \-0.09 |  99.83 |             6.25 | undecided         |

#### check the contrasts at different points of another linear predictor

``` r
model <- stan_glm(Sepal.Width ~ Species * Petal.Length, data=iris)

estimate_contrasts(model, modulate="Petal.Length", length=3)
```

| Level1     | Level2     | Petal.Length | Median |  MAD | CI\_low | CI\_high |    pd | ROPE\_Percentage | ROPE\_Equivalence |
| :--------- | :--------- | -----------: | -----: | ---: | ------: | -------: | ----: | ---------------: | :---------------- |
| setosa     | versicolor |         3.76 |   1.76 | 0.44 |    0.96 |     2.44 | 100.0 |              0.0 | rejected          |
| setosa     | virginica  |         3.76 |   1.77 | 0.49 |    1.03 |     2.60 | 100.0 |              0.0 | rejected          |
| versicolor | virginica  |         3.76 |   0.01 | 0.16 |  \-0.25 |     0.27 |  52.7 |             47.9 | undecided         |

#### Find a predictor’s slopes at each factor level

``` r
estimate_slopes(model)
```

| Species    | Median |  MAD | CI\_low | CI\_high |    pd | ROPE\_Percentage | ROPE\_Equivalence |
| :--------- | -----: | ---: | ------: | -------: | ----: | ---------------: | :---------------- |
| setosa     |   0.41 | 0.19 |    0.10 |     0.71 | 98.67 |             4.65 | undecided         |
| versicolor |   0.33 | 0.09 |    0.18 |     0.47 | 99.98 |             0.60 | rejected          |
| virginica  |   0.21 | 0.08 |    0.08 |     0.34 | 99.88 |             7.20 | undecided         |

#### Generate predictions from your model to compare it with original data

``` r
estimate_response(model)
```

| Species | Petal.Length | Median |  MAD | CI\_low | CI\_high |
| :------ | -----------: | -----: | ---: | ------: | -------: |
| setosa  |          1.4 |   3.41 | 0.33 |    2.87 |     3.91 |
| setosa  |          1.4 |   3.40 | 0.32 |    2.90 |     3.96 |
| setosa  |          1.3 |   3.36 | 0.32 |    2.86 |     3.91 |
| setosa  |          1.5 |   3.44 | 0.32 |    2.95 |     4.00 |
| setosa  |          1.4 |   3.40 | 0.31 |    2.89 |     3.94 |
| setosa  |          1.7 |   3.53 | 0.32 |    2.95 |     4.01 |

#### Estimate the link between the response and a predictor

``` r
model <- stan_glm(Sepal.Width ~ poly(Petal.Length, 2), data=iris)

estimate_fit(model)
```

| Petal.Length | Median |  MAD | CI\_low | CI\_high |
| -----------: | -----: | ---: | ------: | -------: |
|         1.00 |   3.62 | 0.07 |    3.53 |     3.74 |
|         1.98 |   3.18 | 0.04 |    3.11 |     3.25 |
|         2.97 |   2.90 | 0.05 |    2.82 |     2.98 |
|         3.95 |   2.78 | 0.05 |    2.71 |     2.86 |
|         4.93 |   2.83 | 0.04 |    2.77 |     2.90 |
|         5.92 |   3.05 | 0.06 |    2.97 |     3.15 |
|         6.90 |   3.44 | 0.12 |    3.24 |     3.63 |

#### Describe the smooth term

``` r
estimate_smooth(model)
```

| Part |    Start |      End | Size |       Trend | Smoothness |
| ---: | -------: | -------: | ---: | ----------: | ---------: |
|    1 | 1.000000 | 4.113065 | 0.53 | \-0.0080029 |  0.9989059 |
|    2 | 4.113065 | 6.900000 | 0.47 |   0.0068667 |  0.9989540 |
