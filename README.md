
# estimate <img src='man/figures/logo.png' align="right" height="139" />

[![CRAN](http://www.r-pkg.org/badges/version/estimate)](https://cran.r-project.org/package=estimate)
[![downloads](http://cranlogs.r-pkg.org/badges/estimate)](https://cran.r-project.org/package=estimate)
[![Build
Status](https://travis-ci.org/easystats/estimate.svg?branch=master)](https://travis-ci.org/easystats/estimate)
[![codecov](https://codecov.io/gh/easystats/estimate/branch/master/graph/badge.svg)](https://codecov.io/gh/easystats/estimate)

`estimate` is a lightweight package helping with model-based
estimations, used in the computation of marginal means, contrast
analysis and predictions.

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

[![Documentation](https://img.shields.io/badge/documentation-estimate-orange.svg?colorB=E91E63)](https://easystats.github.io/estimate/)
[![Blog](https://img.shields.io/badge/blog-easystats-orange.svg?colorB=FF9800)](https://easystats.github.io/blog/posts/)
[![Features](https://img.shields.io/badge/features-estimate-orange.svg?colorB=2196F3)](https://easystats.github.io/estimate/reference/index.html)

Click on the buttons above to access the package
[**documentation**](https://easystats.github.io/estimate/) and the
[**easystats blog**](https://easystats.github.io/blog/posts/), and
check-out these vignettes:

  - [**Data
    grids**](https://easystats.github.io/estimate/articles/data_grid.html)
  - [**Marginal
    means**](https://easystats.github.io/estimate/articles/estimate_means.html)
  - [**Contrast
    analysis**](https://easystats.github.io/estimate/articles/estimate_contrasts.html)
  - [**Use a model to make
    predictions**](https://easystats.github.io/estimate/articles/estimate_response.html)
  - [**Describe non-linear
    curves**](https://easystats.github.io/estimate/articles/estimate_smooth.html)

# Features

The package is built around 5 main functions:

  - [`estimate_means()`](https://easystats.github.io/estimate/reference/estimate_means.html):
    Estimates the average values at each factor levels
  - [`estimate_contrasts()`](https://easystats.github.io/estimate/reference/estimate_contrasts.html):
    Estimates and tests contrasts between different factor levels
  - [`estimate_slopes()`](https://easystats.github.io/estimate/reference/estimate_slopes.html):
    Estimates the slopes of numeric predictors at different factor
    levels
  - [`estimate_response()`](https://easystats.github.io/estimate/reference/estimate_response.html):
    Predict the response variable using the model
  - [`estimate_smooth()`](https://easystats.github.io/estimate/reference/estimate_smooth.html):
    Describes a non-linear term (*e.g.* in GAMs) by its linear parts

These functions are powered by the
[`data_grid()`](https://easystats.github.io/estimate/reference/data_grid.html)
function, a smart tool for guessing the appropriate reference grid.

The package currently only supports `rstanarm` models, but will be
expanded to cover a large variety of frequentist and Bayesian models.

## Examples

### Estimate marginal means

``` r
library(rstanarm)

model <- stan_glm(Sepal.Width ~ Species, data = iris)

estimate_means(model)
```

| Species    | Median | 89% CI         |
| :--------- | :----- | :------------- |
| setosa     | 3.43   | \[3.34, 3.50\] |
| versicolor | 2.77   | \[2.69, 2.84\] |
| virginica  | 2.97   | \[2.90, 3.05\] |

### Contrast analysis

``` r
estimate_contrasts(model)
```

| Level1     | Level2     | Median | 89% CI           | pd     | % in ROPE | Median (std.) |
| :--------- | :--------- | :----- | :--------------- | :----- | :-------- | :------------ |
| setosa     | versicolor | 0.66   | \[0.54, 0.76\]   | 100%   | 0%        | 1.51          |
| setosa     | virginica  | 0.45   | \[0.34, 0.56\]   | 100%   | 0%        | 1.04          |
| versicolor | virginica  | \-0.20 | \[-0.31, -0.10\] | 99.88% | 6.02%     | \-0.46        |

### Check the contrasts at different points of another linear predictor

``` r
model <- stan_glm(Sepal.Width ~ Species * Petal.Length, data = iris)

estimate_contrasts(model, modulate = "Petal.Length", length = 3)
```

| Level1     | Level2     | Petal.Length | Median | 89% CI          | pd     | % in ROPE | Median (std.) |
| :--------- | :--------- | :----------- | :----- | :-------------- | :----- | :-------- | :------------ |
| setosa     | versicolor | 1.00         | 1.53   | \[1.06, 2.02\]  | 100%   | 0%        | 3.51          |
| setosa     | virginica  | 1.00         | 1.22   | \[0.64, 1.74\]  | 99.92% | 0.20%     | 2.79          |
| versicolor | virginica  | 1.00         | \-0.32 | \[-1.05, 0.39\] | 75.72% | 13.58%    | \-0.73        |
| setosa     | versicolor | 3.95         | 1.78   | \[1.04, 2.54\]  | 100%   | 0%        | 4.09          |
| setosa     | virginica  | 3.95         | 1.82   | \[0.95, 2.58\]  | 100%   | 0.02%     | 4.17          |
| versicolor | virginica  | 3.95         | 0.03   | \[-0.20, 0.24\] | 58.93% | 51.25%    | 0.07          |
| setosa     | versicolor | 6.90         | 2.04   | \[0.45, 3.66\]  | 97.50% | 0.95%     | 4.69          |
| setosa     | virginica  | 6.90         | 2.43   | \[0.81, 3.98\]  | 99.05% | 0.52%     | 5.58          |
| versicolor | virginica  | 6.90         | 0.38   | \[-0.04, 0.81\] | 92.17% | 11.60%    | 0.87          |

### Find a predictor’s slopes at each factor level

``` r
estimate_slopes(model)
```

| Species    | Median | 89% CI         | pd     | % in ROPE | Median (std.) |
| :--------- | :----- | :------------- | :----- | :-------- | :------------ |
| setosa     | 0.41   | \[0.12, 0.71\] | 98.47% | 4.70%     | 1.68          |
| versicolor | 0.33   | \[0.19, 0.48\] | 100%   | 0.42%     | 1.32          |
| virginica  | 0.21   | \[0.09, 0.33\] | 99.52% | 7.80%     | 0.85          |

### Generate predictions from your model to compare it with original data

``` r
estimate_response(model)
```

| Species | Petal.Length | Median | CI\_low | CI\_high |
| :------ | -----------: | -----: | ------: | -------: |
| setosa  |          1.4 |   3.40 |    2.91 |     3.93 |
| setosa  |          1.4 |   3.41 |    2.93 |     3.96 |
| setosa  |          1.3 |   3.36 |    2.87 |     3.89 |
| setosa  |          1.5 |   3.43 |    2.94 |     3.95 |
| setosa  |          1.4 |   3.40 |    2.90 |     3.91 |
| setosa  |          1.7 |   3.51 |    2.99 |     4.03 |

### Estimate the link between the response and a predictor

``` r
model <- stan_glm(Sepal.Width ~ poly(Petal.Length, 2), data=iris)

estimate_link(model)
```

| Petal.Length | Median | CI\_low | CI\_high |
| -----------: | -----: | ------: | -------: |
|         1.00 |   3.62 |    3.52 |     3.73 |
|         1.98 |   3.18 |    3.11 |     3.24 |
|         2.97 |   2.90 |    2.82 |     2.97 |
|         3.95 |   2.78 |    2.70 |     2.85 |
|         4.93 |   2.83 |    2.77 |     2.89 |
|         5.92 |   3.05 |    2.96 |     3.14 |
|         6.90 |   3.44 |    3.26 |     3.64 |

### Describe the smooth term by its linear parts

``` r
estimate_smooth(model)
## Part | Start |  End |   Size | Trend | Linearity
## ------------------------------------------------
## 1    |  1.00 | 4.11 | 53.00% | -0.01 |      0.94
## 2    |  4.11 | 6.90 | 47.00% |  0.01 |      0.93
```
