
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
    means**](https://easystats.github.io/estimate/articles/marginal_means.html)
  - [**Contrast
    analysis**](https://easystats.github.io/estimate/articles/contrast_analysis.html)
  - [**Use a model to make
    predictions**](https://easystats.github.io/estimate/articles/predictions.html)

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
| setosa     | 3.43   | \[3.35, 3.50\] |
| versicolor | 2.77   | \[2.69, 2.85\] |
| virginica  | 2.98   | \[2.90, 3.05\] |

### Contrast analysis

``` r
estimate_contrasts(model)
```

| Level1     | Level2     | Median | 89% CI           | pd     | % in ROPE | Median (std.) |
| :--------- | :--------- | :----- | :--------------- | :----- | :-------- | :------------ |
| setosa     | versicolor | 0.66   | \[0.55, 0.77\]   | 100%   | 0%        | 1.50          |
| setosa     | virginica  | 0.45   | \[0.34, 0.56\]   | 100%   | 0%        | 1.04          |
| versicolor | virginica  | \-0.20 | \[-0.33, -0.10\] | 99.70% | 6.88%     | \-0.47        |

### Check the contrasts at different points of another linear predictor

``` r
model <- stan_glm(Sepal.Width ~ Species * Petal.Length, data = iris)

estimate_contrasts(model, modulate = "Petal.Length", length = 3)
```

|   | Level1     | Level2     | Petal.Length | Median | 89% CI          | pd     | % in ROPE | Median (std.) |
| - | :--------- | :--------- | :----------- | :----- | :-------------- | :----- | :-------- | :------------ |
| 1 | setosa     | versicolor | 1.00         | 1.54   | \[1.06, 1.99\]  | 100%   | 0%        | 3.53          |
| 4 | setosa     | virginica  | 1.00         | 1.20   | \[0.69, 1.80\]  | 100%   | 0.02%     | 2.76          |
| 7 | versicolor | virginica  | 1.00         | \-0.34 | \[-0.98, 0.44\] | 78.03% | 12.85%    | \-0.78        |
| 2 | setosa     | versicolor | 3.95         | 1.78   | \[0.98, 2.48\]  | 100%   | 0%        | 4.09          |
| 5 | setosa     | virginica  | 3.95         | 1.81   | \[1.01, 2.59\]  | 99.98% | 0.05%     | 4.15          |
| 8 | versicolor | virginica  | 3.95         | 0.03   | \[-0.20, 0.24\] | 58.53% | 52.90%    | 0.06          |
| 3 | setosa     | versicolor | 6.90         | 2.04   | \[0.27, 3.41\]  | 97.85% | 0.95%     | 4.68          |
| 6 | setosa     | virginica  | 6.90         | 2.41   | \[0.88, 4.04\]  | 99.15% | 0.45%     | 5.53          |
| 9 | versicolor | virginica  | 6.90         | 0.39   | \[-0.04, 0.80\] | 92.42% | 9.75%     | 0.91          |

### Find a predictor’s slopes at each factor level

``` r
estimate_slopes(model)
```

| Species    | Median | 89% CI         | pd     | % in ROPE | Median (std.) |
| :--------- | :----- | :------------- | :----- | :-------- | :------------ |
| setosa     | 0.41   | \[0.12, 0.70\] | 98.65% | 4.08%     | 1.67          |
| versicolor | 0.33   | \[0.19, 0.48\] | 99.98% | 0.70%     | 1.34          |
| virginica  | 0.21   | \[0.09, 0.33\] | 99.80% | 7.42%     | 0.85          |

### Generate predictions from your model to compare it with original data

``` r
estimate_response(model)
```

| Species | Petal.Length | Median | CI\_low | CI\_high |
| :------ | -----------: | -----: | ------: | -------: |
| setosa  |          1.4 |   3.40 |    2.89 |     3.91 |
| setosa  |          1.4 |   3.40 |    2.86 |     3.88 |
| setosa  |          1.3 |   3.36 |    2.87 |     3.89 |
| setosa  |          1.5 |   3.45 |    2.94 |     3.96 |
| setosa  |          1.4 |   3.41 |    2.88 |     3.89 |
| setosa  |          1.7 |   3.52 |    3.01 |     4.07 |

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
|         3.95 |   2.78 |    2.71 |     2.86 |
|         4.93 |   2.83 |    2.77 |     2.90 |
|         5.92 |   3.05 |    2.96 |     3.13 |
|         6.90 |   3.44 |    3.24 |     3.62 |

### Describe the smooth term by its linear parts

``` r
estimate_smooth(model)
## Part | Start |  End |   Size | Trend | Linearity
## ------------------------------------------------
## 1    |  1.00 | 4.11 | 53.00% | -0.01 |      0.94
## 2    |  4.11 | 6.90 | 47.00% |  0.01 |      0.93
```
