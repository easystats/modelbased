
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

  - **TO BE DONE**.

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
    Predict the response variable based on the model
  - [`estimate_smooth()`](https://easystats.github.io/estimate/reference/estimate_smooth.html):
    Describes a non-linear term (*e.g.* in GAMs) by its linear parts

These functions are powered by the `data_grid` function, a smart tool
for guessing the appropriate reference grid.

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
| virginica  | 2.98   | \[2.89, 3.05\] |

### Contrast analysis

``` r
estimate_contrasts(model)
```

| Level1     | Level2     | Median | 89% CI           | pd     | % in ROPE |
| :--------- | :--------- | :----- | :--------------- | :----- | :-------- |
| setosa     | versicolor | 0.65   | \[0.54, 0.76\]   | 100%   | 0%        |
| setosa     | virginica  | 0.45   | \[0.34, 0.56\]   | 100%   | 0%        |
| versicolor | virginica  | \-0.20 | \[-0.31, -0.09\] | 99.85% | 1.42%     |

### check the contrasts at different points of another linear predictor

``` r
model <- stan_glm(Sepal.Width ~ Species * Petal.Length, data = iris)

estimate_contrasts(model, modulate = "Petal.Length", length = 3)
```

|   | Level1     | Level2     | Petal.Length | Median | 89% CI          | pd     | % in ROPE |
| - | :--------- | :--------- | :----------- | :----- | :-------------- | :----- | :-------- |
| 1 | setosa     | versicolor | 1.00         | 1.54   | \[1.08, 2.01\]  | 100%   | 0%        |
| 4 | setosa     | virginica  | 1.00         | 1.21   | \[0.64, 1.73\]  | 99.95% | 0.05%     |
| 7 | versicolor | virginica  | 1.00         | \-0.33 | \[-1.04, 0.41\] | 76.12% | 5.97%     |
| 2 | setosa     | versicolor | 3.95         | 1.80   | \[1.06, 2.58\]  | 100%   | 0.02%     |
| 5 | setosa     | virginica  | 3.95         | 1.83   | \[1.04, 2.63\]  | 99.98% | 0%        |
| 8 | versicolor | virginica  | 3.95         | 0.03   | \[-0.18, 0.27\] | 58.93% | 24.10%    |
| 3 | setosa     | versicolor | 6.90         | 2.06   | \[0.49, 3.69\]  | 97.35% | 0.40%     |
| 6 | setosa     | virginica  | 6.90         | 2.47   | \[0.84, 4.05\]  | 99.10% | 0.25%     |
| 9 | versicolor | virginica  | 6.90         | 0.39   | \[-0.03, 0.82\] | 92.60% | 4.38%     |

### Find a predictor’s slopes at each factor level

``` r
estimate_slopes(model)
```

| Species    | Median | 89% CI         | pd     | % in ROPE |
| :--------- | :----- | :------------- | :----- | :-------- |
| setosa     | 0.42   | \[0.14, 0.74\] | 98.65% | 4.62%     |
| versicolor | 0.33   | \[0.18, 0.47\] | 99.98% | 0.57%     |
| virginica  | 0.21   | \[0.09, 0.33\] | 99.67% | 7.22%     |

### Generate predictions from your model to compare it with original data

``` r
estimate_response(model)
```

| Species | Petal.Length | Median | CI\_low | CI\_high |
| :------ | -----------: | -----: | ------: | -------: |
| setosa  |          1.4 |   3.40 |    2.89 |     3.91 |
| setosa  |          1.4 |   3.39 |    2.90 |     3.89 |
| setosa  |          1.3 |   3.35 |    2.84 |     3.84 |
| setosa  |          1.5 |   3.45 |    2.97 |     3.98 |
| setosa  |          1.4 |   3.41 |    2.89 |     3.91 |
| setosa  |          1.7 |   3.53 |    2.97 |     4.02 |

### Estimate the link between the response and a predictor

``` r
model <- stan_glm(Sepal.Width ~ poly(Petal.Length, 2), data=iris)

estimate_link(model)
```

| Petal.Length | Median | CI\_low | CI\_high |
| -----------: | -----: | ------: | -------: |
|         1.00 |   3.62 |    3.51 |     3.73 |
|         1.98 |   3.18 |    3.11 |     3.25 |
|         2.97 |   2.90 |    2.82 |     2.97 |
|         3.95 |   2.78 |    2.71 |     2.86 |
|         4.93 |   2.83 |    2.78 |     2.89 |
|         5.92 |   3.05 |    2.97 |     3.15 |
|         6.90 |   3.43 |    3.25 |     3.63 |

### Describe the smooth term by its linear parts

``` r
estimate_smooth(model)
## Part | Start |  End |   Size | Trend | Linearity
## ------------------------------------------------
## 1    |  1.00 | 4.11 | 53.00% | -0.01 |      0.94
## 2    |  4.11 | 6.90 | 47.00% |  0.01 |      0.93
```
