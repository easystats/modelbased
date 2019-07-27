
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

![](man/figures/unnamed-chunk-4-1.png)<!-- -->

``` r
library(rstanarm)

model <- stan_glm(Sepal.Width ~ Species, data = iris)

estimate_means(model)
```

    ## Species    | Median |       89% CI
    ## ----------------------------------
    ## setosa     |   3.43 | [3.34, 3.50]
    ## versicolor |   2.77 | [2.69, 2.85]
    ## virginica  |   2.97 | [2.90, 3.05]

### Contrast analysis

``` r
estimate_contrasts(model)
## Level1     |     Level2 | Median |         89% CI |     pd | % in ROPE | Median (std.)
## --------------------------------------------------------------------------------------
## setosa     | versicolor |   0.65 |   [0.55, 0.76] |   100% |        0% |          1.50
## setosa     |  virginica |   0.45 |   [0.34, 0.56] |   100% |        0% |          1.04
## versicolor |  virginica |  -0.20 | [-0.31, -0.09] | 99.83% |     7.00% |         -0.46
```

``` r
library(see)

plot(estimate_contrasts(model), estimate_means(model))
```

![](man/figures/unnamed-chunk-8-1.png)<!-- -->

### Check the contrasts at different points of another linear predictor

``` r
model <- stan_glm(Sepal.Width ~ Species * Petal.Length, data = iris)

estimate_contrasts(model, modulate = "Petal.Length", length = 3)
```

    ## Level1     |     Level2 | Petal.Length | Median |        89% CI |     pd | % in ROPE | Median (std.)
    ## ----------------------------------------------------------------------------------------------------
    ## setosa     | versicolor |         1.00 |   1.55 |  [1.01, 1.99] |   100% |        0% |          3.55
    ## setosa     |  virginica |         1.00 |   1.21 |  [0.64, 1.74] | 99.95% |     0.07% |          2.78
    ## versicolor |  virginica |         1.00 |  -0.33 | [-1.05, 0.42] | 75.60% |    13.95% |         -0.76
    ## setosa     | versicolor |         3.95 |   1.80 |  [1.08, 2.57] |   100% |        0% |          4.13
    ## setosa     |  virginica |         3.95 |   1.83 |  [1.00, 2.60] |   100% |        0% |          4.20
    ## versicolor |  virginica |         3.95 |   0.03 | [-0.19, 0.26] | 58.70% |    51.85% |          0.08
    ## setosa     | versicolor |         6.90 |   2.08 |  [0.54, 3.71] | 98.12% |     1.07% |          4.78
    ## setosa     |  virginica |         6.90 |   2.46 |  [0.85, 3.98] | 99.35% |     0.38% |          5.64
    ## versicolor |  virginica |         6.90 |   0.39 | [-0.03, 0.84] | 92.07% |    11.47% |          0.89

### Find a predictor’s slopes at each factor level

``` r
estimate_slopes(model)
## Species    | Median |       89% CI |     pd | % in ROPE | Median (std.)
## -----------------------------------------------------------------------
## setosa     |   0.42 | [0.12, 0.70] | 98.88% |     3.82% |          1.72
## versicolor |   0.33 | [0.19, 0.48] |   100% |     0.73% |          1.35
## virginica  |   0.21 | [0.09, 0.34] | 99.65% |     7.40% |          0.85
```

### Generate predictions from your model to compare it with original data

``` r
estimate_response(model)
```

| Species | Petal.Length | Median | CI\_low | CI\_high |
| :------ | -----------: | -----: | ------: | -------: |
| setosa  |          1.4 |   3.40 |    2.85 |     3.90 |
| setosa  |          1.4 |   3.41 |    2.90 |     3.94 |
| setosa  |          1.3 |   3.35 |    2.86 |     3.88 |
| setosa  |          1.5 |   3.45 |    2.91 |     3.94 |
| setosa  |          1.4 |   3.40 |    2.86 |     3.90 |
| setosa  |          1.7 |   3.52 |    3.01 |     4.05 |

### Estimate the link between the response and a predictor

<img src="https://github.com/easystats/estimate/raw/master/man/figures/gganimate_figure.gif" width="80%" style="display: block; margin: auto;" />

``` r
model <- stan_glm(Sepal.Width ~ poly(Petal.Length, 2), data=iris)

estimate_link(model)
```

| Petal.Length | Median | CI\_low | CI\_high |
| -----------: | -----: | ------: | -------: |
|         1.00 |   3.62 |    3.51 |     3.72 |
|         1.98 |   3.18 |    3.11 |     3.24 |
|         2.97 |   2.90 |    2.82 |     2.97 |
|         3.95 |   2.78 |    2.70 |     2.85 |
|         4.93 |   2.84 |    2.78 |     2.89 |
|         5.92 |   3.05 |    2.96 |     3.14 |
|         6.90 |   3.44 |    3.25 |     3.63 |

### Describe the smooth term by its linear parts

``` r
estimate_smooth(model)
## Part | Start |  End |   Size | Trend | Linearity
## ------------------------------------------------
## 1    |  1.00 | 4.08 | 52.50% | -0.01 |      0.94
## 2    |  4.08 | 6.90 | 47.50% |  0.01 |      0.93
```
