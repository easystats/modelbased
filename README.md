
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

### Create smart grids to represent complex interactions

Check-out [**this
vignette**](https://easystats.github.io/estimate/articles/data_grid.html)
to see how to create this plot:

![](man/figures/unnamed-chunk-4-1.png)<!-- -->

### Estimate marginal means

![](man/figures/unnamed-chunk-5-1.png)<!-- -->

``` r
library(rstanarm)

model <- stan_glm(Sepal.Width ~ Species, data = iris)

estimate_means(model)
```

    ## Species    | Median |       89% CI
    ## ----------------------------------
    ## setosa     |   3.43 | [3.35, 3.50]
    ## versicolor |   2.77 | [2.69, 2.84]
    ## virginica  |   2.97 | [2.89, 3.04]

### Contrast analysis

``` r
estimate_contrasts(model)
## Level1     |     Level2 | Median |         89% CI |     pd | % in ROPE | Median (std.)
## --------------------------------------------------------------------------------------
## setosa     | versicolor |   0.66 |   [0.54, 0.77] |   100% |        0% |          1.51
## setosa     |  virginica |   0.45 |   [0.34, 0.56] |   100% |        0% |          1.04
## versicolor |  virginica |  -0.21 | [-0.31, -0.10] | 99.85% |     6.05% |         -0.47
```

``` r
library(see)

plot(estimate_contrasts(model), estimate_means(model))
```

![](man/figures/unnamed-chunk-9-1.png)<!-- -->

### Check the contrasts at different points of another linear predictor

``` r
model <- stan_glm(Sepal.Width ~ Species * Petal.Length, data = iris)

estimate_contrasts(model, modulate = "Petal.Length", length = 3)
```

    ## Level1     |     Level2 | Petal.Length | Median |        89% CI |     pd | % in ROPE | Median (std.)
    ## ----------------------------------------------------------------------------------------------------
    ## setosa     | versicolor |         1.00 |   1.52 |  [1.09, 2.02] |   100% |        0% |          3.49
    ## setosa     |  virginica |         1.00 |   1.21 |  [0.66, 1.76] |   100% |        0% |          2.78
    ## versicolor |  virginica |         1.00 |  -0.31 | [-1.02, 0.41] | 76.20% |    13.48% |         -0.72
    ## setosa     | versicolor |         3.95 |   1.81 |  [1.03, 2.54] | 99.98% |        0% |          4.14
    ## setosa     |  virginica |         3.95 |   1.83 |  [1.07, 2.67] | 99.95% |     0.02% |          4.20
    ## versicolor |  virginica |         3.95 |   0.03 | [-0.19, 0.25] | 59.17% |    51.42% |          0.08
    ## setosa     | versicolor |         6.90 |   2.07 |  [0.49, 3.71] | 98.05% |     1.00% |          4.74
    ## setosa     |  virginica |         6.90 |   2.45 |  [0.95, 4.14] | 99.33% |     0.32% |          5.63
    ## versicolor |  virginica |         6.90 |   0.38 | [-0.03, 0.80] | 92.67% |    10.38% |          0.87

### Find a predictor’s slopes at each factor level

``` r
estimate_slopes(model)
## Species    | Median |       89% CI |     pd | % in ROPE | Median (std.)
## -----------------------------------------------------------------------
## setosa     |   0.42 | [0.11, 0.71] | 98.83% |     3.60% |          1.70
## versicolor |   0.33 | [0.19, 0.47] |   100% |     0.38% |          1.33
## virginica  |   0.21 | [0.09, 0.33] | 99.72% |     7.05% |          0.86
```

### Generate predictions from your model to compare it with original data

See [**this
vignette**](https://easystats.github.io/estimate/reference/estimate_response.html)
to see how to create this plot:

![](man/figures/unnamed-chunk-13-1.png)<!-- -->

``` r
estimate_response(model)
```

| Sepal.Length | Species | Median | CI\_low | CI\_high |
| -----------: | :------ | -----: | ------: | -------: |
|          5.1 | setosa  |   1.47 |    1.09 |     1.92 |
|          4.9 | setosa  |   1.45 |    1.04 |     1.88 |
|          4.7 | setosa  |   1.41 |    1.00 |     1.82 |
|          4.6 | setosa  |   1.40 |    0.99 |     1.82 |
|          5.0 | setosa  |   1.46 |    1.01 |     1.83 |
|          5.4 | setosa  |   1.52 |    1.10 |     1.94 |

### Estimate the link between the response and a predictor

See [**this
vignette**](https://easystats.github.io/estimate/reference/estimate_response.html)
to see how to create this plot:
<img src="https://github.com/easystats/estimate/raw/master/man/figures/gganimate_figure.gif" width="80%" style="display: block; margin: auto;" />

``` r
model <- stan_glm(Sepal.Width ~ poly(Petal.Length, 2), data=iris)

estimate_link(model)
```

| Petal.Length | Median | CI\_low | CI\_high |
| -----------: | -----: | ------: | -------: |
|         1.00 |   3.63 |    3.52 |     3.72 |
|         1.98 |   3.18 |    3.11 |     3.24 |
|         2.97 |   2.89 |    2.83 |     2.98 |
|         3.95 |   2.78 |    2.71 |     2.86 |
|         4.93 |   2.83 |    2.78 |     2.90 |
|         5.92 |   3.05 |    2.97 |     3.15 |
|         6.90 |   3.44 |    3.25 |     3.62 |

### Describe the smooth term by its linear parts

``` r
estimate_smooth(model)
## Part | Start |  End |   Size | Trend | Linearity
## ------------------------------------------------
## 1    |  1.00 | 4.08 | 52.50% | -0.01 |      0.94
## 2    |  4.08 | 6.90 | 47.50% |  0.01 |      0.93
```
