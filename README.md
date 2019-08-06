
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
    ## setosa     |   3.42 | [3.34, 3.50]
    ## versicolor |   2.77 | [2.69, 2.85]
    ## virginica  |   2.97 | [2.90, 3.05]

### Contrast analysis

``` r
estimate_contrasts(model)
## Level1     |     Level2 | Median |         89% CI |     pd | % in ROPE | Median (std.)
## --------------------------------------------------------------------------------------
## setosa     | versicolor |   0.65 |   [0.54, 0.76] |   100% |        0% |          1.50
## setosa     |  virginica |   0.45 |   [0.34, 0.56] |   100% |        0% |          1.03
## versicolor |  virginica |  -0.20 | [-0.32, -0.10] | 99.92% |     6.75% |         -0.47
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
    ## setosa     | versicolor |         1.00 |   1.53 |  [1.07, 2.03] |   100% |        0% |          3.51
    ## setosa     |  virginica |         1.00 |   1.22 |  [0.68, 1.81] | 99.98% |     0.10% |          2.80
    ## versicolor |  virginica |         1.00 |  -0.31 | [-1.02, 0.44] | 75.35% |    14.05% |         -0.71
    ## setosa     | versicolor |         3.95 |   1.78 |  [1.05, 2.53] |   100% |        0% |          4.08
    ## setosa     |  virginica |         3.95 |   1.81 |  [1.05, 2.61] | 99.98% |     0.02% |          4.16
    ## versicolor |  virginica |         3.95 |   0.03 | [-0.19, 0.26] | 60.00% |    51.75% |          0.08
    ## setosa     | versicolor |         6.90 |   2.04 |  [0.36, 3.54] | 97.95% |     1.00% |          4.68
    ## setosa     |  virginica |         6.90 |   2.40 |  [0.73, 3.88] | 99.22% |     0.55% |          5.51
    ## versicolor |  virginica |         6.90 |   0.37 | [-0.05, 0.80] | 92.22% |    11.28% |          0.86

### Find a predictor’s slopes at each factor level

``` r
estimate_slopes(model)
## Species    | Median |       89% CI |     pd | % in ROPE | Median (std.)
## -----------------------------------------------------------------------
## setosa     |   0.41 | [0.14, 0.73] | 98.75% |     4.40% |          1.67
## versicolor |   0.33 | [0.19, 0.48] |   100% |     0.70% |          1.32
## virginica  |   0.21 | [0.09, 0.34] | 99.60% |     7.60% |          0.85
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
|          5.1 | setosa  |   1.47 |    1.10 |     1.95 |
|          4.9 | setosa  |   1.46 |    1.03 |     1.86 |
|          4.7 | setosa  |   1.43 |    1.00 |     1.85 |
|          4.6 | setosa  |   1.40 |    0.94 |     1.82 |
|          5.0 | setosa  |   1.45 |    1.04 |     1.88 |
|          5.4 | setosa  |   1.51 |    1.07 |     1.89 |

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
|         1.00 |   3.62 |    3.52 |     3.74 |
|         1.98 |   3.18 |    3.11 |     3.24 |
|         2.97 |   2.90 |    2.82 |     2.97 |
|         3.95 |   2.78 |    2.71 |     2.86 |
|         4.93 |   2.83 |    2.78 |     2.89 |
|         5.92 |   3.05 |    2.96 |     3.14 |
|         6.90 |   3.44 |    3.26 |     3.64 |

### Describe the smooth term by its linear parts

``` r
estimate_smooth(model)
## Part | Start |  End |   Size | Trend | Linearity
## ------------------------------------------------
## 1    |  1.00 | 4.11 | 53.00% | -0.01 |      0.94
## 2    |  4.11 | 6.90 | 47.00% |  0.01 |      0.94
```
