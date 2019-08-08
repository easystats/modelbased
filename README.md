
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
to create this plot:

![](man/figures/unnamed-chunk-4-1.png)<!-- -->

### Estimate marginal means

Check-out [**this
vignette**](https://easystats.github.io/estimate/reference/estimate_means.html)
to create this plot:

![](man/figures/unnamed-chunk-5-1.png)<!-- -->

``` r
library(rstanarm)

model <- stan_glm(Sepal.Width ~ Species, data = iris)

estimate_means(model)
```

    ## Species    | Median |       89% CI
    ## ----------------------------------
    ## setosa     |   3.42 | [3.35, 3.51]
    ## versicolor |   2.77 | [2.69, 2.85]
    ## virginica  |   2.97 | [2.90, 3.06]

### Contrast analysis

Check-out [**this
vignette**](https://easystats.github.io/estimate/reference/estimate_contrasts.html)
to create this plot:

![](man/figures/unnamed-chunk-8-1.png)<!-- -->

``` r
estimate_contrasts(model)
## Level1     |     Level2 | Median |         89% CI |     pd | % in ROPE | Median (std.)
## --------------------------------------------------------------------------------------
## setosa     | versicolor |   0.65 |   [0.54, 0.76] |   100% |        0% |          1.50
## setosa     |  virginica |   0.45 |   [0.34, 0.56] |   100% |        0% |          1.03
## versicolor |  virginica |  -0.20 | [-0.31, -0.08] | 99.80% |     7.32% |         -0.47
```

### Check the contrasts at different points of another linear predictor

``` r
model <- stan_glm(Sepal.Width ~ Species * Petal.Length, data = iris)

estimate_contrasts(model, modulate = "Petal.Length", length = 3)
```

    ## Level1     |     Level2 | Petal.Length | Median |        89% CI |     pd | % in ROPE | Median (std.)
    ## ----------------------------------------------------------------------------------------------------
    ## setosa     | versicolor |         1.00 |   1.51 |  [1.06, 2.04] |   100% |        0% |          3.47
    ## setosa     |  virginica |         1.00 |   1.21 |  [0.66, 1.74] |   100% |     0.02% |          2.79
    ## versicolor |  virginica |         1.00 |  -0.31 | [-1.06, 0.39] | 75.15% |    13.28% |         -0.71
    ## setosa     | versicolor |         3.95 |   1.81 |  [1.04, 2.55] |   100% |        0% |          4.14
    ## setosa     |  virginica |         3.95 |   1.84 |  [1.01, 2.61] | 99.98% |     0.02% |          4.22
    ## versicolor |  virginica |         3.95 |   0.03 | [-0.19, 0.26] | 59.62% |    51.48% |          0.07
    ## setosa     | versicolor |         6.90 |   2.09 |  [0.44, 3.66] | 98.40% |     0.78% |          4.81
    ## setosa     |  virginica |         6.90 |   2.45 |  [0.90, 4.12] | 99.42% |     0.40% |          5.63
    ## versicolor |  virginica |         6.90 |   0.37 | [-0.03, 0.82] | 92.20% |    11.35% |          0.85

### Find a predictor’s slopes at each factor level

``` r
estimate_slopes(model)
## Species    | Median |       89% CI |     pd | % in ROPE | Median (std.)
## -----------------------------------------------------------------------
## setosa     |   0.42 | [0.13, 0.73] | 98.95% |     4.20% |          1.71
## versicolor |   0.33 | [0.18, 0.47] | 99.95% |     0.83% |          1.32
## virginica  |   0.21 | [0.09, 0.33] | 99.62% |     6.83% |          0.85
```

### Generate predictions from your model to compare it with original data

Check-out [**this
vignette**](https://easystats.github.io/estimate/reference/estimate_response.html)
to create this plot:

![](man/figures/unnamed-chunk-13-1.png)<!-- -->

``` r
estimate_response(model)
```

| Sepal.Length | Species | Median | CI\_low | CI\_high |
| -----------: | :------ | -----: | ------: | -------: |
|          5.1 | setosa  |   1.49 |    1.05 |     1.87 |
|          4.9 | setosa  |   1.46 |    1.00 |     1.88 |
|          4.7 | setosa  |   1.39 |    1.02 |     1.89 |
|          4.6 | setosa  |   1.41 |    1.00 |     1.82 |
|          5.0 | setosa  |   1.47 |    1.04 |     1.86 |
|          5.4 | setosa  |   1.52 |    1.09 |     2.01 |

### Estimate the link between the response and a predictor

See [**this
vignette**](https://easystats.github.io/estimate/reference/estimate_response.html)
to create this plot:
<img src="https://github.com/easystats/estimate/raw/master/man/figures/gganimate_figure.gif" width="80%" style="display: block; margin: auto;" />

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
|         4.93 |   2.83 |    2.78 |     2.89 |
|         5.92 |   3.05 |    2.97 |     3.14 |
|         6.90 |   3.43 |    3.25 |     3.63 |

### Describe the smooth term by its linear parts

``` r
estimate_smooth(model)
## Part | Start |  End |   Size | Trend | Linearity
## ------------------------------------------------
## 1    |  1.00 | 4.08 | 52.50% | -0.01 |      0.94
## 2    |  4.08 | 6.90 | 47.50% |  0.01 |      0.93
```
