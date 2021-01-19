
# modelbased <img src='man/figures/logo.png' align="right" height="139" />

[![publication](https://img.shields.io/badge/Cite-Unpublished-yellow)](https://github.com/easystats/modelbased/blob/master/inst/CITATION)
[![downloads](http://cranlogs.r-pkg.org/badges/modelbased)](https://cran.r-project.org/package=modelbased)
[![total](https://cranlogs.r-pkg.org/badges/grand-total/modelbased)](https://cranlogs.r-pkg.org/)

`modelbased` is a lightweight package helping with model-based
estimations, used in the computation of marginal means, contrast
analysis and model predictions.

## Installation

[![CRAN](http://www.r-pkg.org/badges/version/modelbased)](https://cran.r-project.org/package=modelbased)
![Tests](https://github.com/easystats/modelbased/workflows/Tests/badge.svg)
[![codecov](https://codecov.io/gh/easystats/modelbased/branch/master/graph/badge.svg)](https://codecov.io/gh/easystats/modelbased)

Run the following to install the stable release of **modelbased** from
CRAN:

``` r
install.packages("modelbased")
```

Or this one to install the latest development version:

``` r
install.packages("remotes")
remotes::install_github("easystats/modelbased")
```

## Documentation

[![Documentation](https://img.shields.io/badge/documentation-modelbased-orange.svg?colorB=E91E63)](https://easystats.github.io/modelbased/)
[![Blog](https://img.shields.io/badge/blog-easystats-orange.svg?colorB=FF9800)](https://easystats.github.io/blog/posts/)
[![Features](https://img.shields.io/badge/features-modelbased-orange.svg?colorB=2196F3)](https://easystats.github.io/modelbased/reference/index.html)

Click on the buttons above to access the package
[**documentation**](https://easystats.github.io/modelbased/) and the
[**easystats blog**](https://easystats.github.io/blog/posts/), and
check-out these vignettes:

-   [**Visualisation
    matrix**](https://easystats.github.io/modelbased/articles/visualisation_matrix.html)
-   [**Marginal
    means**](https://easystats.github.io/modelbased/articles/estimate_means.html)
-   [**Contrast
    analysis**](https://easystats.github.io/modelbased/articles/estimate_contrasts.html)
-   [**Use a model to make
    predictions**](https://easystats.github.io/modelbased/articles/estimate_response.html)
-   [**Describe non-linear
    curves**](https://easystats.github.io/modelbased/articles/estimate_smooth.html)

# Features

The package is built around 5 main functions:

-   [`estimate_means()`](https://easystats.github.io/modelbased/reference/estimate_means.html):
    Estimates the average values at each factor levels
-   [`estimate_contrasts()`](https://easystats.github.io/modelbased/reference/estimate_contrasts.html):
    Estimates and tests contrasts between different factor levels
-   [`estimate_slopes()`](https://easystats.github.io/modelbased/reference/estimate_slopes.html):
    Estimates the slopes of numeric predictors at different factor
    levels
-   [`estimate_response()`](https://easystats.github.io/modelbased/reference/estimate_response.html):
    Predict the response variable using the model
-   [`estimate_smooth()`](https://easystats.github.io/modelbased/reference/estimate_smooth.html):
    Describes a non-linear term (*e.g.* in GAMs) by its linear parts

These functions are powered by the
[`visualisation_matrix()`](https://easystats.github.io/modelbased/reference/visualisation_matrix.html)
function, a smart tool for guessing the appropriate reference grid.

The package currently only supports `rstanarm` models, but will be
expanded to cover a large variety of frequentist and Bayesian models.

## Examples

### Create smart grids to represent complex interactions

Check-out [**this
vignette**](https://easystats.github.io/modelbased/articles/visualisation_matrix.html)
to create this plot:

![](man/figures/unnamed-chunk-4-1.png)<!-- -->

### Estimate marginal means

Check-out [**this
vignette**](https://easystats.github.io/modelbased/articles/estimate_means.html)
to create this plot:

![](man/figures/unnamed-chunk-5-1.png)<!-- -->

``` r
library(rstanarm)

model <- stan_glm(Sepal.Width ~ Species, data = iris)

estimate_means(model)
```

    ## Species    | Mean |       95% CI
    ## --------------------------------
    ## setosa     | 3.43 | [3.34, 3.52]
    ## versicolor | 2.77 | [2.68, 2.86]
    ## virginica  | 2.98 | [2.88, 3.07]

### Contrast analysis

Check-out [**this
vignette**](https://easystats.github.io/modelbased/articles/estimate_contrasts.html)
to create this plot:

![](man/figures/unnamed-chunk-8-1.png)<!-- -->

``` r
estimate_contrasts(model)
## Level1     |     Level2 | Difference |         95% CI |     pd | % in ROPE | Std_Difference
## -------------------------------------------------------------------------------------------
## setosa     | versicolor |       0.66 | [ 0.53,  0.79] |   100% |        0% |           1.51
## setosa     |  virginica |       0.45 | [ 0.31,  0.57] |   100% |        0% |           1.04
## versicolor |  virginica |      -0.21 | [-0.33, -0.07] | 99.83% |     6.53% |          -0.47
```

### Check the contrasts at different points of another linear predictor

``` r
model <- stan_glm(Sepal.Width ~ Species * Petal.Length, data = iris)

estimate_contrasts(model, modulate = "Petal.Length", length = 3)
```

    ## Level1     |     Level2 | Petal.Length | Difference |        95% CI |     pd | % in ROPE | Std_Difference
    ## ---------------------------------------------------------------------------------------------------------
    ## setosa     | versicolor |         1.00 |       1.68 | [ 1.05, 2.29] |   100% |        0% |           3.85
    ## setosa     |  virginica |         1.00 |       1.34 | [ 0.63, 2.08] | 99.98% |     0.05% |           3.08
    ## versicolor |  virginica |         1.00 |      -0.33 | [-1.25, 0.62] | 76.02% |    13.23% |          -0.75
    ## setosa     | versicolor |         3.95 |       1.61 | [ 0.67, 2.56] | 99.95% |     0.12% |           3.70
    ## setosa     |  virginica |         3.95 |       1.66 | [ 0.64, 2.63] | 99.92% |     0.12% |           3.82
    ## versicolor |  virginica |         3.95 |       0.05 | [-0.24, 0.34] | 64.03% |    48.77% |           0.13
    ## setosa     | versicolor |         6.90 |       1.51 | [-0.45, 3.65] | 92.27% |     2.38% |           3.47
    ## setosa     |  virginica |         6.90 |       1.97 | [-0.05, 4.06] | 96.95% |     1.60% |           4.53
    ## versicolor |  virginica |         6.90 |       0.44 | [-0.10, 0.99] | 94.38% |     7.83% |           1.02

### Find a predictor’s slopes at each factor level

``` r
estimate_slopes(model)
## Species    | Median |        95% CI |     pd | % in ROPE | Std. Median
## ----------------------------------------------------------------------
## setosa     |   0.34 | [-0.03, 0.71] | 95.58% |    10.30% |        1.37
## versicolor |   0.36 | [ 0.17, 0.54] | 99.98% |     0.27% |        1.46
## virginica  |   0.23 | [ 0.08, 0.38] | 99.85% |     5.42% |        0.93
```

### Generate predictions from your model to compare it with original data

Check-out [**this
vignette**](https://easystats.github.io/modelbased/articles/estimate_response.html)
to create this plot:

![](man/figures/unnamed-chunk-13-1.png)<!-- -->

``` r
estimate_response(model)
```

| Sepal.Length | Species | Predicted | CI\_low | CI\_high |
|-------------:|:--------|----------:|--------:|---------:|
|          5.1 | setosa  |      1.48 |    1.01 |     2.07 |
|          4.9 | setosa  |      1.45 |    0.93 |     1.98 |
|          4.7 | setosa  |      1.41 |    0.88 |     1.89 |
|          4.6 | setosa  |      1.41 |    0.83 |     1.92 |
|          5.0 | setosa  |      1.46 |    0.96 |     1.98 |
|          5.4 | setosa  |      1.52 |    1.01 |     2.04 |

### Estimate the link between the response and a predictor

See [**this
vignette**](https://easystats.github.io/modelbased/articles/estimate_response.html)
to create this plot:
<img src="https://github.com/easystats/modelbased/raw/master/man/figures/gganimate_figure.gif" width="80%" style="display: block; margin: auto;" />

``` r
model <- stan_glm(Sepal.Width ~ poly(Petal.Length, 2), data=iris)

estimate_link(model)
```

| Petal.Length | Predicted | CI\_low | CI\_high |
|-------------:|----------:|--------:|---------:|
|         1.00 |      3.62 |    3.49 |     3.76 |
|         1.98 |      3.18 |    3.10 |     3.26 |
|         2.97 |      2.89 |    2.80 |     2.99 |
|         3.95 |      2.78 |    2.68 |     2.86 |
|         4.93 |      2.83 |    2.76 |     2.90 |
|         5.92 |      3.05 |    2.94 |     3.17 |
|         6.90 |      3.44 |    3.20 |     3.68 |

### Describe the smooth term by its linear parts

``` r
estimate_smooth(model)
## Part | Start |  End |   Size |     Trend | Linearity
## ----------------------------------------------------
## 1    |  1.00 | 4.11 | 53.00% | -8.07e-03 |      0.94
## 2    |  4.11 | 6.90 | 47.00% |  6.89e-03 |      0.93
```
