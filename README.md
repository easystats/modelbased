
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
    ## versicolor | 2.77 | [2.68, 2.87]
    ## virginica  | 2.97 | [2.89, 3.07]

### Contrast analysis

Check-out [**this
vignette**](https://easystats.github.io/modelbased/articles/estimate_contrasts.html)
to create this plot:

![](man/figures/unnamed-chunk-8-1.png)<!-- -->

``` r
estimate_contrasts(model)
## Level1     |     Level2 | Difference |         95% CI |     pd | % in ROPE | Std_Difference
## -------------------------------------------------------------------------------------------
## setosa     | versicolor |       0.66 | [ 0.53,  0.80] |   100% |        0% |           1.51
## setosa     |  virginica |       0.45 | [ 0.33,  0.59] |   100% |        0% |           1.04
## versicolor |  virginica |      -0.20 | [-0.34, -0.07] | 99.83% |     6.68% |          -0.47
```

### Check the contrasts at different points of another linear predictor

``` r
model <- stan_glm(Sepal.Width ~ Species * Petal.Length, data = iris)

estimate_contrasts(model, modulate = "Petal.Length", length = 3)
```

    ## Level1     |     Level2 | Petal.Length | Difference |        95% CI |     pd | % in ROPE | Std_Difference
    ## ---------------------------------------------------------------------------------------------------------
    ## setosa     | versicolor |         1.00 |       1.67 | [ 1.05, 2.27] |   100% |        0% |           3.84
    ## setosa     |  virginica |         1.00 |       1.34 | [ 0.59, 2.07] |   100% |     0.02% |           3.07
    ## versicolor |  virginica |         1.00 |      -0.33 | [-1.27, 0.57] | 75.48% |    13.60% |          -0.76
    ## setosa     | versicolor |         3.95 |       1.61 | [ 0.66, 2.54] | 99.92% |     0.07% |           3.70
    ## setosa     |  virginica |         3.95 |       1.66 | [ 0.69, 2.68] | 99.90% |     0.15% |           3.81
    ## versicolor |  virginica |         3.95 |       0.05 | [-0.23, 0.34] | 63.50% |    47.48% |           0.12
    ## setosa     | versicolor |         6.90 |       1.54 | [-0.54, 3.60] | 93.00% |     2.73% |           3.54
    ## setosa     |  virginica |         6.90 |       2.00 | [-0.02, 4.00] | 97.35% |     1.27% |           4.58
    ## versicolor |  virginica |         6.90 |       0.44 | [-0.08, 0.96] | 94.62% |     8.80% |           1.01

### Find a predictor’s slopes at each factor level

``` r
estimate_slopes(model)
## Species    | Median |        95% CI |     pd | % in ROPE | Std. Median
## ----------------------------------------------------------------------
## setosa     |   0.34 | [-0.05, 0.69] | 96.00% |     8.48% |        1.37
## versicolor |   0.36 | [ 0.18, 0.54] | 99.98% |     0.18% |        1.46
## virginica  |   0.23 | [ 0.07, 0.38] | 99.88% |     5.78% |        0.93
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
|          5.1 | setosa  |      1.47 |    0.98 |     1.98 |
|          4.9 | setosa  |      1.45 |    0.92 |     1.97 |
|          4.7 | setosa  |      1.41 |    0.88 |     1.96 |
|          4.6 | setosa  |      1.42 |    0.92 |     1.98 |
|          5.0 | setosa  |      1.46 |    0.90 |     1.96 |
|          5.4 | setosa  |      1.51 |    1.00 |     2.11 |

### Estimate the link between the response and a predictor

See [**this
vignette**](https://easystats.github.io/modelbased/articles/estimate_response.html)
to create this plot:
<img src="https://github.com/easystats/modelbased/raw/master/man/figures/gganimate_figure.gif" width="80%" style="display: block; margin: auto;" />

``` r
model <- stan_glm(Sepal.Width ~ poly(Petal.Length, 2), data = iris)

estimate_link(model)
```

| Petal.Length | Predicted | CI\_low | CI\_high |
|-------------:|----------:|--------:|---------:|
|         1.00 |      3.62 |    3.49 |     3.75 |
|         1.98 |      3.18 |    3.09 |     3.26 |
|         2.97 |      2.90 |    2.81 |     2.99 |
|         3.95 |      2.78 |    2.69 |     2.87 |
|         4.93 |      2.83 |    2.77 |     2.91 |
|         5.92 |      3.05 |    2.94 |     3.16 |
|         6.90 |      3.44 |    3.20 |     3.68 |

### Describe the smooth term by its linear parts

``` r
estimate_smooth(model)
## Part | Start |  End |   Size | Trend | Linearity
## ------------------------------------------------
## 1    |  1.00 | 3.62 | 50.00% | -0.19 |      0.96
## 2    |  3.62 | 6.90 | 50.00% |  0.08 |      0.81
```
