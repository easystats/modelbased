
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

``` r
library(rstanarm)
library(ggplot2)
library(see)
library(estimate)

# Fit model
model <- lm(Sepal.Length ~ Petal.Length * Petal.Width , data = iris)

# Create dataframe
newdata <- iris %>% 
  data_grid(c("Petal.Length", "Petal.Width"), length = 10) %>% 
  data_grid("Petal.Width", length=3, numerics = "combination", standardize = TRUE)
newdata$Predicted_Sepal.Length <- predict(model, newdata)

# Express values in an abstract way
newdata$Petal.Width <- parameters::format_standardize(newdata$Petal.Width, reference = iris$Petal.Width)

# Plot
iris %>% 
  ggplot(aes(x = Petal.Length, y = Sepal.Length)) +
  geom_point2(aes(fill = Petal.Width), color = "white", shape=21, size = 5) +  # Only shapes from 21 to 25 have a fill aesthetic
  geom_line(data = newdata, aes(y = Predicted_Sepal.Length, color = Petal.Width), size = 1) + 
  scale_color_viridis_d() +
  scale_fill_viridis_c(guide = FALSE) +
  theme_modern()
```

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
    ## versicolor |   2.77 | [2.69, 2.85]
    ## virginica  |   2.97 | [2.90, 3.05]

### Contrast analysis

``` r
estimate_contrasts(model)
## Level1     |     Level2 | Median |         89% CI |     pd | % in ROPE | Median (std.)
## --------------------------------------------------------------------------------------
## setosa     | versicolor |   0.65 |   [0.55, 0.76] |   100% |        0% |          1.50
## setosa     |  virginica |   0.45 |   [0.34, 0.56] |   100% |        0% |          1.03
## versicolor |  virginica |  -0.20 | [-0.31, -0.09] | 99.95% |     6.28% |         -0.46
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
    ## setosa     | versicolor |         1.00 |   1.54 |  [1.09, 2.05] |   100% |        0% |          3.54
    ## setosa     |  virginica |         1.00 |   1.23 |  [0.68, 1.78] | 99.92% |     0.12% |          2.81
    ## versicolor |  virginica |         1.00 |  -0.31 | [-1.02, 0.44] | 76.02% |    14.12% |         -0.72
    ## setosa     | versicolor |         3.95 |   1.79 |  [1.06, 2.55] | 99.92% |     0.05% |          4.10
    ## setosa     |  virginica |         3.95 |   1.83 |  [1.02, 2.62] | 99.90% |     0.12% |          4.19
    ## versicolor |  virginica |         3.95 |   0.03 | [-0.18, 0.26] | 59.92% |    52.20% |          0.08
    ## setosa     | versicolor |         6.90 |   2.01 |  [0.53, 3.67] | 97.92% |     0.83% |          4.62
    ## setosa     |  virginica |         6.90 |   2.41 |  [0.89, 4.00] | 99.10% |     0.20% |          5.54
    ## versicolor |  virginica |         6.90 |   0.39 | [-0.03, 0.84] | 92.73% |    10.62% |          0.88

### Find a predictor’s slopes at each factor level

![](man/figures/unnamed-chunk-12-1.png)<!-- -->

``` r
estimate_slopes(model)
## Species    | Median |        89% CI |     pd | % in ROPE | Median (std.)
## ------------------------------------------------------------------------
## setosa     |   0.14 | [-0.05, 0.28] | 88.80% |    35.90% |          0.06
## versicolor |   0.68 |  [0.56, 0.79] |   100% |        0% |          0.32
## virginica  |   0.75 |  [0.66, 0.85] |   100% |        0% |          0.35
```

### Generate predictions from your model to compare it with original data

``` r
estimate_response(model)
```

| Sepal.Length | Species | Median | CI\_low | CI\_high |
| -----------: | :------ | -----: | ------: | -------: |
|          5.1 | setosa  |   1.48 |    1.05 |     1.90 |
|          4.9 | setosa  |   1.45 |    1.00 |     1.87 |
|          4.7 | setosa  |   1.44 |    1.02 |     1.88 |
|          4.6 | setosa  |   1.39 |    0.98 |     1.82 |
|          5.0 | setosa  |   1.46 |    1.04 |     1.86 |
|          5.4 | setosa  |   1.51 |    1.09 |     1.93 |

### Estimate the link between the response and a predictor

<img src="https://github.com/easystats/estimate/raw/master/man/figures/gganimate_figure.gif" width="80%" style="display: block; margin: auto;" />

``` r
model <- stan_glm(Sepal.Width ~ poly(Petal.Length, 2), data=iris)

estimate_link(model)
```

| Petal.Length | Median | CI\_low | CI\_high |
| -----------: | -----: | ------: | -------: |
|         1.00 |   3.62 |    3.52 |     3.73 |
|         1.98 |   3.18 |    3.11 |     3.24 |
|         2.97 |   2.89 |    2.82 |     2.97 |
|         3.95 |   2.78 |    2.71 |     2.85 |
|         4.93 |   2.83 |    2.78 |     2.89 |
|         5.92 |   3.05 |    2.97 |     3.15 |
|         6.90 |   3.44 |    3.25 |     3.64 |

### Describe the smooth term by its linear parts

``` r
estimate_smooth(model)
## Part | Start |  End |   Size | Trend | Linearity
## ------------------------------------------------
## 1    |  1.00 | 4.08 | 52.50% | -0.01 |      0.94
## 2    |  4.08 | 6.90 | 47.50% |  0.01 |      0.93
```
