# Plotting estimated marginal means

This vignette provides a quick overview with different examples that
show how to plot estimated marginal means.

In summary, you can use the `length` and `range` arguments in
[`estimate_means()`](https://easystats.github.io/modelbased/reference/estimate_means.md)
(which are passed to
[`insight::get_datagrid()`](https://easystats.github.io/insight/reference/get_datagrid.html)),
as well as directly specifying meaningful values in the `by` argument,
which are also used to create a data grid, to control the
plot-appearance. See also the [vignette on data
grids](https://easystats.github.io/modelbased/articles/visualisation_matrix.html).

Although the **modelbased** package does not focus on publication-ready
plots, the default plots can already be used directly. Furthermore, a
few modifications are already applies, like a percentage-scale for
logistic regression models, or using variable labels for *labelled
data*.

## One predictor - categorical

The simplest case is possibly plotting one categorical predictor.
Predicted values for each level and its confidence intervals are shown.

``` r

library(modelbased)
data(efc, package = "modelbased")
efc <- datawizard::to_factor(efc, c("e16sex", "c172code", "e42dep"))

m <- lm(neg_c_7 ~ e16sex + c172code + barthtot, data = efc)
estimate_means(m, "c172code") |> plot()
```

![](plotting_files/figure-html/unnamed-chunk-1-1.png)

## One predictor - numeric

For numeric predictors, the range of predictions at different values of
the focal predictor are plotted, the uncertainty is displayed as
confidence band.

``` r

estimate_means(m, "barthtot") |> plot()
```

![](plotting_files/figure-html/unnamed-chunk-2-1.png)

## Two predictors - categorical

For two categorical predictors, the first focal predictors is plotted
along the x-axis, while the levels of the second predictor are mapped to
different colors.

``` r

m <- lm(neg_c_7 ~ e16sex * c172code + e42dep, data = efc)
estimate_means(m, c("e16sex", "c172code")) |> plot()
```

![](plotting_files/figure-html/unnamed-chunk-3-1.png)

## Two predictors - numeric \* categorical

For two predictors, where the first is numeric and the second
categorical, range of predictions including confidence bands are shown,
with the different levels of the second (categorical) predictor mapped
to colors again.

``` r

m <- lm(neg_c_7 ~ barthtot * c172code + e42dep, data = efc)
estimate_means(m, c("barthtot", "c172code")) |> plot()
```

![](plotting_files/figure-html/unnamed-chunk-4-1.png)

In general, plots can be further modified using functions from the
**ggplot2** package. Thereby, other themes, color scales, faceting and
so on, can be applies.

``` r

library(ggplot2)
estimate_means(m, c("barthtot", "c172code")) |>
  plot() +
  see::theme_modern(show.ticks = TRUE)
```

![](plotting_files/figure-html/unnamed-chunk-5-1.png)

``` r


estimate_means(m, c("barthtot", "c172code")) |>
  plot() +
  facet_grid(~c172code)
```

![](plotting_files/figure-html/unnamed-chunk-5-2.png)

``` r


estimate_means(m, c("barthtot", "c172code")) |>
  plot() +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2")
```

![](plotting_files/figure-html/unnamed-chunk-5-3.png)

## Two predictors - categorical \* numeric

If the numeric predictor is the *second* focal term, its values are
still mapped to colors, however, by default to a continuous (gradient)
scale, because a range of representative values for that numeric
predictor is used by default.

Focal predictors specified in
[`estimate_means()`](https://easystats.github.io/modelbased/reference/estimate_means.md)
are passed to
[`insight::get_datagrid()`](https://easystats.github.io/insight/reference/get_datagrid.html).
If not specified otherwise, representative values for numeric predictors
are evenly distributed from the minimum to the maximum, with a total
number of `length` values covering that range.

I.e., by default, arguments `range = "range"` and `length = 10` in
[`insight::get_datagrid()`](https://easystats.github.io/insight/reference/get_datagrid.html),
and thus for numeric predictors, a *range* of *length* values is used to
estimate predictions.

``` r

# by default, `range = "range"` and `length = 10`
estimate_means(m, c("c172code", "barthtot")) |> plot()
```

![](plotting_files/figure-html/unnamed-chunk-6-1.png)

That means that the `length` argument can be used to control how many
values (lines) for the numeric predictors are chosen.

``` r

estimate_means(m, c("c172code", "barthtot"), length = 20) |> plot()
```

![](plotting_files/figure-html/unnamed-chunk-7-1.png)

Another option would be to use `range = "grid"`, in which case the mean
and +/- one standard deviation around the mean are chosen as
representative values for numeric predictors.

``` r

estimate_means(m, c("c172code", "barthtot"), range = "grid") |> plot()
```

![](plotting_files/figure-html/unnamed-chunk-8-1.png)

It is also possible to specify representative values, at which the
estimated marginal means of the outcome should be plotted. Again,
consult the documentation at `?ìnsight::get_datagrid` for further
details.

``` r

estimate_means(
  m,
  c(
    "c172code = c('low level of education', 'high level of education')",
    "barthtot = c(30, 50, 80)"
  )
) |> plot()
```

![](plotting_files/figure-html/unnamed-chunk-9-1.png)

``` r


estimate_means(m, c("c172code", "barthtot = [fivenum]")) |> plot()
```

![](plotting_files/figure-html/unnamed-chunk-9-2.png)

## Three numeric predictors

The default plot-setting for three numeric predictors can be rather
confusing.

``` r

m <- lm(neg_c_7 ~ c12hour * barthtot * c160age, data = efc)
estimate_means(m, c("c12hour", "barthtot", "c160age")) |> plot()
```

![](plotting_files/figure-html/unnamed-chunk-10-1.png)

Instead, it is recommended to use `length`, create a “reference grid”,
or again specify meaningful values directly in the `by` argument.

``` r

estimate_means(m, c("c12hour", "barthtot", "c160age"), length = 2) |> plot()
```

![](plotting_files/figure-html/unnamed-chunk-11-1.png)

``` r


estimate_means(m, c("c12hour", "barthtot", "c160age"), range = "grid") |> plot()
```

![](plotting_files/figure-html/unnamed-chunk-11-2.png)

## Three categorical predictors

Multiple categorical predictors are usually less problematic, since
discrete color scales and faceting are used to distinguish between
factor levels.

``` r

m <- lm(neg_c_7 ~ e16sex * c172code * e42dep, data = efc)
estimate_means(m, c("e16sex", "c172code", "e42dep")) |> plot()
```

![](plotting_files/figure-html/unnamed-chunk-12-1.png)

## Smooth plots

Remember that by default a range of ten values is chosen for numeric
focal predictors. While this mostly works well for plotting linear
relationships, plots may look less smooth for certain models that
involve quadratic or cubic terms, or splines, or for instance if you
have GAMs.

``` r

m <- lm(neg_c_7 ~ e16sex * c12hour + e16sex * I(c12hour^2), data = efc)
estimate_means(m, c("c12hour", "e16sex")) |> plot()
```

![](plotting_files/figure-html/unnamed-chunk-13-1.png)

In this case, simply increase the number of representative values by
setting `length` to a higher number.

``` r

estimate_means(m, c("c12hour", "e16sex"), length = 200) |> plot()
```

![](plotting_files/figure-html/unnamed-chunk-14-1.png)
