# Contrasts and comparisons for generalized linear models

This vignette is the fourth in a 5-part series:

1.  [**Contrasts and Pairwise
    Comparisons**](https://easystats.github.io/modelbased/articles/introduction_comparisons_1.html)

2.  [**User Defined Contrasts and Joint
    Tests**](https://easystats.github.io/modelbased/articles/introduction_comparisons_2.html)

3.  [**Comparisons of Slopes, Floodlight and Spotlight Analysis
    (Johnson-Neyman
    Intervals)**](https://easystats.github.io/modelbased/articles/introduction_comparisons_3.html)

4.  **Contrasts and Comparisons for Generalized Linear Models**

5.  [**Contrasts and Comparisons for Zero-Inflation
    Models**](https://easystats.github.io/modelbased/articles/introduction_comparisons_5.html)

## Contrasts and comparisons for GLM - logistic regression example

We will now show an example for non-Gaussian models. For GLM’s
(generalized linear models) with (non-Gaussian) link-functions,
[`estimate_means()`](https://easystats.github.io/modelbased/reference/estimate_means.md)
by default returns predicted values on the *response* scale. For
example, predicted values for logistic regression models are shown as
*probabilities*.

Let’s look at a simple example.

[`library`](https://rdrr.io/r/base/library.html)`(`[`modelbased`](https://easystats.github.io/modelbased/)`)`` `[`set.seed`](https://rdrr.io/r/base/Random.html)`(``1234``)`` ``dat`` ``<-`` `[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(`` `` outcome ``=`` `[`rbinom`](https://rdrr.io/r/stats/Binomial.html)`(``n ``=`` ``100``, size ``=`` ``1``, prob ``=`` ``0.35``)``,`` `` x1 ``=`` `[`as.factor`](https://rdrr.io/r/base/factor.html)`(`[`sample`](https://rdrr.io/r/base/sample.html)`(``1``:``3``, size ``=`` ``100``, ``TRUE``, prob ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``0.5``, ``0.2``, ``0.3``)``)``)``,`` `` x2 ``=`` `[`rnorm`](https://rdrr.io/r/stats/Normal.html)`(``n ``=`` ``100``, mean ``=`` ``10``, sd ``=`` ``7``)``,`` `` x3 ``=`` `[`as.factor`](https://rdrr.io/r/base/factor.html)`(`[`sample`](https://rdrr.io/r/base/sample.html)`(``1``:``4``, size ``=`` ``100``, ``TRUE``, prob ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``0.1``, ``0.4``, ``0.2``, ``0.3``)``)``)`` ``)`` `` ``m`` ``<-`` `[`glm`](https://rdrr.io/r/stats/glm.html)`(``outcome`` ``~`` ``x1`` ``+`` ``x2`` ``+`` ``x3``, data ``=`` ``dat``, family ``=`` `[`binomial`](https://rdrr.io/r/stats/family.html)`(``)``)`` `[`estimate_means`](https://easystats.github.io/modelbased/reference/estimate_means.md)`(``m``, ``"x1"``)`` ``#> Estimated Marginal Means`` ``#> `` ``#> x1 | Probability | 95% CI`` ``#> -------------------------------`` ``#> 1 | 0.21 | [0.11, 0.36]`` ``#> 2 | 0.14 | [0.05, 0.34]`` ``#> 3 | 0.31 | [0.16, 0.51]`` ``#> `` ``#> Variable predicted: outcome`` ``#> Predictors modulated: x1`` ``#> Predictors averaged: x2 (10), x3`` ``#> Predictions are on the response-scale.`

### Contrasts and comparisons for categorical focal terms

Contrasts or comparisons - like predictions (see above) - are by default
on the *response* scale, i.e. they’re represented as difference between
probabilities (in percentage points).

[`estimate_contrasts`](https://easystats.github.io/modelbased/reference/estimate_contrasts.md)`(``m``, ``"x1"``)`` ``#> Marginal Contrasts Analysis`` ``#> `` ``#> Level1 | Level2 | Difference | SE | 95% CI | z | p`` ``#> -------------------------------------------------------------------`` ``#> 2 | 1 | -0.07 | 0.09 | [-0.25, 0.10] | -0.81 | 0.417`` ``#> 3 | 1 | 0.09 | 0.10 | [-0.11, 0.30] | 0.92 | 0.357`` ``#> 3 | 2 | 0.17 | 0.11 | [-0.05, 0.38] | 1.51 | 0.130`` ``#> `` ``#> Variable predicted: outcome`` ``#> Predictors contrasted: x1`` ``#> Predictors averaged: x2 (10), x3`` ``#> p-values are uncorrected.`` ``#> Contrasts are on the response-scale.`

The difference between the predicted probability of `x1 = 1` (21.2%) and
`x1 = 2` (13.9%) is roughly 7.3 percentage points. This difference is
not statistically significant (p = 0.417).

Contrasts or comparisons can also be represented on the link-scale, in
this case as *log-odds*. To do so, use `predict = "link"`.

[`estimate_contrasts`](https://easystats.github.io/modelbased/reference/estimate_contrasts.md)`(``m``, ``"x1"``, predict ``=`` ``"link"``)`` ``#> Marginal Contrasts Analysis`` ``#> `` ``#> Level1 | Level2 | Difference | SE | 95% CI | z | p`` ``#> -------------------------------------------------------------------`` ``#> 2 | 1 | -0.51 | 0.66 | [-1.80, 0.79] | -0.77 | 0.443`` ``#> 3 | 1 | 0.50 | 0.53 | [-0.54, 1.55] | 0.94 | 0.345`` ``#> 3 | 2 | 1.01 | 0.70 | [-0.36, 2.38] | 1.45 | 0.147`` ``#> `` ``#> Variable predicted: outcome`` ``#> Predictors contrasted: x1`` ``#> Predictors averaged: x2 (10), x3`` ``#> p-values are uncorrected.`` ``#> Contrasts are on the FALSE-transformed link-scale.`

The `transform` argument in
[`estimate_contrasts()`](https://easystats.github.io/modelbased/reference/estimate_contrasts.md)
can be used transform comparisons. For example, to transform contrasts
to *odds ratios*, we can use `transform = "exp"` in combination with
`predict = "link"`.

[`estimate_contrasts`](https://easystats.github.io/modelbased/reference/estimate_contrasts.md)`(``m``, ``"x1"``, predict ``=`` ``"link"``, transform ``=`` ``"exp"``)`` ``#> Marginal Contrasts Analysis`` ``#> `` ``#> Level1 | Level2 | Difference | 95% CI | p`` ``#> ----------------------------------------------------`` ``#> 2 | 1 | 0.60 | [0.16, 2.20] | 0.443`` ``#> 3 | 1 | 1.65 | [0.58, 4.71] | 0.345`` ``#> 3 | 2 | 2.75 | [0.70, 10.78] | 0.147`` ``#> `` ``#> Variable predicted: outcome`` ``#> Predictors contrasted: x1`` ``#> Predictors averaged: x2 (10), x3`` ``#> p-values are uncorrected.`` ``#> Contrasts are on the exp-transformed link-scale.`

[Go to next vignette: **Contrasts and Comparisons for Zero-Inflation
Models**](https://easystats.github.io/modelbased/articles/introduction_comparisons_5.html)
