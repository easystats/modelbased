---
title: 'modelbased: An R package to make the most out of your statistical models through marginal means, marginal effects, and model predictions'
tags:
  - R
  - easystats
  - marginal effects
  - marginal means
  - model predictions
authors:
  - name: Adrian M. Price-Whelan
    orcid: 0000-0000-0000-0000
    equal-contrib: true
    affiliation: "1, 2" # (Multiple affiliations must be quoted)
  - name: Author Without ORCID
    equal-contrib: true # (This is how you can denote equal contributions between multiple authors)
    affiliation: 2
  - name: Author with no affiliation
    corresponding: true # (This is how to denote the corresponding author)
    affiliation: 3
  - given-names: Ludwig
    dropping-particle: van
    surname: Beethoven
    affiliation: 3
affiliations:
 - name: Lyman Spitzer, Jr. Fellow, Princeton University, United States
   index: 1
   ror: 00hx57361
 - name: Institution Name, Country
   index: 2
 - name: Independent Researcher, Country
   index: 3
date: 13 August 2017
---

# Statement of need

Applied statistics have historically been focused on statistical *tests* (e.g., *t*-tests, correlation tests, ANOVAs), seen as most apt to provide researchers with interpretable answers to the questions they seek.
However, these tests are typically based on statistical *models*, which are the true underlying cornerstone of modern data science.
The replication crisis (REF) and methodological revolution(s) (Makowski & Waggoner, 2023) have underlined the issues with the traditional focus on statistical tests (the effacement of model assumptions, a focus on null-hypothesis testing, non-compatibility with more complex variance structures), and the need to shift the focus to the models themselves.

In line with these efforts, new tools have been created to facilitate the direct usage and reporting of statistical models. For instance, the `easystats` collection of R packages has been developed to help researchers "tame, discipline, and harness" the power of statistical models. Existing packages are dedicated to model parameters (e.g., the `parameters` package; REF), predictive performance (e.g., the `performance` package; REF) or effect importance (e.g., the `effectsize` package; REF).

**But there is even more power hidden inside models!**
Their fundamental nature - being a statistical link between an outcome $y$ and predictor variables $X$ - enables the generation of predictions for any combination of predictors - typically observed combinations, but often unobserved (counter-factual) ones.
These predictions, in turn, can be used to estimate expected values of the outcome  and predictor effects of any levels, as well as contrasting them, making it possible to visualize the model's behaviour in a more meaningful and comprehensive way and answering a wide range of research questions.

The two probably most popular R packages for extracting these quantities of interest from statistical models are `emmeans` (REF) and `marginaleffects` (REF). These packages are enormously feature rich and (almost) cover all imaginable needs for post-hoc analysis of statistical models. However, these packages are not always easy to use, especially for users who are not familiar with the underlying statistical concepts. The `modelbased` package aims to unlock this currently underused potential by providing a unified interface to extract marginal means, marginal effects, contrasts, comparisons and model predictions from a wide range of statistical models. It is built on top of the two aforementioned `emmeans` and `marginaleffects` packages. In line with the `easystats`' *raison d'être*, the `modelbased` package is designed to be user-friendly, with a focus on simplicity and flexibility.


# Key concepts

- Conceptual differences between predictions and marginal means?
- Concept of focal and non-focal terms?
- For marginalization, we can refer to non-focal terms, meaning it would be good to have the concept introduced somewhere

## Predictions

At a fundamental level, `modelbased` and similar packages leverage model *predictions*.
These predictions can be of different types, depending on the model and the question at hand.
For instance, for linear regressions, predictions can be associated with **confidence intervals** (`predict="expectation"`) or **prediction intervals** (`predict="prediction"`).
The former corresponds to the uncertainty around the "relationship" (i.e., the conditional estimate, typically of the expectation ($E[X]$) according to a model's parameters)
while the latter provides information about the range individual observations might take (e.g., _around_ the expectation $E[X]$).
For generalized linear models (GLMs), <!-- while the distinction between prediction and confidence intervals do not apply, -->
predictions can be made on the **response scale** (`predict="response"`) or the **link scale** (`predict="link"`).
This corresponds for instance to predictions in terms of probability (response scale) or log odds (link scale) for logistic regression models.

These different types of estimates can be obtained for observation in the original dataset,
which is useful to assess the model's goodness-of-fit,
or for new data (typically a "data grid"), which is useful for visualization and counter-factual reasoning.

For convenience, the `modelbased` package includes 4 related functions, that mostly differ in their default arguments for `data` and `predict`:

- `estimate_prediction()`: original data, prediction intervals.
- `estimate_expectation()`: original data, confidence intervals.
- `estimate_relation()`: data grid, predictions on the response scale.
- `estimate_link()`: data grid, predictions on the link scale.

*Note:* if the defaults are changed, then these functions can become redundant.
For instance, `estimate_relation(..., predict="link")` will be equivalent to `estimate_link(...)`.

These functions belong to the same family, and their relevance depends on the model and the research question.


## Marginal means and effects

The concept of "marginal" in statistical modeling refers to the effect of one variable when all other variables are held constant at specific values
(e.g., their reference value, or their empirical or theoretical average).
This is crucial for interpreting how individual predictors influence the response variable in complex models.

- `estimate_means()`: computes **Marginal Means**, i.e., the average predictions for each level of a categorical predictor, averaged across all levels of other predictors.
- `estimate_contrasts()`: computes **Marginal Contrasts**, i.e., the comparison the marginal means of different levels of a factor to assess differences or effects.
- `estimate_slopes()`: computes **Marginal Slopes**, i.e., the change in the response variable for an infinitesimal change in a predictor, averaged across all levels of other predictors. They are essentially the partial derivatives of the response with respect to each predictor, useful for continuous predictors.

The modelbased package simplifies the extraction of these quantities, providing a clear interface to understand how different predictors interact with outcomes in various scenarios.

[details about types of marginalization]


## Technical details

The algorithmic heavy lifting is done by its two backend packages, `emmeans` and `marginaleffects`,
which can be set as a global option with (e.g., `options(modelbased_backend = "marginaleffects")`).

Of the two, `emmeans` (REF) is the more senior package;
Originally, the package was known as `lsmeans`, which stands for "Least-Squares Means".
This term has been historically used to describe what are now more commonly referred to as "Estimated Marginal Means" or EMMs:
predictions made over a regular grid - a counter-factual dataset containing all combinations of the categorical predictors in the model and typically the mean of numerical predictors.
The package was renamed in 2016 to `emmeans` to clarify it is not specific to least-squares estimation.

Within `emmeans, estimated marginal means are generated as a linear function of the model's coefficients,
with standard errors (SEs) produced in a similar manner by taking a linear combination of the coefficients' variance-covariance matrix.
For example if $b$ is a vector of 4 coefficients, and $V$ is a 4-by-4 matrix of the coefficients' variance-covariance,
we can get an estimate and SE for a linear combination (or set of linear combinations) $L$ like so:

```
L %*% b
sqrt(L %*% V %*% L)
```

These grid predictions are sometimes averaged over (averaging being a linear operation itself)
to produce "marginal" (in the sense of marginalized-over) predictions - means.
These predictions can then be contrasted using various built-in or custom contrasts weights to obtain meaningful estimates of various effects.
Using linear combinations with regular grids often means that results from `emmeans` directly correspond to a models coefficients
(which is a benefit for those who are used to understanding models by examining coefficient tables).

`marginaleffects` (REF) was more recently introduced, and used a different approach:
various effects are estimated by generating two counter-factual predictions of unit-level observations,
and then taking the difference between them (with SEs computed using the delta method; REF).
By default, such effects are estimated for every observation in the original model frame.
These unit-level effects are typically averaged to obtain average effects (e.g., an average treatment effect, ATE).

Using the delta method allows for more flexibility in the specification of the marginal effects to be estimated.
For example, while `emmeans` by default compares predictions from GLMs on the link scale
and then transforms the comparison back to something closer to the response scales
(e.g., the difference between two log-odds is taken, and then exponentiation to produce odds ratios),
`marginaleffects` by default compares predictions on the response scale directly (e.g, taking the difference between two probabilities).
The delta method's implemented in `marginaleffects` uses iterative estimation,
making it more computationally costly relative to the simple simple matrix multiplication used for estimating linear combinations
(though `marginaleffects` is very efficient).

This means that while `emmeans` typically produces _effects at the mean_, `marginaleffects` typically produces _mean effects_.
Depending on the quantity of interest, model, use of a link function, design balance and weights, these can be nearly identical, or very very different.

Of course, `emmeans` can also use the delta method and can use non-regular grids,
and `marginaleffects` can also generate linear predictions at the mean.
But to obtain these requires a higher degree of competency in the relevant packages than perhaps most users have.


`modelbased` leverages `get_datagrid()` function from the `insight` package (REF)
to intuitively generate an appropriate grid of data points for which predictions or effects or slopes will be estimated.
And since these two packages support a wider range of models - including generalized linear models, mixed models, and Bayesian models -
this means that `modelbased` also inherits the support for such models.


# Examples

Imagine the following linear model in which we predict flower's `Petal.Width` from the interaction between `Petal.Length` and `Species`.

```r
library(easystats)

model <- lm(Petal.Width ~ Petal.Length * Species, data = iris)

parameters::parameters(model)
```
```
Parameter                           | Coefficient |   SE |        95% CI | t(144) |      p
------------------------------------------------------------------------------------------
(Intercept)                         |       -0.05 | 0.21 | [-0.47, 0.38] |  -0.22 | 0.823
Petal Length                        |        0.20 | 0.15 | [-0.09, 0.49] |   1.38 | 0.170
Species [versicolor]                |       -0.04 | 0.32 | [-0.66, 0.59] |  -0.11 | 0.909
Species [virginica]                 |        1.18 | 0.33 | [ 0.52, 1.84] |   3.54 | < .001
Petal Length × Species [versicolor] |        0.13 | 0.16 | [-0.18, 0.44] |   0.83 | 0.405
Petal Length × Species [virginica]  |       -0.04 | 0.15 | [-0.34, 0.26] |  -0.27 | 0.789
```

The **parameters** of this model can be a bit hard to interpret and do not offer us all the insights that we could get from this model.

## Visualize relationship

We can start by easily visualizing the relationship between our response variable and our predictors.

```r
estimate_relation(model, by=c("Petal.Length", "Species"), length=100) |>
  plot()
```
**ADD PLOT**

But what is the **average value** of `Petal.Width` for each species?

## Marginal Means

The marginal means can be computed, which are the mean predictions for each level of a categorical predictor, *averaged across* all levels of other predictors (`Petal.Length` in this case).

```r
estimate_means(model, by="Species")
```
```
Estimated Marginal Means

Species    | Mean |   SE |       95% CI
---------------------------------------
setosa     | 0.71 | 0.34 | [0.04, 1.37]
versicolor | 1.16 | 0.04 | [1.09, 1.23]
virginica  | 1.74 | 0.09 | [1.57, 1.91]

Marginal means estimated at Species
```

But are these different species **significantly different** from each other?

## Marginal Contrasts

We can estimate all the pairwise contrasts between the levels of the `Species` factor.

```r
estimate_contrasts(model, contrast="Species")
```
```
Marginal Contrasts Analysis

Level1     |     Level2 | Difference |         95% CI |   SE | t(144) |      p
------------------------------------------------------------------------------
setosa     | versicolor |      -0.45 | [-1.27,  0.37] | 0.34 |  -1.34 | 0.183
setosa     |  virginica |      -1.03 | [-1.87, -0.19] | 0.35 |  -2.97 | 0.007
versicolor |  virginica |      -0.58 | [-0.81, -0.35] | 0.09 |  -6.18 | < .001

Marginal contrasts estimated at Species
p-value adjustment method: Holm (1979)
```

As we can see, the average difference betweeen *setosa* and *versicolor* is not significant.

## Marginal Slopes

Similarly, we can compute the marginal effect of `Petal.Length` (i.e., the "slope") for each species.

```r
estimate_slopes(model, trend="Petal.Length", by="Species")
```
```
Estimated Marginal Effects

Species    | Coefficient |   SE |        95% CI | t(144) |      p
-----------------------------------------------------------------
setosa     |        0.20 | 0.15 | [-0.09, 0.49] |   1.38 | 0.170
versicolor |        0.33 | 0.05 | [ 0.22, 0.44] |   6.14 | < .001
virginica  |        0.16 | 0.05 | [ 0.07, 0.25] |   3.49 | < .001
Marginal effects estimated for Petal.Length
```

This shows that there is a significant positive relationship between `Petal.Length` and `Petal.Width` for all species but *setosa*

## Marginal Contrasts of Slopes

Finally, we can even compute the contrasts between the slopes of `Petal.Length` for each species.

```r
estimate_contrasts(model, contrast="Petal.Length", by="Species")
```
```
Marginal Contrasts Analysis

Parameter              | Coefficient |   SE |        95% CI |     t |     p
---------------------------------------------------------------------------
setosa - versicolor    |       -0.13 | 0.16 | [-0.43, 0.17] | -0.83 | 0.808
setosa - virginica     |        0.04 | 0.15 | [-0.26, 0.34] |  0.27 | 0.808
versicolor - virginica |        0.17 | 0.07 | [ 0.03, 0.31] |  2.41 | 0.048

Marginal contrasts estimated at Petal.Length
p-value adjustment method: Holm (1979)
```

The effect of `Petal.Length` on `Petal.Width` is significantly stronger in *virginica* compared to *versicolor*.

# Conclusion

The `modelbased` package provides a simple and intuitive interface to extract and visualize important information contained within statistical models.

# Acknowledgements

Something something.

# References
