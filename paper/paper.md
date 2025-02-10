---
title: "modelbased: An R package to make the most out of your statistical models through marginal means, marginal effects, and model predictions"
tags:
  - R
  - easystats
  - marginal effects
  - marginal means
  - model predictions
authors:
- name: Dominique Makowski
  orcid: 0000-0001-5375-9967
  affiliation: "1, 2"
  corresponding: TRUE

- name: Mattan S. Ben-Shachar
  orcid: 0000-0002-4287-4801
  affiliation: 3

- name: Brenton M. Wiernik
  orcid: 0000-0001-9560-6336
  affiliation: 4

- name: Indrajeet Patil
  orcid: 0000-0003-1995-6531
  affiliation: 5

- name: Rémi Thériault
  orcid: 0000-0003-4315-6788
  affiliation: 6

- name: Daniel Lüdecke
  orcid: 0000-0002-8895-3206
  affiliation: 7

affiliations:
- name: School of Psychology, University of Sussex, Brighton, UK
  index: 1
  
- name: Sussex Centre for Consciousness Science, University of Sussex, Brighton, UK
  index: 2
  
- name: Independent Researcher, Ramat Gan, Israel
  index: 3

- name: Independent Researcher, Tampa, FL, USA
  index: 4

- name: Center for Humans and Machines, Max Planck Institute for Human Development, Berlin, Germany
  index: 5

- name: Department of Psychology, New York University, New York, NY, USA
  index: 6

- name: Institute of Medical Sociology, University Medical Center Hamburg-Eppendorf, Germany
  index: 7
correspondence: D.Makowski@sussex.ac.uk.
type: article
date: "2025-02-10"
bibliography: paper.bib
# abstract: |
#   The `modelbased` package provides a straightforward approach to computing and visualizing model-based estimates in R, including marginal means, contrasts, slopes, and predictions. Designed as part of the easystats ecosystem, it streamlines post-hoc analysis and model interpretation across a wide range of statistical models. While tools like `emmeans` and `marginaleffects` offer similar functionality, `modelbased` prioritizes ease of use, making it more accessible to researchers without extensive statistical programming expertise. By integrating modern workflows for effect estimation and visualization, modelbased enhances the interpretability and reproducibility of statistical analyses in psychological and social sciences.
keywords: |
  marginal effects; marginal means; model predictions; R; easystats
acknowledgement: |
  *{performance}* is part of the collaborative
  [*easystats*](https://github.com/easystats/easystats) ecosystem
  [@easystatspackage]. Thus, we thank all
  [members of easystats](https://github.com/orgs/easystats/people),
  contributors, and users alike.
authorcontributions: |
  D.M. drafted the paper; all authors contributed to both the writing of the
  paper and the conception of the software.
funding: |
  This research received no external funding.
conflictsofinterest: |
  The authors declare no conflict of interest.
output:
  rticles::joss_article
journal:
  pissn: 2475-9066
csl: apa.csl
---



# Statement of need

Applied statistics have historically focused on statistical *tests* (e.g., *t* tests, correlation tests, and analyses of variances, ANOVAs), seen as most apt to provide researchers with interpretable answers to the questions they seek.
However, these tests typically rely on statistical *models*—the true underlying cornerstone of modern data science.
The replication crisis [@OSC2015estimating; @camerer2018evaluating] and methodological revolutions [@makowski2023we] have underlined some of the issues with the traditional focus on statistical tests (e.g., the effacement of model assumptions, a focus on null-hypothesis testing, non-compatibility with more complex variance structures) and called for shifting the focus to the models themselves.

In line with these efforts, new tools have been created to facilitate the direct usage and reporting of statistical models. For instance, the `easystats` collection of R packages [@easystatspackage] has been developed to help researchers "tame, discipline, and harness" the power of statistical models. Existing packages are dedicated to model parameters [the `parameters` package, @ludecke2020extracting], predictive performance [the `performance` package, @ludecke2021performance] or effect importance [the `effectsize` package, @ben2020effectsize].

**But the models themselves hide even more critical information!**

The fundamental nature of these models—a statistical link between an outcome $y$ and predictor variables $X$—enables the generation of predictions for any combination of predictors, observed or unobserved alike.
These predictions refer to expected values of the outcome for given levels of predictors of interest, as well as contrasting them, making it possible to visualize the model's behaviour in a more meaningful and comprehensive way, and answering a broad range of research questions.

The two most popular R packages for extracting these quantities of interest from statistical models are `emmeans` [@russell2024emmeans] and `marginaleffects` [@arel2024interpret]. These packages pack an enormously rich set of features and cover (almost) all imaginable needs for post-hoc analysis of statistical models. However, their power and flexibility come at a cost: ease of use—especially for users not familiar with the underlying statistical concepts. The `modelbased` package, built on top of these two packages, aims to unleash this vast, untapped potential by providing a unified interface to extract marginal means, marginal effects, contrasts, comparisons, and model predictions from a wide range of statistical models. In line with the `easystats`' *raison d'être*, the `modelbased` package focuses on simplicity, flexibility, and user-friendliness to help researchers harness the full power of their models.


# Key concepts

## Terminology

Answering research questions based on statistical models means describing the relationship between predictors (also called *focal terms*) of interest and the outcome, as well as differences between observed groups in the sample. There are two ways of representing this relationship: _predictions_ and _marginal estimates_. 
1) We can describe this relationship (i.e., estimate the expected outcome value) for a very "specific" observation that is defined by a combination of predictor values (that may or may not actually appear in our data). This is what we call _predictions_. 
2) Or, we can express the relationship considering an "average" or "typical" observation (i.e., estimating the _average_ relationship between predictors and an outcome). We call this _marginal means_. 
<!-- When we talk about how strong a predictor is associated with the outcome, we use the term _marginal effects_. To specify the characteristics of predictors we're interested in, we use a _data grid_, which is a (tabular) representation of the combinations of levels for our focal terms. -->

## Predictions

At a fundamental level, `modelbased` and similar packages leverage model *predictions*.
These predictions can be of different types, depending on the model and the question at hand.
For instance, predictions can be associated with **confidence intervals** (`predict = "expectation"`) or **prediction intervals** (`predict = "prediction"`).
The former corresponds to the uncertainty around the "relationship" (i.e., the conditional estimate, typically of the expectation ($E[X]$) according to a model's parameters),
while the latter is typically larger and provides information about the range which individual observations might take (e.g., _around_ the expectation $E[X]$).
Moreover, for generalized linear models (GLMs), predictions can be made on the **response scale** (`predict = "response"`) or the **link scale** (`predict = "link"`).
This corresponds for instance to predictions in terms of probability (response scale) or log odds (link scale) for logistic regression models.

These different types of estimates can be obtained for observations in the original dataset—which is useful to assess the model's goodness-of-fit—or for new data (typically a "data grid")—which is useful for visualization.

For convenience, the `modelbased` package includes four related functions, which mostly differ in their default arguments for `data` and `predict`[^1]:

- `estimate_prediction()`: original data, prediction intervals.
- `estimate_expectation()`: original data, confidence intervals.
- `estimate_relation()`: data grid, predictions on the response scale.
- `estimate_link()`: data grid, predictions on the link scale.

[^1]: These functions can become redundant if the defaults are changed. For instance, `estimate_relation(..., predict = "link")` matches `estimate_link(...)`.

Which function to use depends on the model and the research question at hand (visualizing effects, generating predictions, etc.).


## Marginal means and effects

The concept of "marginal" in statistical modeling typically refers to the analysis of the effect of one or more "focus" variables while all other variables are held constant at specific values
(e.g., their empirical or theoretical average or reference value).
This, in turn, facilitates understanding the effect of a variable in a complex model, where multiple variables interact with each other.

The `modelbased` package simplifies the extraction of these quantities by providing a clear interface to understand how different predictors interact with outcomes in various scenarios.

- `estimate_means()`: computes **Marginal Means**—the average predictions for each level of a categorical predictor, averaged across all levels of other predictors.
- `estimate_contrasts()`: computes **Marginal Contrasts**—the comparison of marginal means at different levels of a factor to assess differences or effects.
- `estimate_slopes()`: computes **Marginal Effects**—the change in the response variable for an infinitesimal change in a predictor, averaged across all levels of other predictors. Marginal effects are essentially the partial derivatives of the response with respect to each predictor (useful for continuous predictors) and describe the average strength of an effect.


**[TODO: details about types of marginalization]**

`modelbased` provides two types of "marginalization": 

- `estimate = "average"` (default): TODO
- `estimate = "population"`: TODO



## Technical details

The algorithmic heavy lifting is done by `modelbased`'s two back-end packages, `emmeans` and `marginaleffects` (the default), which can be set as a global option (e.g., with `options(modelbased_backend = "emmeans")`).

Of the two, `emmeans` [@russell2024emmeans] is the more senior package and was originally known as `lsmeans` (for "Least-Squares Means").
This term has been historically used to describe what are now more commonly referred to as "Estimated Marginal Means" or EMMs:
predictions made over a regular grid—a grid typically constructed from all possible combinations of the categorical predictors in the model and the mean of numerical predictors.
The package was renamed in 2016 to `emmeans` to clarify its extension beyond least-squares estimation and its support of a wider range of models (e.g., Bayesian models).

Within `emmeans`, estimates are generated as a linear function of the model's coefficients,
with standard errors (SEs) produced in a similar manner by taking a linear combination of the coefficients' variance-covariance matrix.
For example if $b$ is a vector of 4 coefficients, and $V$ is a 4-by-4 matrix of the coefficients' variance-covariance, we can get an estimate and SE for a linear combination (or set of linear combinations) $L$ like so:

$$
\hat{b} = L \cdot b
$$

$$
SE_{\hat{b}} = \sqrt{\text{diag}(L \cdot V \cdot L^T)}
$$



These grid predictions are sometimes averaged over (averaging being a linear operation itself)
to produce "marginal" predictions (in the sense of marginalized-over): means.
These predictions can then be contrasted using various built-in or custom contrasts weights to obtain meaningful estimates of various effects.
Using linear combinations with regular grids often means that results from `emmeans` directly correspond to a models coefficients (which is a benefit for those who are used to understanding models by examining coefficient tables).

`marginaleffects` [@arel2024interpret] was more recently introduced, and uses a different approach:
various effects are estimated by generating two counter-factual predictions of unit-level observations, and then taking the difference between them [with SEs computed using the delta method, @arel2024interpret].
By default, such effects are estimated for every observation in the original model frame.
These unit-level effects are typically averaged to obtain average effects (e.g., an Average Treatment Effect, ATE).

Using the delta method affords more flexibility in the specification of the marginal effects to be estimated.
For example, while `emmeans` by default compares predictions from GLMs on the link scale
and then transforms the comparison back to something closer to the response scales
(e.g., the difference between two log-odds is taken, and then exponentiation to produce odds ratios), `marginaleffects` by default compares predictions on the response scale directly (e.g, taking the difference between two probabilities).
The delta method implemented in `marginaleffects` uses iterative estimation, making it more computationally costly relative to the simple matrix multiplication used for estimating linear combinations (though `marginaleffects` is very efficient).

This means that while `emmeans` typically produces _effects at the mean_, `marginaleffects` typically produces _mean effects_.
Depending on the quantity of interest, model, use of a link function, design balance and weights, these can be nearly identical, or very different.

Note that `emmeans` can also use the delta method and can use non-regular grids, and `marginaleffects` can also generate linear predictions at the mean.
However, obtaining these requires some deeper knowledge of the relevant packages.

Finally, `modelbased` leverages the `get_datagrid()` function from the `insight` package [@ludecke2019insight] to intuitively generate an appropriate grid of data points for which predictions or effects or slopes will be estimated.
Since these packages support a wider range of models—including generalized linear models, mixed models, and Bayesian models—`modelbased` also inherits the support for such models.

# Examples

The `iris` dataset contains measures in centimeters of three different species of iris flowers [setosa, versicolor, and virginica, @becker1988new]. Imagine the following linear model in which we predict those flowers' petal width (`Petal.Width`) from the interaction between their petal length (`Petal.Length`) and their `Species`.


``` r
library(easystats)

model <- lm(Petal.Width ~ Petal.Length * Species, data = iris)

parameters::parameters(model)
```

```
#> Parameter                           | Coefficient |   SE |        95% CI
#> ------------------------------------------------------------------------
#> (Intercept)                         |       -0.05 | 0.21 | [-0.47, 0.38]
#> Petal Length                        |        0.20 | 0.15 | [-0.09, 0.49]
#> Species [versicolor]                |       -0.04 | 0.32 | [-0.66, 0.59]
#> Species [virginica]                 |        1.18 | 0.33 | [ 0.52, 1.84]
#> Petal Length × Species [versicolor] |        0.13 | 0.16 | [-0.18, 0.44]
#> Petal Length × Species [virginica]  |       -0.04 | 0.15 | [-0.34, 0.26]
#> 
#> Parameter                           | t(144) |      p
#> -----------------------------------------------------
#> (Intercept)                         |  -0.22 | 0.823 
#> Petal Length                        |   1.38 | 0.170 
#> Species [versicolor]                |  -0.11 | 0.909 
#> Species [virginica]                 |   3.54 | < .001
#> Petal Length × Species [versicolor] |   0.83 | 0.405 
#> Petal Length × Species [virginica]  |  -0.27 | 0.789
```

```
#> 
#> Uncertainty intervals (equal-tailed) and p-values (two-tailed) computed
#>   using a Wald t-distribution approximation.
```

The model's **parameters** can be challenging to interpret and do not offer us all the insights that this model actually contains.

## Visualize relationship

We can start by easily visualizing the relationship between our response variable and our predictors (Figure 1).


``` r
estimate_relation(model, by = c("Petal.Length", "Species"), length = 100) |>
  plot()
```

\begin{figure}
\includegraphics[width=1\linewidth]{paper_files/figure-latex/fig1-1} \caption{Scatter plot of petal length by pelal width, grouped by species}\label{fig:fig1}
\end{figure}

But what is the **average value** of `Petal.Width` for each species?

## Marginal Means

The marginal means can be computed, which are the mean predictions for each level of a categorical predictor, *averaged across* all levels of other predictors (`Petal.Length` in this case).


``` r
estimate_means(model, by = "Species")
```

```
#> Estimated Marginal Means
#> 
#> Species    | Mean |   SE |       95% CI | t(144)
#> ------------------------------------------------
#> setosa     | 0.71 | 0.34 | [0.04, 1.37] |   2.11
#> versicolor | 1.16 | 0.04 | [1.09, 1.23] |  31.44
#> virginica  | 1.74 | 0.09 | [1.57, 1.91] |  20.20
#> 
#> Variable predicted: Petal.Width
#> Predictors modulated: Species
#> Predictors averaged: Petal.Length (3.8)
```

However, are these different species **significantly different** from each other?

## Marginal Contrasts

We can estimate all the pairwise contrasts between the levels of the `Species` factor.


``` r
estimate_contrasts(model, contrast = "Species")
```

```
#> Marginal Contrasts Analysis
#> 
#> Level1     | Level2     | Difference |   SE |        95% CI | t(144) |      p
#> -----------------------------------------------------------------------------
#> versicolor | setosa     |       0.45 | 0.34 | [-0.22, 1.12] |   1.34 |  0.183
#> virginica  | setosa     |       1.03 | 0.35 | [ 0.35, 1.72] |   2.97 |  0.003
#> virginica  | versicolor |       0.58 | 0.09 | [ 0.39, 0.76] |   6.18 | < .001
#> 
#> Variable predicted: Petal.Width
#> Predictors contrasted: Species
#> Predictors averaged: Petal.Length (3.8)
#> p-values are uncorrected.
```

As we can see, the average difference between *versicolor* and *setosa* is not significant.

## Marginal Slopes

Similarly, we can compute the marginal effect of `Petal.Length` (i.e., the "slope") for each species.


``` r
estimate_slopes(model, trend = "Petal.Length", by = "Species")
```

```
#> Estimated Marginal Effects
#> 
#> Species    | Slope |   SE |        95% CI |    t |      p
#> ---------------------------------------------------------
#> setosa     |  0.20 | 0.15 | [-0.08, 0.49] | 1.38 |  0.168
#> versicolor |  0.33 | 0.05 | [ 0.23, 0.44] | 6.14 | < .001
#> virginica  |  0.16 | 0.05 | [ 0.07, 0.25] | 3.49 | < .001
#> 
#> Marginal effects estimated for Petal.Length
#> Type of slope was dY/dX
```

This shows that there is a significant positive relationship between `Petal.Length` and `Petal.Width` for all species but *setosa*.

## Marginal Contrasts of Slopes

Finally, we can even compute the contrasts between the slopes of `Petal.Length` for each species.


``` r
estimate_contrasts(model, contrast = "Petal.Length", by = "Species")
```

```
#> Marginal Contrasts Analysis
#> 
#> Level1     | Level2     | Difference |   SE |         95% CI |     t |     p
#> ----------------------------------------------------------------------------
#> versicolor | setosa     |       0.13 | 0.16 | [-0.17,  0.43] |  0.83 | 0.404
#> virginica  | setosa     |      -0.04 | 0.15 | [-0.34,  0.26] | -0.27 | 0.789
#> virginica  | versicolor |      -0.17 | 0.07 | [-0.31, -0.03] | -2.41 | 0.016
#> 
#> Variable predicted: Petal.Width
#> Predictors contrasted: Petal.Length
#> Predictors averaged: Petal.Length (3.8)
#> p-values are uncorrected.
```

The effect of `Petal.Length` on `Petal.Width` is significantly stronger in *virginica* compared to *versicolor*.

# Conclusion

The `modelbased` package provides a simple and intuitive interface to extract and visualize important information contained within statistical models.

# Declarations

## Funding information

This research received no external funding.

## Competing Interests

The authors declare no conflict of interest

## Availability of data and materials (data transparency)

All data used in this paper uses data included with base R.

## Code availability

The `modelbased` package is available at the package official website (https://easystats.github.io/modelbased), on CRAN (https://cran.r-project.org/package=modelbased), and on the R-Universe (https://easystats.r-universe.dev/modelbased). The source code is available on GitHub (https://github.com/easystats/modelbased), and the package can be installed from CRAN with `install.packages("modelbased")`.

## Contributions

DM: Writing- Original draft preparation, Writing- Reviewing and Editing, Software. MSB-S, BMW, IP, RT, and DL: Writing- Reviewing and Editing, Software.

## Acknowledgements

*{modelbased}* is part of the collaborative [*easystats*](https://github.com/easystats/easystats) ecosystem [@easystatspackage]. Thus, we thank all [members of easystats](https://github.com/orgs/easystats/people), contributors, and users alike.

# References
