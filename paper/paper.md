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
bibliography: paper.bib
---

# Statement of need

Applied statistics have historically been focused on statistical *tests* (e.g., *t*-tests, correlation tests, ANOVAs), seen as most apt to provide researchers with interpretable answers to the questions they seek. 
However, these tests are typically based on statistical *models*, which are the true underlying cornerstone of modern data science.
The replication crisis (REF) and methodological revolution(s) (Makowski & Waggoner, 2023) have underlined the issues with the traditional focus on statistical tests (the effacement of model assumptions, a focus on null-hypothesis testing, non-compatibility with more complex variance structures), and the need to shift the focus to the models themselves.

In line with these efforts, new tools have been created to facilitate the direct usage and reporting of statistical models. For instance, the `easystats` collection of R packages has been developed to help researchers "tame, discipline, and harness" the power of statistical models. Existing packages are dedicated to model parameters (e.g., the `parameters` package; REF), predictive performance (e.g., the `performance` package; REF) or effect importance (e.g., the `effectsize` package; REF).

**But there is even more power hidden inside models!** 
Their fundamental nature - being a statistical link between an outcome $y$ and predictor variables $X$ - enables the generation predictions for any (real or not) combination of predictors.
These predictions, in turn, can be used to estimate values and effects any levels, as well as contrasting them, making it possible to visualize the model's behaviour in a more meaningful and comprehensive way and answering a wide range of research questions. 

This currently underused potential is what the `modelbased` package aims to unlock. 
It provides a unified interface to extract marginal means, marginal effects, and model predictions from a wide range of statistical models.
It is built on top of the `emmeans` (REF) and `marginaleffects` (REF) packages, which are the two most popular packages for extracting these quantities of interest from statistical models.
In line with the `easystats`' *raison d'être*, the `modelbased` package is designed to be user-friendly, with a focus on simplicity and flexibility. 


# Key concepts

## Predictions

At a fundamental level, `modelbased` and similar packages leverage model *predictions*.
These predictions can be of different types, depending on the model and the question at hand.
For instance, for linear regressions, predictions can be associated with **confidence intervals** (`predict="expectation"`) or **prediction intervals** (`predict="prediction"`).
The former corresponds to the uncertainty around the "relationship" (i.e., the estimate in the model's conditional parameters) while the second provides information about the range where observation values can actually fall.
For logistic models, predictions can be made on the **response scale** (`predict="response"`) - in terms of probability - or on the **link scale** (`predict="link"`) - in terms of log odds.

These different types of variations can be obtained for the original data, which is useful to assess the model's goodness-of-fit, or for new data (typically a "data grid"), which is useful for visualization.

For convenience, the `modelbased` package includes 4 "related" functions, that mostly differ in their default arguments for `data` and `predict`:

- `estimate_prediction()`: original data, prediction intervals.
- `estimate_expectation()`: original data, confidence intervals.
- `estimate_relation()`: data grid, predictions on the response scale. 
- `estimate_link()`: data grid, predictions on the link scale.

These functions belong to the same family, and the relevance of their output depends on the model and the research question.

## Marginal effects

The concept of "marginal" in statistical modeling refers to the effect of one variable when all other variables are held constant at specific values (e.g., their reference value, or their empirical or theoretical average). 
This is crucial for interpreting how individual predictors influence the response variable in complex models.

- `estimate_means()`: computes **Marginal Means**, i.e., the average predictions for each level of a categorical predictor, averaged across all levels of other predictors.
- `estimate_contrasts()`: computes **Marginal Contrasts**, i.e., the comparison the marginal means of different levels of a factor to assess differences or effects.
- `estimate_slopes()`: computes **Marginal Slopes**, i.e., the change in the response variable for a one-unit change in a predictor, holding all other predictors constant. They are essentially the partial derivatives of the response with respect to each predictor, useful for continuous predictors.

The modelbased package simplifies the extraction of these effects, providing a clear interface to understand how different predictors interact with outcomes in various scenarios.

[details about types of marginalization]


## Technical details

It leverages the `get_datagrid()` function from the `insight` package (REF) to intuitively generate an appropriate grid of data points for which predictions will be computed.

The algorithmic heavy lifting is done by its two backend packages, `emmeans` and `marginaleffects`.

- `emmeans` (REF) Originally, the package was known as `lsmeans`, which stands for "Least-Squares Means". 
  This term was coined by researchers in the statistical community to describe what are now more commonly referred to as "Estimated Marginal Means" or EMMs, which are essentially predictions averaged over specified levels of factors in the model while holding continuous variables at their means or other specified values.
  The term "Least-Squares Means" was somewhat misleading as it suggested a method specific to least-squares estimation, hence its renaming to `emmeans` in 2016 to clarify its scope for a wider range of models including generalized linear models, mixed models, and Bayesian models.
- `marginaleffects` (REF) was more recently introduced and also employs the delta method for variance estimation.

[What's the difference / benefits / drawbacks of using one or ther other?]

Something about the delta method?

This backend can be set as a  global option with e.g., `options(modelbased_backend = "marginaleffects")`.

# Examples

TODO.
Take a few ones from README.

# Acknowledgements

Something something.

# References