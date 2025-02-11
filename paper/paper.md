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

Mention types of predictions (expectations, predictions, response vs. the link scale)

## Marginal effects

Concept of "marginal". 

Marginal means, contrasts and slopes.

## Technical details

It leverages the `get_datagrid()` function from the `insight` package to intuitively generate an appropriate grid of data points for which predictions will be computed.

The algorithmic heavy lifting is done by its two backend packages, `emmeans` and `marginaleffects`.

Differences? 

Something about the delta method?

# Examples

TODO.

# Acknowledgements

Something something.

# References