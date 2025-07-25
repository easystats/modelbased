% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/estimate_slopes.R
\name{estimate_slopes}
\alias{estimate_slopes}
\title{Estimate Marginal Effects}
\usage{
estimate_slopes(
  model,
  trend = NULL,
  by = NULL,
  predict = NULL,
  ci = 0.95,
  estimate = NULL,
  transform = NULL,
  p_adjust = "none",
  keep_iterations = FALSE,
  backend = NULL,
  verbose = TRUE,
  ...
)
}
\arguments{
\item{model}{A statistical model.}

\item{trend}{A character indicating the name of the variable for which to
compute the slopes. To get marginal effects at specific values, use
\code{trend="<variable>"} along with the \code{by} argument, e.g.
\code{by="<variable>=c(1, 3, 5)"}, or a combination of \code{by} and \code{length}, for
instance, \verb{by="<variable>", length=30}. To calculate average marginal
effects over a range of values, use \code{trend="<variable>=seq(1, 3, 0.1)"} (or
similar) and omit the variable provided in \code{trend} from the \code{by} argument.}

\item{by}{The (focal) predictor variable(s) at which to evaluate the desired
effect / mean / contrasts. Other predictors of the model that are not
included here will be collapsed and "averaged" over (the effect will be
estimated across them). \code{by} can be a character (vector) naming the focal
predictors, optionally including representative values or levels at which
focal predictors are evaluated (e.g., \code{by = "x = c(1, 2)"}). When \code{estimate}
is \emph{not} \code{"average"}, the \code{by} argument is used to create a "reference grid"
or "data grid" with representative values for the focal predictors. In this
case, \code{by} can also be list of named elements. See details in
\code{\link[insight:get_datagrid]{insight::get_datagrid()}} to learn more about how to create data grids for
predictors of interest.}

\item{predict}{Is passed to the \code{type} argument in \code{emmeans::emmeans()} (when
\code{backend = "emmeans"}) or in \code{marginaleffects::avg_predictions()} (when
\code{backend = "marginaleffects"}). Valid options for \code{predict} are:
\itemize{
\item \code{backend = "marginaleffects"}: \code{predict} can be \code{"response"}, \code{"link"},
\code{"inverse_link"} or any valid \code{type} option supported by model's class
\code{predict()} method (e.g., for zero-inflation models from package
\strong{glmmTMB}, you can choose \code{predict = "zprob"} or \code{predict = "conditional"}
etc., see \link[glmmTMB:predict.glmmTMB]{glmmTMB::predict.glmmTMB}). By default, when \code{predict = NULL},
the most appropriate transformation is selected, which usually returns
predictions or contrasts on the response-scale. The \code{"inverse_link"} is a
special option, comparable to \emph{marginaleffects}' \code{invlink(link)} option. It
will calculate predictions on the link scale and then back-transform to the
response scale.
\item \code{backend = "emmeans"}: \code{predict} can be \code{"response"}, \code{"link"}, \code{"mu"},
\code{"unlink"}, or \code{"log"}. If \code{predict = NULL} (default), the most appropriate
transformation is selected (which usually is \code{"response"}). See also
\href{https://CRAN.R-project.org/package=emmeans/vignettes/transformations.html}{this vignette}.
}

See also section \emph{Predictions on different scales}.}

\item{ci}{Confidence Interval (CI) level. Default to \code{0.95} (\verb{95\%}).}

\item{estimate}{The \code{estimate} argument determines how predictions are
averaged ("marginalized") over variables not specified in \code{by} or \code{contrast}
(non-focal predictors). It controls whether predictions represent a "typical"
individual, an "average" individual from the sample, or an "average"
individual from a broader population.
\itemize{
\item \code{"typical"} (Default): Calculates predictions for a balanced data grid
representing all combinations of focal predictor levels (specified in \code{by}).
For non-focal numeric predictors, it uses the mean; for non-focal
categorical predictors, it marginalizes (averages) over the levels. This
represents a "typical" observation based on the data grid and is useful for
comparing groups. It answers: "What would the average outcome be for a
'typical' observation?". This is the default approach when estimating
marginal means using the \emph{emmeans} package.
\item \code{"average"}: Calculates predictions for each observation in the sample and
then averages these predictions within each group defined by the focal
predictors. This reflects the sample's actual distribution of non-focal
predictors, not a balanced grid. It answers: "What is the predicted value
for an average observation in my data?"
\item \code{"population"}: "Clones" each observation, creating copies with all
possible combinations of focal predictor levels. It then averages the
predictions across these "counterfactual" observations (non-observed
permutations) within each group. This extrapolates to a hypothetical
broader population, considering "what if" scenarios. It answers: "What is
the predicted response for the 'average' observation in a broader possible
target population?" This approach entails more assumptions about the
likelihood of different combinations, but can be more apt to generalize.
This is also the option that should be used for \strong{G-computation}
(causal inference, see \emph{Chatton and Rohrer 2024}).
}

You can set a default option for the \code{estimate} argument via \code{options()},
e.g. \code{options(modelbased_estimate = "average")}.

Note following limitations:
\itemize{
\item When you set \code{estimate} to \code{"average"}, it calculates the average based
only on the data points that actually exist. This is in particular
important for two or more focal predictors, because it doesn't generate a
\emph{complete} grid of all theoretical combinations of predictor values.
Consequently, the output may not include all the values.
\item Filtering the output at values of continuous predictors, e.g.
\code{by = "x=1:5"}, in combination with \code{estimate = "average"} may result in
returning an empty data frame because of what was described above. In such
case, you can use \code{estimate = "typical"} or use the \code{newdata} argument to
provide a data grid of predictor values at which to evaluate predictions.
\item \code{estimate = "population"} is not available for \code{estimate_slopes()}.
}}

\item{transform}{A function applied to predictions and confidence intervals
to (back-) transform results, which can be useful in case the regression
model has a transformed response variable (e.g., \code{lm(log(y) ~ x)}). For
Bayesian models, this function is applied to individual draws from the
posterior distribution, before computing summaries. Can also be \code{TRUE}, in
which case \code{insight::get_transformation()} is called to determine the
appropriate transformation-function. Note that no standard errors are returned
when transformations are applied.}

\item{p_adjust}{The p-values adjustment method for frequentist multiple
comparisons. For \code{estimate_slopes()}, multiple comparison only occurs for
Johnson-Neyman intervals, i.e. in case of interactions with two numeric
predictors (one specified in \code{trend}, one in \code{by}). In this case, the
\code{"esarey"} or \code{"sup-t"} options are recommended, but \code{p_adjust} can also be
one of \code{"none"} (default), \code{"hochberg"}, \code{"hommel"}, \code{"bonferroni"}, \code{"BH"},
\code{"BY"}, \code{"fdr"}, \code{"tukey"}, \code{"sidak"}, or \code{"holm"}. \code{"sup-t"} computes
simultaneous confidence bands, also called sup-t confidence band (Montiel
Olea & Plagborg-Møller, 2019).}

\item{keep_iterations}{If \code{TRUE}, will keep all iterations (draws) of
bootstrapped or Bayesian models. They will be added as additional columns
named \code{iter_1}, \code{iter_2}, and so on. If \code{keep_iterations} is a positive
number, only as many columns as indicated in \code{keep_iterations} will be added
to the output. You can reshape them to a long format by running
\code{\link[bayestestR:reshape_iterations]{bayestestR::reshape_iterations()}}.}

\item{backend}{Whether to use \code{"marginaleffects"} (default) or \code{"emmeans"} as
a backend. Results are usually very similar. The major difference will be
found for mixed models, where \code{backend = "marginaleffects"} will also average
across random effects levels, producing "marginal predictions" (instead of
"conditional predictions", see Heiss 2022).

Another difference is that \code{backend = "marginaleffects"} will be slower than
\code{backend = "emmeans"}. For most models, this difference is negligible. However,
in particular complex models or large data sets fitted with \emph{glmmTMB} can be
significantly slower.

You can set a default backend via \code{options()}, e.g. use
\code{options(modelbased_backend = "emmeans")} to use the \strong{emmeans} package or
\code{options(modelbased_backend = "marginaleffects")} to set \strong{marginaleffects} as
default backend.}

\item{verbose}{Use \code{FALSE} to silence messages and warnings.}

\item{...}{Other arguments passed, for instance, to \code{\link[insight:get_datagrid]{insight::get_datagrid()}},
to functions from the \strong{emmeans} or \strong{marginaleffects} package, or to process
Bayesian models via \code{\link[bayestestR:describe_posterior]{bayestestR::describe_posterior()}}. Examples:
\itemize{
\item \code{insight::get_datagrid()}: Argument such as \code{length}, \code{digits} or \code{range}
can be used to control the (number of) representative values. For integer
variables, \code{protect_integers} modulates whether these should also be
treated as numerics, i.e. values can have fractions or not.
\item \strong{marginaleffects}: Internally used functions are \code{avg_predictions()} for
means and contrasts, and \code{avg_slope()} for slopes. Therefore, arguments for
instance like \code{vcov}, \code{equivalence}, \code{df}, \code{slope}, \code{hypothesis} or even
\code{newdata} can be passed to those functions. A \code{weights} argument is passed
to the \code{wts} argument in \code{avg_predictions()} or \code{avg_slopes()}, however,
weights can only be applied when \code{estimate} is \code{"average"} or
\code{"population"} (i.e. for those marginalization options that do not use data
grids). Other arguments, such as \code{re.form} or \code{allow.new.levels}, may be
passed to \code{predict()} (which is internally used by \emph{marginaleffects}) if
supported by that model class.
\item \strong{emmeans}: Internally used functions are \code{emmeans()} and \code{emtrends()}.
Additional arguments can be passed to these functions.
\item Bayesian models: For Bayesian models, parameters are cleaned using
\code{describe_posterior()}, thus, arguments like, for example, \code{centrality},
\code{rope_range}, or \code{test} are passed to that function.
\item Especially for \code{estimate_contrasts()} with integer focal predictors, for
which contrasts should be calculated, use argument \code{integer_as_numeric} to
set the maximum number of unique values in an integer predictor to treat
that predictor as "discrete integer" or as numeric. For the first case,
contrasts are calculated between values of the predictor, for the latter,
contrasts of slopes are calculated. If the integer has more than
\code{integer_as_numeric} unique values, it is treated as numeric. Defaults to
\code{5}.
\item For count regression models that use an offset term, use \verb{offset = <value>}
to fix the offset at a specific value. Or use \code{estimate = "average"}, to
average predictions over the distribution of the offset (if appropriate).
}}
}
\value{
A data.frame of class \code{estimate_slopes}.
}
\description{
Estimate the slopes (i.e., the coefficient) of a predictor over or within
different factor levels, or alongside a numeric variable. In other words, to
assess the effect of a predictor \emph{at} specific configurations data. It corresponds
to the derivative and can be useful to understand where a predictor has a
significant role when interactions or non-linear relationships are present.

Other related functions based on marginal estimations includes
\code{\link[=estimate_contrasts]{estimate_contrasts()}} and \code{\link[=estimate_means]{estimate_means()}}.

See the \strong{Details} section below, and don't forget to also check out the
\href{https://easystats.github.io/modelbased/articles/estimate_slopes.html}{Vignettes}
and \href{https://easystats.github.io/modelbased/index.html#features}{README examples} for
various examples, tutorials and use cases.
}
\details{
The \code{\link[=estimate_slopes]{estimate_slopes()}}, \code{\link[=estimate_means]{estimate_means()}} and \code{\link[=estimate_contrasts]{estimate_contrasts()}}
functions are forming a group, as they are all based on \emph{marginal}
estimations (estimations based on a model). All three are built on the
\strong{emmeans} or \strong{marginaleffects} package (depending on the \code{backend}
argument), so reading its documentation (for instance \code{\link[emmeans:emmeans]{emmeans::emmeans()}},
\code{\link[emmeans:emtrends]{emmeans::emtrends()}} or this \href{https://marginaleffects.com/}{website}) is
recommended to understand the idea behind these types of procedures.
\itemize{
\item Model-based \strong{predictions} is the basis for all that follows. Indeed, the
first thing to understand is how models can be used to make predictions
(see \code{\link[=estimate_link]{estimate_link()}}). This corresponds to the predicted response (or
"outcome variable") given specific predictor values of the predictors
(i.e., given a specific data configuration). This is why the concept of
\code{\link[insight:get_datagrid]{reference grid()}} is so important for direct
predictions.
\item \strong{Marginal "means"}, obtained via \code{\link[=estimate_means]{estimate_means()}}, are an extension of
such predictions, allowing to "average" (collapse) some of the predictors,
to obtain the average response value at a specific predictors
configuration. This is typically used when some of the predictors of
interest are factors. Indeed, the parameters of the model will usually give
you the intercept value and then the "effect" of each factor level (how
different it is from the intercept). Marginal means can be used to directly
give you the mean value of the response variable at all the levels of a
factor. Moreover, it can also be used to control, or average over
predictors, which is useful in the case of multiple predictors with or
without interactions.
\item \strong{Marginal contrasts}, obtained via \code{\link[=estimate_contrasts]{estimate_contrasts()}}, are themselves
at extension of marginal means, in that they allow to investigate the
difference (i.e., the contrast) between the marginal means. This is, again,
often used to get all pairwise differences between all levels of a factor.
It works also for continuous predictors, for instance one could also be
interested in whether the difference at two extremes of a continuous
predictor is significant.
\item Finally, \strong{marginal effects}, obtained via \code{\link[=estimate_slopes]{estimate_slopes()}}, are
different in that their focus is not values on the response variable, but
the model's parameters. The idea is to assess the effect of a predictor at
a specific configuration of the other predictors. This is relevant in the
case of interactions or non-linear relationships, when the effect of a
predictor variable changes depending on the other predictors. Moreover,
these effects can also be "averaged" over other predictors, to get for
instance the "general trend" of a predictor over different factor levels.
}

\strong{Example:} Let's imagine the following model \code{lm(y ~ condition * x)} where
\code{condition} is a factor with 3 levels A, B and C and \code{x} a continuous
variable (like age for example). One idea is to see how this model performs,
and compare the actual response y to the one predicted by the model (using
\code{\link[=estimate_expectation]{estimate_expectation()}}). Another idea is evaluate the average mean at each of
the condition's levels (using \code{\link[=estimate_means]{estimate_means()}}), which can be useful to
visualize them. Another possibility is to evaluate the difference between
these levels (using \code{\link[=estimate_contrasts]{estimate_contrasts()}}). Finally, one could also estimate
the effect of x averaged over all conditions, or instead within each
condition (using \code{\link[=estimate_slopes]{estimate_slopes()}}).
}
\section{Predictions and contrasts at meaningful values (data grids)}{


To define representative values for focal predictors (specified in \code{by},
\code{contrast}, and \code{trend}), you can use several methods. These values are
internally generated by \code{insight::get_datagrid()}, so consult its
documentation for more details.
\itemize{
\item You can directly specify values as strings or lists for \code{by}, \code{contrast},
and \code{trend}.
\itemize{
\item For numeric focal predictors, use examples like \code{by = "gear = c(4, 8)"},
\code{by = list(gear = c(4, 8))} or \code{by = "gear = 5:10"}
\item For factor or character predictors, use \code{by = "Species = c('setosa', 'virginica')"}
or \code{by = list(Species = c('setosa', 'virginica'))}
}
\item You can use "shortcuts" within square brackets, such as \code{by = "Sepal.Width = [sd]"}
or \code{by = "Sepal.Width = [fivenum]"}
\item For numeric focal predictors, if no representative values are specified
(i.e., \code{by = "gear"} and \emph{not} \code{by = "gear = c(4, 8)"}), \code{length} and
\code{range} control the number and type of representative values for the focal
predictors:
\itemize{
\item \code{length} determines how many equally spaced values are generated.
\item \code{range} specifies the type of values, like \code{"range"} or \code{"sd"}.
\item \code{length} and \code{range} apply to all numeric focal predictors.
\item If you have multiple numeric predictors, \code{length} and \code{range} can accept
multiple elements, one for each predictor (see 'Examples').
}
\item For integer variables, only values that appear in the data will be included
in the data grid, independent from the \code{length} argument. This behaviour
can be changed by setting \code{protect_integers = FALSE}, which will then treat
integer variables as numerics (and possibly produce fractions).
}

See also \href{https://easystats.github.io/modelbased/articles/visualisation_matrix.html}{this vignette}
for some examples.
}

\examples{
\dontshow{if (all(insight::check_if_installed(c("marginaleffects", "emmeans", "effectsize", "mgcv", "ggplot2", "see"), quietly = TRUE))) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
library(ggplot2)
# Get an idea of the data
ggplot(iris, aes(x = Petal.Length, y = Sepal.Width)) +
  geom_point(aes(color = Species)) +
  geom_smooth(color = "black", se = FALSE) +
  geom_smooth(aes(color = Species), linetype = "dotted", se = FALSE) +
  geom_smooth(aes(color = Species), method = "lm", se = FALSE)

# Model it
model <- lm(Sepal.Width ~ Species * Petal.Length, data = iris)
# Compute the marginal effect of Petal.Length at each level of Species
slopes <- estimate_slopes(model, trend = "Petal.Length", by = "Species")
slopes

\dontrun{
# Plot it
plot(slopes)
standardize(slopes)

model <- mgcv::gam(Sepal.Width ~ s(Petal.Length), data = iris)
slopes <- estimate_slopes(model, by = "Petal.Length", length = 50)
summary(slopes)
plot(slopes)

model <- mgcv::gam(Sepal.Width ~ s(Petal.Length, by = Species), data = iris)
slopes <- estimate_slopes(model,
  trend = "Petal.Length",
  by = c("Petal.Length", "Species"), length = 20
)
summary(slopes)
plot(slopes)

# marginal effects, grouped by Species, at different values of Petal.Length
estimate_slopes(model,
  trend = "Petal.Length",
  by = c("Petal.Length", "Species"), length = 10
)

# marginal effects at different values of Petal.Length
estimate_slopes(model, trend = "Petal.Length", by = "Petal.Length", length = 10)

# marginal effects at very specific values of Petal.Length
estimate_slopes(model, trend = "Petal.Length", by = "Petal.Length=c(1, 3, 5)")

# average marginal effects of Petal.Length,
# just for the trend within a certain range
estimate_slopes(model, trend = "Petal.Length=seq(2, 4, 0.01)")
}
\dontshow{\}) # examplesIf}
\dontshow{if (all(insight::check_if_installed(c("marginaleffects", "emmeans"), quietly = TRUE)) && getRversion() >= "4.5.0") (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
\dontrun{
# marginal effects with different `estimate` options
data(penguins)
penguins$long_bill <- factor(datawizard::categorize(penguins$bill_len), labels = c("short", "long"))
m <- glm(long_bill ~ sex + species + island * bill_dep, data = penguins, family = "binomial")

# the emmeans default
estimate_slopes(m, "bill_dep", by = "island")
emmeans::emtrends(m, "island", var = "bill_dep", regrid = "response")

# the marginaleffects default
estimate_slopes(m, "bill_dep", by = "island", estimate = "average")
marginaleffects::avg_slopes(m, variables = "bill_dep", by = "island")
}
\dontshow{\}) # examplesIf}
}
\references{
Montiel Olea, J. L., and Plagborg-Møller, M. (2019). Simultaneous
confidence bands: Theory, implementation, and an application to SVARs.
Journal of Applied Econometrics, 34(1), 1–17. \doi{10.1002/jae.2656}
}
