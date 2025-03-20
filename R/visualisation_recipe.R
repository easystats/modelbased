#' Automated plotting for 'modelbased' objects
#'
#' Most 'modelbased' objects can be visualized using the `plot()` function, which
#' internally calls the `visualisation_recipe()` function. See the **examples**
#' below for more information and examples on how to create and customize plots.
#'
#' The plotting works by mapping any predictors from the `by` argument to the x-axis,
#' colors, alpha (transparency) and facets. Thus, the appearance of the plot depends
#' on the order of the variables that you specify in the `by` argument. For instance,
#' the plots corresponding to `estimate_relation(model, by=c("Species", "Sepal.Length"))`
#' and `estimate_relation(model, by=c("Sepal.Length", "Species"))` will look different.
#'
#' The automated plotting is primarily meant for convenient visual checks, but
#' for publication-ready figures, we recommend re-creating the figures using the
#' `ggplot2` package directly.
#'
#' @param x A modelbased object.
#' @param show_data Logical, if `TRUE`, display the "raw" data as a background
#'   to the model-based estimation.
#' @param join_dots Logical, if `TRUE` (default) and for categorical focal terms
#' in `by`, dots (estimates) are connected by lines, i.e. plots will be a
#' combination of dots with error bars and connecting lines. If `FALSE`, only
#' dots and error bars are shown. It is possible to set a global default value
#' using `options()`, e.g. `options(modelbased_join_dots = FALSE)`.
#' @param numeric_as_discrete Maximum number of unique values in a numeric
#' predictor to treat that predictor as discrete. Defaults to `8`. Numeric
#' predictors are usually mapped to a continuous color scale, unless they have
#' only few unique values. In the latter case, numeric predictors are assumed to
#' represent "categories", e.g. when only the mean value and +/- 1 standard
#' deviation around the mean are chosen as representative values for that
#' predictor. Use `FALSE` to always use continuous color scales for numeric
#' predictors. It is possible to set a global default value using `options()`,
#' e.g. `options(modelbased_numeric_as_discrete = 10)`.
#' @param point,line,pointrange,ribbon,facet,grid Additional
#' aesthetics and parameters for the geoms (see customization example).
#' @param ... Not used.
#'
#' @details There are two options to remove the confidence bands or errors bars
#' from the plot. To remove error bars, simply set the `pointrange` geom to
#' `point`, e.g. `plot(..., pointrange = list(geom = "point"))`. To remove the
#' confidence bands from line geoms, use `ribbon = "none"`.
#'
#' @section Global Options to Customize Plots:
#' Some arguments for `plot()` can get global defaults using `options()`:
#'
#' - `modelbased_join_dots`: `options(modelbased_join_dots = <logical>)` will
#'   set a default value for the `join_dots`.
#'
#' - `modelbased_numeric_as_discrete`: `options(modelbased_numeric_as_discrete = <number>)`
#'   will set a default value for the `modelbased_numeric_as_discrete` argument.
#'   Can also be `FALSE`.
#'
#' @examplesIf all(insight::check_if_installed(c("marginaleffects", "see", "ggplot2"), quietly = TRUE)) && getRversion() >= "4.1.0"
#' library(ggplot2)
#' library(see)
#' # ==============================================
#' # estimate_relation, estimate_expectation, ...
#' # ==============================================
#' # Simple Model ---------------
#' x <- estimate_relation(lm(mpg ~ wt, data = mtcars))
#' layers <- visualisation_recipe(x)
#' layers
#' plot(layers)
#'
#' # visualization_recipe() is called implicitly when you call plot()
#' plot(estimate_relation(lm(mpg ~ qsec, data = mtcars)))
#'
#' \dontrun{
#' # And can be used in a pipe workflow
#' lm(mpg ~ qsec, data = mtcars) |>
#'   estimate_relation(ci = c(0.5, 0.8, 0.9)) |>
#'   plot()
#'
#' # Customize aesthetics ----------
#'
#' plot(x,
#'   point = list(color = "red", alpha = 0.6, size = 3),
#'   line = list(color = "blue", size = 3),
#'   ribbon = list(fill = "green", alpha = 0.7)
#' ) +
#'   theme_minimal() +
#'   labs(title = "Relationship between MPG and WT")
#'
#' # Customize raw data -------------
#'
#' plot(x, point = list(geom = "density_2d_filled"), line = list(color = "white")) +
#'   scale_x_continuous(expand = c(0, 0)) +
#'   scale_y_continuous(expand = c(0, 0)) +
#'   theme(legend.position = "none")
#'
#' # Single predictors examples -----------
#'
#' plot(estimate_relation(lm(Sepal.Length ~ Species, data = iris)))
#'
#' # 2-ways interaction ------------
#'
#' # Numeric * numeric
#' x <- estimate_relation(lm(mpg ~ wt * qsec, data = mtcars))
#' plot(x)
#'
#' # Numeric * factor
#' x <- estimate_relation(lm(Sepal.Width ~ Sepal.Length * Species, data = iris))
#' plot(x)
#'
#' # ==============================================
#' # estimate_means
#' # ==============================================
#' # Simple Model ---------------
#' x <- estimate_means(lm(Sepal.Width ~ Species, data = iris), by = "Species")
#' layers <- visualisation_recipe(x)
#' layers
#' plot(layers)
#'
#' # Customize aesthetics
#' layers <- visualisation_recipe(x,
#'   point = list(width = 0.03, color = "red"),
#'   pointrange = list(size = 2, linewidth = 2),
#'   line = list(linetype = "dashed", color = "blue")
#' )
#' plot(layers)
#'
#' # Two levels ---------------
#' data <- mtcars
#' data$cyl <- as.factor(data$cyl)
#'
#' model <- lm(mpg ~ cyl * wt, data = data)
#'
#' x <- estimate_means(model, by = c("cyl", "wt"))
#' plot(x)
#'
#'
#' # GLMs ---------------------
#' data <- data.frame(vs = mtcars$vs, cyl = as.factor(mtcars$cyl))
#' x <- estimate_means(glm(vs ~ cyl, data = data, family = "binomial"), by = c("cyl"))
#' plot(x)
#' }
#' @export
visualisation_recipe.estimate_predicted <- function(x,
                                                    show_data = FALSE,
                                                    show_residuals = FALSE,
                                                    point = NULL,
                                                    line = NULL,
                                                    pointrange = NULL,
                                                    ribbon = NULL,
                                                    facet = NULL,
                                                    grid = NULL,
                                                    join_dots = NULL,
                                                    numeric_as_discrete = NULL,
                                                    ...) {
  # Process argument ---------------------------------------------------------
  # --------------------------------------------------------------------------

  # set defaults
  if (is.null(join_dots)) {
    join_dots <- getOption("modelbased_join_dots", TRUE)
  }
  if (is.null(numeric_as_discrete)) {
    numeric_as_discrete <- getOption("modelbased_numeric_as_discrete", 8)
  }

  .visualization_recipe(
    x,
    show_data = show_data,
    show_residuals = show_residuals,
    point = point,
    line = line,
    pointrange = pointrange,
    ribbon = ribbon,
    facet = facet,
    grid = grid,
    join_dots = join_dots,
    numeric_as_discrete = numeric_as_discrete,
    ...
  )
}


#' @export
visualisation_recipe.estimate_means <- visualisation_recipe.estimate_predicted


#' @rdname visualisation_recipe.estimate_predicted
#'
#' @examplesIf all(insight::check_if_installed(c("marginaleffects", "see", "ggplot2"), quietly = TRUE))
#' # ==============================================
#' # estimate_slopes
#' # ==============================================
#' model <- lm(Sepal.Width ~ Species * Petal.Length, data = iris)
#' x <- estimate_slopes(model, trend = "Petal.Length", by = "Species")
#'
#' layers <- visualisation_recipe(x)
#' layers
#' plot(layers)
#'
#' \dontrun{
#' # Customize aesthetics and add horizontal line and theme
#' layers <- visualisation_recipe(x, pointrange = list(size = 2, linewidth = 2))
#' plot(layers) +
#'   geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
#'   theme_minimal() +
#'   labs(y = "Effect of Petal.Length", title = "Marginal Effects")
#'
#' model <- lm(Petal.Length ~ poly(Sepal.Width, 4), data = iris)
#' x <- estimate_slopes(model, trend = "Sepal.Width", by = "Sepal.Width", length = 20)
#' plot(visualisation_recipe(x))
#'
#' model <- lm(Petal.Length ~ Species * poly(Sepal.Width, 3), data = iris)
#' x <- estimate_slopes(model, trend = "Sepal.Width", by = c("Sepal.Width", "Species"))
#' plot(visualisation_recipe(x))
#' }
#' @export
visualisation_recipe.estimate_slopes <- function(x,
                                                 line = NULL,
                                                 pointrange = NULL,
                                                 ribbon = NULL,
                                                 facet = NULL,
                                                 grid = NULL,
                                                 ...) {
  .visualization_recipe(
    x,
    show_data = FALSE,
    line = line,
    pointrange = pointrange,
    ribbon = ribbon,
    facet = facet,
    grid = grid,
    ...
  )
}


#' @rdname visualisation_recipe.estimate_predicted
#'
#' @examplesIf all(insight::check_if_installed(c("ggplot2", "marginaleffects", "see", "lme4"), quietly = TRUE))
#' # ==============================================
#' # estimate_grouplevel
#' # ==============================================
#' \dontrun{
#' data <- lme4::sleepstudy
#' data <- rbind(data, data)
#' data$Newfactor <- rep(c("A", "B", "C", "D"))
#'
#' # 1 random intercept
#' model <- lme4::lmer(Reaction ~ Days + (1 | Subject), data = data)
#' x <- estimate_grouplevel(model)
#' layers <- visualisation_recipe(x)
#' layers
#' plot(layers)
#'
#' # 2 random intercepts
#' model <- lme4::lmer(Reaction ~ Days + (1 | Subject) + (1 | Newfactor), data = data)
#' x <- estimate_grouplevel(model)
#' plot(x) +
#'   geom_hline(yintercept = 0, linetype = "dashed") +
#'   theme_minimal()
#' # Note: we need to use hline instead of vline because the axes is flipped
#'
#' model <- lme4::lmer(Reaction ~ Days + (1 + Days | Subject) + (1 | Newfactor), data = data)
#' x <- estimate_grouplevel(model)
#' plot(x)
#' }
#' @export
visualisation_recipe.estimate_grouplevel <- function(x,
                                                     line = NULL,
                                                     pointrange = NULL,
                                                     ribbon = NULL,
                                                     facet = NULL,
                                                     grid = NULL,
                                                     ...) {
  if (is.null(facet)) {
    facet <- list(scales = "free")
  } else {
    facet <- utils::modifyList(facet, list(scales = "free"))
  }

  .visualization_recipe(
    x,
    show_data = FALSE,
    line = line,
    pointrange = pointrange,
    ribbon = ribbon,
    facet = facet,
    grid = grid,
    join_dots = FALSE,
    ...
  )
}
