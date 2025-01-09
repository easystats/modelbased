#' Visualisation Recipe for 'modelbased' Objects
#'
#' @param x A modelbased object.
#' @param show_data Display the "raw" data as a background to the model-based
#'   estimation.
#' @param point,line,pointrange,ribbon,facet Additional
#' aesthetics and parameters for the geoms (see customization example).
#' @param ... Not used.
#'
#' @examplesIf require("ggplot2") && require("emmeans") && require("see")
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
#' # And can be used in a pipe workflow
#' lm(mpg ~ qsec, data = mtcars) |>
#'   estimate_relation(ci=c(0.5, 0.8, 0.9)) |>
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
#'   labs(title="Relationship between MPG and WT")
#'
#'
#' # Customize raw data -------------
#'
#' plot(x, point = list(geom="density_2d_filled"), line=list(color="white")) +
#'   scale_x_continuous(expand = c(0, 0)) +
#'   scale_y_continuous(expand = c(0, 0)) +
#'   theme(legend.position="none")
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
#'   pointrange = list(size=2, linewidth=2),
#'   line = list(linetype = "dashed", color="blue")
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
#' @export
visualisation_recipe.estimate_predicted <- function(x,
                                                    show_data = TRUE,
                                                    point = NULL,
                                                    line = NULL,
                                                    pointrange = NULL,
                                                    ribbon = NULL,                                                     facet=NULL,
                                                    ...) {
  .visualization_recipe(
    x,
    show_data = show_data,
    point = point,
    line = line,
    pointrange = pointrange,
    ribbon = ribbon,
    facet = facet,
    ...
  )
}


#' @export
visualisation_recipe.estimate_means <- visualisation_recipe.estimate_predicted





#' @rdname visualisation_recipe.estimate_predicted
#'
#' @examplesIf require("ggplot2") && require("emmeans") && require("see")
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
#' # Customize aesthetics and add horizontal line and theme
#' layers <- visualisation_recipe(x, pointrange = list(size=2, linewidth=2))
#' plot(layers) +
#'   geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
#'   theme_minimal() +
#'   labs(y="Effect of Petal.Length", title="Marginal Effects")
#'
#' model <- lm(Petal.Length ~ poly(Sepal.Width, 4), data = iris)
#' x <- estimate_slopes(model, by = "Sepal.Width", length = 20)
#' plot(visualisation_recipe(x))
#'
#' model <- lm(Petal.Length ~ Species * poly(Sepal.Width, 3), data = iris)
#' x <- estimate_slopes(model, by = c("Sepal.Width", "Species"))
#' plot(visualisation_recipe(x))
#' @export
visualisation_recipe.estimate_slopes <- function(x,
                                                 line = NULL,
                                                 pointrange = NULL,
                                                 ribbon = NULL,
                                                 facet = NULL,
                                                 ...) {
  .visualization_recipe(
    x,
    show_data = FALSE,
    line = line,
    pointrange = pointrange,
    ribbon = ribbon,
    facet = facet,
    ...
  )
}



#' @rdname visualisation_recipe.estimate_predicted
#'
#' @examplesIf require("see") && require("lme4") && require("emmeans")
#' # ==============================================
#' # estimate_grouplevel
#' # ==============================================
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
#' plot(x)
#'
#' model <- lme4::lmer(Reaction ~ Days + (1 + Days | Subject) + (1 | Newfactor), data = data)
#' x <- estimate_grouplevel(model)
#' plot(x)
#' @export
visualisation_recipe.estimate_grouplevel <- function(x,
                                                     line = NULL,
                                                     pointrange = NULL,
                                                     ribbon = NULL,
                                                     facet = NULL,
                                                     ...) {
  if (is.null(facet)) {
    facet <- list("scales" = "free")
  } else {
    facet <- utils::modifyList(facet, list("scales" = "free"))
  }

  .visualization_recipe(
    x,
    show_data = FALSE,
    line = line,
    pointrange = pointrange,
    ribbon = ribbon,
    facet = facet,
    ...
  )
}
