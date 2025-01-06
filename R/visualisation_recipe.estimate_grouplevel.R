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
#' \donttest{
#' # 2 random intercepts
#' model <- lme4::lmer(Reaction ~ Days + (1 | Subject) + (1 | Newfactor), data = data)
#' x <- estimate_grouplevel(model)
#' plot(visualisation_recipe(x))
#'
#' model <- lme4::lmer(Reaction ~ Days + (1 + Days | Subject) + (1 | Newfactor), data = data)
#' x <- estimate_grouplevel(model)
#' plot(visualisation_recipe(x))
#' }
#' @export
visualisation_recipe.estimate_grouplevel <- function(x,
                                                     hline = NULL,
                                                     pointrange = NULL,
                                                     facet_wrap = NULL,
                                                     labs = NULL,
                                                     ...) {
  my_data <- as.data.frame(x)
  # Fix order so that it's plotted with sorted levels
  my_data$Level <- factor(my_data$Level, levels = sort(.to_numeric(unique(my_data$Level))))

  layers <- list()

  # Main aesthetics -----------------
  color <- NULL
  if (length(unique(my_data$Group)) > 1) color <- "Group"
  y <- "Coefficient"
  x1 <- "Level"

  # Layers -----------------------
  l <- 1
  # Horizontal Line
  layers[[paste0("l", l)]] <- .visualisation_grouplevel_hline(
    data = my_data,
    x1,
    hline = hline
  )
  l <- l + 1
  # Point range
  layers[[paste0("l", l)]] <- .visualisation_grouplevel_pointrange(
    data = my_data,
    x1,
    color,
    pointrange = pointrange
  )
  l <- l + 1
  # Flip coord
  layers[[paste0("l", l)]] <- list(geom = "coord_flip")
  l <- l + 1
  # Facet wrap
  if (length(unique(my_data$Parameter)) > 1) {
    layers[[paste0("l", l)]] <- .visualisation_grouplevel_facet_wrap(facet_wrap = facet_wrap)
    l <- l + 1
  }
  # Labs
  layers[[paste0("l", l)]] <- .visualisation_grouplevel_labs(labs = labs)

  # Out
  class(layers) <- unique(c("visualisation_recipe", "see_visualisation_recipe", class(layers)))
  attr(layers, "data") <- my_data
  layers
}

# Layer - hline -------------------------------------------------------

.visualisation_grouplevel_hline <- function(data, x1, hline = NULL) {
  out <- list(
    geom = "hline",
    data = data,
    yintercept = 0,
    linetype = "dashed"
  )

  if (!is.null(hline)) out <- utils::modifyList(out, hline) # Update with additional args
  out
}


# Layer - Pointrange -------------------------------------------------------

.visualisation_grouplevel_pointrange <- function(data, x1, color, pointrange = NULL) {
  out <- list(
    geom = "pointrange",
    data = data,
    aes = list(
      y = "Coefficient",
      x = x1,
      ymin = "CI_low",
      ymax = "CI_high",
      color = color
    )
  )

  if (!is.null(pointrange)) out <- utils::modifyList(out, pointrange) # Update with additional args
  out
}

# Layer - facet_wrap -------------------------------------------------------

.visualisation_grouplevel_facet_wrap <- function(facet_wrap = NULL) {
  out <- list(
    geom = "facet_wrap",
    facets = "~ Parameter",
    scales = "free_x"
  )

  if (!is.null(facet_wrap)) out <- utils::modifyList(out, facet_wrap) # Update with additional args
  out
}


# Layer - Labels --------------------------------------------------------------

.visualisation_grouplevel_labs <- function(labs = NULL) {
  out <- list(geom = "labs", title = "Group-level Scores")
  if (!is.null(labs)) out <- utils::modifyList(out, labs) # Update with additional args
  out
}
