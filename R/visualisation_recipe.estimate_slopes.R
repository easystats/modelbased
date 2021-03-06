#' @rdname visualisation_recipe.estimate_predicted
#'
#' @examples
#' # ==============================================
#' # estimate_slopes
#' # ==============================================
#' if (require("ggplot2")) {
#'   model <- lm(Sepal.Width ~ Species * Petal.Length, data = iris)
#'   x <- estimate_slopes(model, trend = "Petal.Length", levels = "Species")
#'
#'   layers <- visualisation_recipe(x)
#'   layers
#'   plot(layers)
#'
#'   model <- lm(Petal.Length ~ poly(Sepal.Width, 4), data = iris)
#'   x <- estimate_slopes(model, modulate = "Sepal.Width", length = 20)
#'   plot(visualisation_recipe(x))
#'
#'   model <- lm(Petal.Length ~ Species * poly(Sepal.Width, 3), data = iris)
#'   x <- estimate_slopes(model, modulate = "Sepal.Width", levels = "Species")
#'   plot(visualisation_recipe(x))
#' }
#' @export
visualisation_recipe.estimate_slopes <- function(x,
                                                 hline = NULL,
                                                 line = NULL,
                                                 pointrange = NULL,
                                                 ribbon = NULL,
                                                 labs = NULL,
                                                 ...) {
  info <- attributes(x)
  layers <- list()

  # Main aesthetics -----------------
  data <- as.data.frame(x)
  data$Confidence <- .estimate_slopes_sig(x, ...)
  data$.group = {function(x) rep(seq_along(x$lengths), x$length)}(rle(data$Confidence))

  y <- info$trend
  color <- "Confidence"
  fill <- NULL
  group <- NULL
  alpha <- NULL

  if (is.null(info$modulate)) {
    x1 <- info$levels[1]
    if (length(info$levels) > 1) {
      warning("Cannot deal with more than 2 'levels' variables for now. Other ones will be omitted.")
    }
  } else {
    x1 <- info$modulate[1]
    if (length(info$modulate) > 1) {
      warning("Cannot deal with more than 2 'modulate' variables for now. Other ones will be omitted.")
    }
    if (!is.null(info$levels)) {
      color <- info$levels[1]
      fill <- info$levels[1]
      group_ribbon <- info$levels[1]
      group_line <- info$levels[1]
      alpha <- "Confidence"
    } else {
      group_ribbon <- ".group"
      group_line <- 1
      fill <- "Confidence"
      color <- NULL
    }
  }


  # Layers -----------------------
  l <- 1

  # Horizontal Line
  layers[[paste0("l", l)]] <- .visualisation_grouplevel_hline(data, x1, hline = hline)
  l <- l + 1

  # Line + Point-range style
  if (!is.null(info$levels[1]) && x1 == info$levels[1]) {
    layers[[paste0("l", l)]] <- .visualisation_means_line(data, x1, y = "Coefficient", color = NULL, alpha = NULL, line = line)
    l <- l + 1
    layers[[paste0("l", l)]] <- .visualisation_means_pointrange(data, x1, y = "Coefficient", color = color, alpha = NULL, pointrange = pointrange)
    l <- l + 1

    # Ribbon + line style
  } else if (x1 == info$modulate[1]) {
    layers[[paste0("l", l)]] <- .visualisation_predicted_ribbon(data, x1, y = "Coefficient", fill = fill, ribbon = ribbon, group = group_ribbon)
    l <- l + 1
    layers[[paste0("l", l)]] <- .visualisation_slopes_line(data, x1, color, group_line, alpha, line = line)
    l <- l + 1
  }


  # Labs
  layers[[paste0("l", l)]] <- .visualisation_slopes_labs(info, color, labs = labs)

  # Out
  class(layers) <- c("visualisation_recipe", class(layers))
  attr(layers, "data") <- data
  layers
}


# Layer - Lines -------------------------------------------------------------


.visualisation_slopes_line <- function(data, x1, color, group, alpha, line = NULL) {
  out <- list(
    data = data,
    geom = "line",
    aes = list(
      y = "Coefficient",
      x = x1,
      color = color,
      group = group,
      alpha = alpha
    )
  )
  if (!is.null(line)) out <- utils::modifyList(out, line) # Update with additional args
  out
}


# Layer - Labels --------------------------------------------------------------

.visualisation_slopes_labs <- function(info, color = NULL, labs = NULL) {
  out <- list(
    geom = "labs",
    y = paste0("Effect of ", info$trend),
    color = color,
    title = paste0(
      "Estimated Coefficients (",
      format(insight::find_formula(info$model)),
      ")"
    )
  )

  if (!is.null(labs)) out <- utils::modifyList(out, labs) # Update with additional args
  out
}
