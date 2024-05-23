#' @rdname visualisation_recipe.estimate_predicted
#'
#' @examples
#' # ==============================================
#' # estimate_slopes
#' # ==============================================
#' if (require("ggplot2")) {
#'   model <- lm(Sepal.Width ~ Species * Petal.Length, data = iris)
#'   x <- estimate_slopes(model, trend = "Petal.Length", by ="Species")
#'
#'   layers <- visualisation_recipe(x)
#'   layers
#'   plot(layers)
#'
#'   model <- lm(Petal.Length ~ poly(Sepal.Width, 4), data = iris)
#'   x <- estimate_slopes(model, by ="Sepal.Width", length = 20)
#'   plot(visualisation_recipe(x))
#'
#'   model <- lm(Petal.Length ~ Species * poly(Sepal.Width, 3), data = iris)
#'   x <- estimate_slopes(model, by =c("Sepal.Width", "Species"))
#'   plot(visualisation_recipe(x))
#' }
#' \donttest{
#' # TODO: fails with latest emmeans (1.8.0)
#' if (require("mgcv")) {
#'   data <- iris
#'   data$Petal.Length <- data$Petal.Length^2
#'
#'   model <- mgcv::gam(Sepal.Width ~ t2(Petal.Width, Petal.Length), data = data)
#'   x <- estimate_slopes(model, by =c("Petal.Width", "Petal.Length"), length = 20)
#'   plot(visualisation_recipe(x))
#'
#'   model <- mgcv::gam(Sepal.Width ~ t2(Petal.Width, Petal.Length, by = Species), data = data)
#'   x <- estimate_slopes(model, by =c("Petal.Width", "Petal.Length", "Species"), length = 10)
#'   plot(visualisation_recipe(x))
#' }
#' }
#' @export
visualisation_recipe.estimate_slopes <- function(x,
                                                 hline = NULL,
                                                 line = NULL,
                                                 pointrange = NULL,
                                                 ribbon = NULL,
                                                 labs = NULL,
                                                 facet_wrap = NULL,
                                                 ...) {
  info <- attributes(x)
  layers <- list()

  # Main aesthetics -----------------
  data <- as.data.frame(x)
  data$Confidence <- .estimate_slopes_sig(x, ...)
  data$.group <- {
    function(x) rep(seq_along(x$lengths), x$length)
  }(rle(data$Confidence))

  y <- info$trend
  color <- "Confidence"
  fill <- NULL
  group <- NULL
  alpha <- NULL
  facet <- NULL

  # What are the fact
  facs <- info$at[sapply(data[info$at], function(x) is.factor(x) | is.character(x))]
  nums <- info$at[!info$at %in% facs]
  if (length(facs) > 0 && length(nums) == 0) {
    x1 <- facs[1]
    if (length(facs) > 1) {
      facet <- facs[2]
      if (length(facs) > 2) {
        warning("Cannot deal with more than 2 'factors' variables for now. Keeping the two firsts.", call. = FALSE)
      }
    }
  } else {
    x1 <- nums[1]
    if (length(nums) > 1) {
      alpha <- nums[2]
      group_line <- nums[2]
      if (length(facs) > 0) {
        facet <- facs[1]
      }
    } else if (length(facs) > 0) {
      color <- facs[1]
      fill <- facs[1]
      group_ribbon <- facs[1]
      group_line <- facs[1]
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
  if (length(facs) > 0 && x1 == facs[1]) {
    layers[[paste0("l", l)]] <-
      .visualisation_means_line(
        data,
        x1,
        y = "Coefficient",
        color = NULL,
        alpha = NULL,
        line = line
      )

    l <- l + 1

    layers[[paste0("l", l)]] <- .visualisation_means_pointrange(
      data,
      x1,
      y = "Coefficient",
      color = color,
      alpha = NULL,
      pointrange = pointrange
    )

    l <- l + 1

    # Ribbon + line style
  } else if (x1 == nums[1] && length(nums) == 1) {
    layers[[paste0("l", l)]] <- .visualisation_predicted_ribbon(
      data,
      x1,
      y = "Coefficient",
      fill = fill,
      ribbon = ribbon,
      group = group_ribbon
    )

    l <- l + 1

    layers[[paste0("l", l)]] <- .visualisation_slopes_line(
      data,
      x1,
      color,
      group_line,
      alpha,
      line = line
    )

    l <- l + 1
  } else if (x1 == nums[1] && alpha == nums[2]) {
    layers[[paste0("l", l)]] <- .visualisation_slopes_line(
      data,
      x1,
      color,
      group_line,
      alpha,
      line = line
    )

    l <- l + 1
  }

  # Facet
  if (!is.null(facet)) {
    layers[[paste0("l", l)]] <- list(geom = "facet_wrap", facets = facet)

    if (!is.null(facet_wrap)) {
      layers[[paste0("l", l)]] <-
        utils::modifyList(layers[[paste0("l", l)]], facet_wrap)
    }

    l <- l + 1
  }


  # Labs
  layers[[paste0("l", l)]] <- .visualisation_slopes_labs(info, color, labs = labs)

  # Out
  class(layers) <- unique(c("visualisation_recipe", "see_visualisation_recipe", class(layers)))
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
