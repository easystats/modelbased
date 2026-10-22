test_that("visualization_recipe", {
  skip_if_not_installed("emmeans")
  skip_if_not_installed("marginaleffects", minimum_version = "0.29.0")

  data <- iris
  data$fac <- rep_len(c("A", "B"), 150)
  data$fac2 <- rep_len(c("X", "X", "X", "Y", "Y", "Y"), 150)
  model <- lm(Sepal.Length ~ Species * fac * Sepal.Width * fac2, data = data)

  # Estimate means -------------------------------------
  x <- estimate_means(model, by = "Species")
  # plot(modelbased:::.visualization_recipe(x))
  aes <- modelbased:::.find_aes(x)$aes
  expect_identical(aes$y, "Mean")
  expect_identical(aes$x, "Species")

  x <- estimate_means(model, by = "Sepal.Width")
  # plot(modelbased:::.visualization_recipe(x))
  aes <- modelbased:::.find_aes(x)$aes
  expect_identical(aes$y, "Mean")
  expect_identical(aes$x, "Sepal.Width")

  x <- estimate_means(model, by = c("Sepal.Width", "Species"))
  # plot(modelbased:::.visualization_recipe(x))
  aes <- modelbased:::.find_aes(x)$aes
  expect_identical(aes$y, "Mean")
  expect_identical(aes$x, "Sepal.Width")

  x <- estimate_means(model, by = c("Species", "Sepal.Width"))
  # plot(modelbased:::.visualization_recipe(x))
  aes <- modelbased:::.find_aes(x)$aes
  expect_identical(aes$y, "Mean")
  expect_identical(aes$x, "Species")

  x <- estimate_means(model, by = c("Species", "fac", "Sepal.Width"))
  # plot(modelbased:::.visualization_recipe(x))
  aes <- modelbased:::.find_aes(x)$aes
  expect_identical(aes$y, "Mean")
  expect_identical(aes$x, "Species")
  expect_identical(aes$color, "fac")

  x <- estimate_means(model, by = c("Species", "Sepal.Width", "fac"))
  # plot(modelbased:::.visualization_recipe(x))
  aes <- modelbased:::.find_aes(x)$aes
  expect_identical(aes$y, "Mean")
  expect_identical(aes$x, "Species")
  expect_identical(aes$color, "Sepal.Width")

  # works with glm-Poisson
  set.seed(123)
  dat <- data.frame(y = rpois(100, 3), fa = gl(4, 20, 100))
  dat_glm <- glm(y ~ fa, data = dat, family = poisson(link = "log"))
  x <- estimate_means(dat_glm, "fa", backend = "emmeans")
  vr <- visualisation_recipe(x, show_data = TRUE, join_dots = TRUE)
  expect_identical(vr$l1$aes, list(y = "y", x = "fa", color = NULL, alpha = NULL))
  expect_identical(
    vr$l2$aes,
    list(y = "Rate", x = "fa", color = NULL, alpha = NULL, group = ".group")
  )
  expect_identical(
    vr$l3$aes,
    list(
      y = "Rate",
      x = "fa",
      ymin = "CI_low",
      ymax = "CI_high",
      color = NULL,
      group = ".group",
      alpha = NULL
    )
  )
  vr <- visualisation_recipe(x, show_data = TRUE, join_dots = FALSE)
  expect_identical(vr$l1$aes, list(y = "y", x = "fa", color = NULL, alpha = NULL))
  expect_identical(
    vr$l2$aes,
    list(
      y = "Rate",
      x = "fa",
      ymin = "CI_low",
      ymax = "CI_high",
      color = NULL,
      group = ".group",
      alpha = NULL
    )
  )
  expect_null(vr$l3$aes)
  x <- estimate_means(dat_glm, "fa", backend = "marginaleffects")
  vr <- visualisation_recipe(x, show_data = TRUE, join_dots = TRUE)
  expect_identical(vr$l1$aes, list(y = "y", x = "fa", color = NULL, alpha = NULL))
  expect_identical(
    vr$l2$aes,
    list(y = "Mean", x = "fa", color = NULL, alpha = NULL, group = ".group")
  )
  expect_identical(
    vr$l3$aes,
    list(
      y = "Mean",
      x = "fa",
      ymin = "CI_low",
      ymax = "CI_high",
      color = NULL,
      group = ".group",
      alpha = NULL
    )
  )
  vr <- visualisation_recipe(x, show_data = FALSE, join_dots = TRUE)
  expect_identical(
    vr$l1$aes,
    list(y = "Mean", x = "fa", color = NULL, alpha = NULL, group = ".group")
  )
  expect_identical(
    vr$l2$aes,
    list(
      y = "Mean",
      x = "fa",
      ymin = "CI_low",
      ymax = "CI_high",
      color = NULL,
      group = ".group",
      alpha = NULL
    )
  )

  # Estimate predictions ---------------------------
  x <- estimate_relation(model, by = "Sepal.Width")
  # plot(modelbased:::.visualization_recipe(x))
  aes <- modelbased:::.find_aes(x)$aes
  expect_identical(aes$y, "Predicted")
  expect_identical(aes$x, "Sepal.Width")
  expect_null(aes$color)

  x <- estimate_relation(model, by = "Species")
  # plot(modelbased:::.visualization_recipe(x))
  aes <- modelbased:::.find_aes(x)$aes
  expect_identical(aes$y, "Predicted")
  expect_identical(aes$x, "Species")
  expect_null(aes$color)

  x <- estimate_relation(model, by = c("Sepal.Width", "Species"))
  # plot(modelbased:::.visualization_recipe(x))
  aes <- modelbased:::.find_aes(x)$aes
  expect_identical(aes$y, "Predicted")
  expect_identical(aes$x, "Sepal.Width")
  expect_identical(aes$color, "Species")

  x <- estimate_relation(model, by = c("Species", "Sepal.Width"))
  # plot(modelbased:::.visualization_recipe(x))
  aes <- modelbased:::.find_aes(x)$aes
  expect_identical(aes$y, "Predicted")
  expect_identical(aes$x, "Species")
  expect_identical(aes$color, "Sepal.Width")

  x <- estimate_relation(model, by = c("Species", "fac", "Sepal.Width"))
  # plot(modelbased:::.visualization_recipe(x))
  aes <- modelbased:::.find_aes(x)$aes
  expect_identical(aes$y, "Predicted")
  expect_identical(aes$x, "Species")
  expect_identical(aes$color, "fac")
  expect_identical(aes$alpha, "Sepal.Width")

  x <- estimate_relation(model, by = c("Species", "Sepal.Width", "fac"))
  # plot(modelbased:::.visualization_recipe(x))
  aes <- modelbased:::.find_aes(x)$aes
  expect_identical(aes$y, "Predicted")
  expect_identical(aes$x, "Species")
  expect_identical(aes$color, "Sepal.Width")
  expect_identical(deparse(aes$facet), "~fac")

  x <- lm(Sepal.Length ~ Petal.Width * Species * Sepal.Width, data = iris) |>
    estimate_relation(by = c("Petal.Width", "Species", "Sepal.Width"))
  # plot(modelbased:::.visualization_recipe(x))
  aes <- modelbased:::.find_aes(x)$aes
  expect_identical(aes$y, "Predicted")
  expect_identical(aes$x, "Petal.Width")
  expect_identical(aes$color, "Species")
  expect_identical(aes$alpha, "Sepal.Width")

  x <- estimate_relation(model, by = c("Species", "Sepal.Width", "fac", "fac2"))
  # plot(modelbased:::.visualization_recipe(x))
  aes <- modelbased:::.find_aes(x)$aes
  expect_identical(aes$y, "Predicted")
  expect_identical(aes$x, "Species")
  expect_identical(aes$color, "Sepal.Width")
  expect_identical(deparse(aes$grid), "fac2 ~ fac")
  expect_null(aes$facet)

  # Estimate slopes --------------------------------
  x <- estimate_slopes(model, trend = "Sepal.Width")
  expect_error(modelbased:::.find_aes(x)$aes)

  x <- estimate_slopes(model, trend = "Sepal.Width", by = "Species")
  # plot(modelbased:::.visualization_recipe(x))
  aes <- modelbased:::.find_aes(x)$aes
  expect_identical(aes$y, "Slope")
  expect_identical(aes$x, "Species")
  expect_null(aes$color)

  x <- estimate_slopes(
    model,
    trend = "Species",
    by = "Sepal.Width",
    backend = "marginaleffects"
  )
  aes <- modelbased:::.find_aes(x)$aes
  expect_identical(aes$y, "Slope")
  expect_identical(aes$x, "Sepal.Width")
  expect_identical(aes$color, "Comparison")
  # plot(modelbased:::.visualization_recipe(x))
})


test_that("visualization_recipe, grid argument modifies facet_grid layer", {
  skip_if_not_installed("emmeans")
  skip_if_not_installed("marginaleffects", minimum_version = "0.29.0")

  data <- iris
  data$fac <- rep_len(c("A", "B"), 150)
  data$fac2 <- rep_len(c("X", "X", "X", "Y", "Y", "Y"), 150)
  model <- lm(Sepal.Length ~ Species * fac * Sepal.Width * fac2, data = data)
  x <- estimate_relation(model, by = c("Species", "Sepal.Width", "fac", "fac2"))

  .grid_layer <- function(r) {
    geoms <- vapply(r, function(l) l$geom, character(1))
    r[[which(geoms == "facet_grid")]]
  }

  # grid argument is used for the facet_grid layer
  out <- .grid_layer(visualisation_recipe(x, grid = list(scales = "free")))
  expect_identical(out$scales, "free")
  expect_identical(deparse(out$rows), "fac2 ~ fac")

  # facet argument does not change the facet_grid layer
  out <- .grid_layer(visualisation_recipe(
    x,
    grid = list(scales = "free"),
    facet = list(scales = "free_y")
  ))
  expect_identical(out$scales, "free")
})


# emmeans backend: ordinal and other models ----------------------------------

.recipe_mapping <- function(x, ...) {
  r <- visualisation_recipe(x, ...)
  geoms <- vapply(r, function(l) l$geom, character(1))
  main <- r[[which(geoms %in% c("pointrange", "line"))[1]]]
  facet <- r[geoms == "facet_wrap"]
  list(
    geoms = unname(geoms),
    x = main$aes$x,
    color = main$aes$color,
    facet = if (length(facet)) deparse(facet[[1]]$facets) else NULL,
    data = attributes(r)$data
  )
}


test_that("visualization_recipe, emmeans ordinal, color by Response", {
  skip_if_not_installed("emmeans")
  skip_if_not_installed("MASS")

  data(housing, package = "MASS")
  m <- MASS::polr(Sat ~ Infl + Type, weights = Freq, data = housing, Hess = TRUE)
  x <- estimate_means(m, by = "Infl", backend = "emmeans")
  out <- .recipe_mapping(x)
  expect_identical(out$x, "Infl")
  expect_identical(out$color, "Response")
  expect_true("scale_y_continuous" %in% out$geoms)
  expect_identical(attributes(x)$focal_terms, "Infl")

  x <- estimate_means(m, by = c("Infl", "Type"), backend = "emmeans")
  out <- .recipe_mapping(x)
  expect_identical(out$color, "Type")
  expect_identical(out$facet, "~Response")

  # printed rows keep their order: the response varies within predictor levels,
  # and the printed table does not depend on the model information
  printed <- lapply(as.list(format(x)), as.character)
  expect_identical(printed$Response[1:6], rep(c("Low", "Medium", "High"), 2))
  attr(x, "model_info") <- NULL
  expect_identical(lapply(as.list(format(x)), as.character), printed)
})


test_that("visualization_recipe, emmeans ordinal, color by Threshold", {
  skip_if_not_installed("emmeans")
  skip_if_not_installed("MASS")

  data(housing, package = "MASS")
  m <- MASS::polr(Sat ~ Infl + Type, weights = Freq, data = housing, Hess = TRUE)
  out <- .recipe_mapping(estimate_means(
    m,
    by = "Infl",
    backend = "emmeans",
    predict = "cum.prob"
  ))
  expect_identical(out$color, "Threshold")
  expect_true("scale_y_continuous" %in% out$geoms)
  out <- .recipe_mapping(estimate_means(
    m,
    by = c("Infl", "Type"),
    backend = "emmeans",
    predict = "linear.predictor"
  ))
  expect_identical(out$color, "Type")
  expect_identical(out$facet, "~Threshold")
  expect_false("scale_y_continuous" %in% out$geoms)
  # no grouping column for the latent scale
  out <- .recipe_mapping(estimate_means(
    m,
    by = "Infl",
    backend = "emmeans",
    predict = "latent"
  ))
  expect_null(out$color)

  # thresholds in model order, not in alphabetical order
  data(wine, package = "ordinal")
  rating_levels <- c("very low", "low", "medium", "high", "very high")
  wine$rating <- factor(
    rating_levels[as.integer(wine$rating)],
    levels = rating_levels,
    ordered = TRUE
  )
  m <- MASS::polr(rating ~ temp + contact, data = wine, Hess = TRUE)
  x <- estimate_means(m, by = "temp", backend = "emmeans", predict = "cum.prob")
  out <- .recipe_mapping(x)
  expect_identical(
    levels(out$data$Threshold),
    c("very low|low", "low|medium", "medium|high", "high|very high")
  )
})


test_that("visualization_recipe, ordinal, focal predictor named Threshold", {
  skip_if_not_installed("emmeans")
  skip_if_not_installed("MASS")

  data(housing, package = "MASS")
  d <- housing
  d$Threshold <- d$Infl
  m <- MASS::polr(Sat ~ Threshold + Type, weights = Freq, data = d, Hess = TRUE)
  out <- .recipe_mapping(estimate_means(m, by = "Threshold", backend = "emmeans"))
  expect_identical(out$x, "Threshold")
  expect_identical(out$color, "Response")
})


test_that("visualization_recipe, emmeans, percent scale for logistic models", {
  skip_if_not_installed("emmeans")

  d <- mtcars
  d$cyl <- factor(d$cyl)
  m <- glm(am ~ cyl, family = binomial, data = d)
  out <- .recipe_mapping(estimate_means(m, by = "cyl", backend = "emmeans"))
  expect_identical(out$geoms, c("pointrange", "labs", "scale_y_continuous"))
})


test_that("visualization_recipe, informative error when nothing goes on x-axis", {
  # fails on R 4.5.1
  # Error in `match(x, table, nomatch = 0L)`: 'match' requires vector arguments
  #   Backtrace:
  #        ▆
  #     1. ├─testthat::expect_error(...) at test-visualisation_recipe.R:496:3
  #     2. │ └─testthat:::expect_condition_matching_(...)
  #     3. │   └─testthat:::quasi_capture(...)
  #     4. │     ├─testthat (local) .capture(...)
  #     5. │     │ └─base::withCallingHandlers(...)
  #     6. │     └─rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
  #     7. ├─datawizard::visualisation_recipe(...)
  #     8. └─modelbased:::visualisation_recipe.estimate_means(...)
  #     9.   └─modelbased:::.visualization_recipe(...)
  #    10.     └─modelbased:::.find_aes(x, model_info, numeric_as_discrete)
  #    11.       ├─by[by %in% colnames(data)]
  #    12.       ├─stats:::`[.formula`(by, by %in% colnames(data))
  #    13.       ├─base::NextMethod("[")
  #    14.       └─by %in% colnames(data)

  skip_on_cran()
  # intercept-only model
  model <- lm(qsec ~ 1, data = mtcars)
  expect_error(visualisation_recipe(estimate_prediction(model)), regex = "intercept-only")
  expect_error(visualisation_recipe(estimate_relation(model)), regex = "intercept-only")
  # no `by` variable
  model <- lm(qsec ~ wt, data = mtcars)
  expect_error(
    visualisation_recipe(estimate_prediction(model)),
    regex = "Use the `by` argument"
  )
  x <- estimate_prediction(model, by = "wt")
  expect_s3_class(visualisation_recipe(x), "visualisation_recipe")
  # emmeans backend returns `~1` as focal term
  skip_if_not_installed("emmeans")
  model <- lm(qsec ~ 1, data = mtcars)
  expect_error(
    visualisation_recipe(estimate_means(model, by = NULL, backend = "emmeans")),
    regex = "intercept-only"
  )
})


test_that("visualization_recipe, emmeans ordinal slopes, color by Response", {
  skip_if_not_installed("emmeans")
  skip_if_not_installed("MASS")

  data(housing, package = "MASS")
  set.seed(1)
  housing$x <- as.numeric(housing$Cont) + stats::rnorm(nrow(housing))
  m <- MASS::polr(Sat ~ Infl * x, weights = Freq, data = housing, Hess = TRUE)
  out <- .recipe_mapping(estimate_slopes(m, "x", by = "Infl", backend = "emmeans"))
  expect_identical(out$x, "Infl")
  expect_identical(out$color, "Response")
  out <- .recipe_mapping(estimate_slopes(
    m,
    "x",
    by = "Infl",
    predict = "cum.prob",
    backend = "emmeans"
  ))
  expect_identical(out$color, "Threshold")
})
