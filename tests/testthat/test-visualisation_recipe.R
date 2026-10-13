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

# printed table as a list of character columns
.printed_table <- function(x) {
  lapply(as.list(format(x)), as.character)
}

# the printed table must not depend on the model information
.expect_same_print <- function(x) {
  y <- x
  attr(y, "model_info") <- NULL
  expect_identical(.printed_table(x), .printed_table(y))
}


test_that("visualization_recipe, emmeans ordinal, color by Response", {
  skip_if_not_installed("emmeans")
  skip_if_not_installed("MASS")
  skip_if_not_installed("ordinal")
  skip_if_not_installed("marginaleffects", minimum_version = "0.29.0")

  data(housing, package = "MASS")
  models <- list(
    polr = MASS::polr(Sat ~ Infl + Type, weights = Freq, data = housing, Hess = TRUE),
    clm = ordinal::clm(Sat ~ Infl + Type, weights = Freq, data = housing)
  )
  for (m in models) {
    for (backend in c("emmeans", "marginaleffects")) {
      x <- estimate_means(m, by = "Infl", backend = backend)
      out <- .recipe_mapping(x)
      expect_identical(out$x, "Infl")
      expect_identical(out$color, "Response")
      expect_null(out$facet)
      expect_true("scale_y_continuous" %in% out$geoms)
      expect_identical(attributes(x)$focal_terms, "Infl")
      if (backend == "emmeans") .expect_same_print(x)

      x <- estimate_means(m, by = c("Infl", "Type"), backend = backend)
      out <- .recipe_mapping(x)
      expect_identical(out$x, "Infl")
      expect_identical(out$color, "Type")
      expect_identical(out$facet, "~Response")
      expect_identical(attributes(x)$focal_terms, c("Infl", "Type"))
      if (backend == "emmeans") .expect_same_print(x)
    }
  }

  # printed rows keep their order: the response varies within predictor levels
  x <- estimate_means(models$polr, by = c("Infl", "Type"), backend = "emmeans")
  printed <- .printed_table(x)
  expect_identical(
    printed$Infl[1:6],
    c("Low", "Low", "Low", "Medium", "Medium", "Medium")
  )
  expect_identical(printed$Type[1:6], rep("Tower", 6))
  expect_identical(
    printed$Response[1:6],
    c("Low", "Medium", "High", "Low", "Medium", "High")
  )

  # mixed models from package ordinal
  data(wine, package = "ordinal")
  m <- ordinal::clmm(rating ~ temp + contact + (1 | judge), data = wine)
  x <- estimate_means(m, by = "temp", backend = "emmeans")
  out <- .recipe_mapping(x)
  expect_identical(out$x, "temp")
  expect_identical(out$color, "Response")
  .expect_same_print(x)
})


test_that("visualization_recipe, emmeans ordinal, color by Threshold", {
  skip_if_not_installed("emmeans")
  skip_if_not_installed("MASS")
  skip_if_not_installed("ordinal")

  data(housing, package = "MASS")
  models <- list(
    polr = MASS::polr(Sat ~ Infl + Type, weights = Freq, data = housing, Hess = TRUE),
    clm = ordinal::clm(Sat ~ Infl + Type, weights = Freq, data = housing)
  )
  for (m in models) {
    for (p in c("cum.prob", "exc.prob", "linear.predictor")) {
      x <- estimate_means(m, by = "Infl", backend = "emmeans", predict = p)
      out <- .recipe_mapping(x)
      expect_identical(out$x, "Infl")
      expect_identical(out$color, "Threshold")
      expect_null(out$facet)
      expect_identical(
        "scale_y_continuous" %in% out$geoms,
        p %in% c("cum.prob", "exc.prob")
      )
      expect_identical(attributes(x)$focal_terms, "Infl")
      .expect_same_print(x)

      x <- estimate_means(m, by = c("Infl", "Type"), backend = "emmeans", predict = p)
      out <- .recipe_mapping(x)
      expect_identical(out$x, "Infl")
      expect_identical(out$color, "Type")
      expect_identical(out$facet, "~Threshold")
      .expect_same_print(x)
    }
  }

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
  expect_identical(out$color, "Threshold")
  expect_identical(
    levels(out$data$Threshold),
    c("very low|low", "low|medium", "medium|high", "high|very high")
  )
  expect_identical(
    as.character(unique(out$data$Threshold)),
    c("very low|low", "low|medium", "medium|high", "high|very high")
  )
  .expect_same_print(x)
})


test_that("visualization_recipe, ordinal, focal predictor named Threshold", {
  skip_if_not_installed("emmeans")
  skip_if_not_installed("MASS")
  skip_if_not_installed("marginaleffects", minimum_version = "0.29.0")

  data(housing, package = "MASS")
  d <- housing
  d$Threshold <- d$Infl
  m <- MASS::polr(Sat ~ Threshold + Type, weights = Freq, data = d, Hess = TRUE)
  for (backend in c("emmeans", "marginaleffects")) {
    x <- estimate_means(m, by = "Threshold", backend = backend)
    out <- .recipe_mapping(x)
    expect_identical(out$x, "Threshold")
    expect_identical(out$color, "Response")
    expect_null(out$facet)
  }
})


test_that("visualization_recipe, emmeans, percent scale for logistic models", {
  skip_if_not_installed("emmeans")
  skip_if_not_installed("marginaleffects", minimum_version = "0.29.0")

  d <- mtcars
  d$cyl <- factor(d$cyl)
  m <- glm(am ~ cyl, family = binomial, data = d)
  for (backend in c("emmeans", "marginaleffects")) {
    x <- estimate_means(m, by = "cyl", backend = backend)
    out <- .recipe_mapping(x)
    expect_identical(out$geoms, c("pointrange", "labs", "scale_y_continuous"))
  }
  .expect_same_print(estimate_means(m, by = "cyl", backend = "emmeans"))
})


test_that("visualization_recipe, emmeans, recipes that do not change", {
  skip_if_not_installed("emmeans")
  skip_if_not_installed("MASS")

  m <- lm(Sepal.Length ~ Species, data = iris)
  x <- estimate_means(m, by = "Species", backend = "emmeans")
  out <- .recipe_mapping(x, show_data = TRUE)
  expect_identical(out$geoms, c("jitter", "pointrange", "labs"))
  expect_identical(out$x, "Species")
  expect_null(out$color)
  .expect_same_print(x)

  m <- lm(log(Sepal.Length) ~ Species, data = iris)
  x <- estimate_means(m, by = "Species", backend = "emmeans")
  out <- .recipe_mapping(x, show_data = TRUE)
  expect_identical(out$geoms, c("jitter", "pointrange", "labs"))
  expect_identical(out$x, "Species")
  expect_null(out$color)
  .expect_same_print(x)

  d <- mtcars
  d$cyl <- factor(d$cyl)
  m <- glm(carb ~ cyl, family = poisson, data = d)
  x <- estimate_means(m, by = "cyl", backend = "emmeans")
  out <- .recipe_mapping(x)
  expect_identical(out$geoms, c("pointrange", "labs"))
  expect_identical(out$x, "cyl")
  expect_null(out$color)
  .expect_same_print(x)

  m <- glm(am ~ cyl, family = binomial, data = d)
  x <- estimate_means(m, by = "cyl", backend = "emmeans", predict = "link")
  out <- .recipe_mapping(x)
  expect_identical(out$geoms, c("pointrange", "labs"))
  expect_identical(out$x, "cyl")
  expect_null(out$color)
  .expect_same_print(x)

  data(housing, package = "MASS")
  m <- MASS::polr(Sat ~ Infl + Type, weights = Freq, data = housing, Hess = TRUE)
  for (p in c("mean.class", "latent")) {
    x <- estimate_means(m, by = "Infl", backend = "emmeans", predict = p)
    out <- .recipe_mapping(x)
    expect_identical(out$geoms, c("pointrange", "labs"))
    expect_identical(out$x, "Infl")
    expect_null(out$color)
    .expect_same_print(x)
  }
test_that("visualization_recipe, informative error when nothing goes on x-axis", {
  # intercept-only model
  model <- lm(qsec ~ 1, data = mtcars)
  expect_error(
    visualisation_recipe(estimate_prediction(model)),
    regex = "intercept-only"
  )
  expect_error(
    visualisation_recipe(estimate_relation(model)),
    regex = "intercept-only"
  )
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
