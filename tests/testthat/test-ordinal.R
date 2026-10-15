skip_on_cran()
skip_if_not_installed("curl")
skip_if_offline()
skip_if_not_installed("brms")
skip_if_not_installed("BH")
skip_if_not_installed("RcppEigen")
skip_if_not_installed("marginaleffects", minimum_version = "0.29.0")
skip_if_not_installed("httr2")
skip_if_not_installed("MASS")

test_that("estimate_relation prints ordinal models correctly", {
  m <- suppressWarnings(insight::download_model("brms_categorical_2_num"))
  skip_if(is.null(m))
  out <- suppressWarnings(estimate_relation(m))
  expect_snapshot(print(out, zap_small = TRUE), variant = "windows")
  out <- suppressWarnings(estimate_means(m, by = "Sepal.Width"))
  expect_snapshot(print(out, zap_small = TRUE), variant = "windows")

  m <- MASS::polr(Species ~ Sepal.Width, data = iris)
  out <- estimate_relation(m, verbose = FALSE)
  expect_snapshot(print(out, zap_small = TRUE), variant = "windows")
  out <- estimate_means(m, by = "Sepal.Width")
  expect_snapshot(print(out, zap_small = TRUE), variant = "windows")

  # keep row column
  out <- suppressWarnings(estimate_relation(m, data = iris[1:3, ], verbose = FALSE))
  expect_named(
    out,
    c("Row", "Response", "Sepal.Width", "Predicted", "CI_low", "CI_high", "Residuals")
  )
  expect_identical(dim(out), c(9L, 7L))
})


# compares probabilities from the emmeans and the marginaleffects backend,
# matching rows by focal terms and response category
.compare_ordinal_backends <- function(out_emmeans, out_marginaleffects, by) {
  merge_by <- c(by, "Response")
  out_emmeans <- as.data.frame(out_emmeans)[c(merge_by, "Probability")]
  out_marginaleffects <- as.data.frame(out_marginaleffects)[c(merge_by, "Probability")]
  for (i in merge_by) {
    out_emmeans[[i]] <- as.character(out_emmeans[[i]])
    out_marginaleffects[[i]] <- as.character(out_marginaleffects[[i]])
  }
  merge(out_emmeans, out_marginaleffects, by = merge_by)
}


test_that("estimate_means, backend emmeans, ordinal, predict = 'prob'", {
  skip_if_not_installed("emmeans")
  data(housing, package = "MASS")
  m <- MASS::polr(Sat ~ Infl + Type + Cont, weights = Freq, data = housing)

  # one row per focal level and response category
  out <- estimate_means(m, "Type", predict = "prob", backend = "emmeans")
  expect_identical(nrow(out), 12L)
  expect_setequal(as.character(out$Response), levels(housing$Sat))
  compared <- .compare_ordinal_backends(out, estimate_means(m, "Type"), "Type")
  expect_identical(nrow(compared), 12L)
  expect_equal(compared$Probability.x, compared$Probability.y, tolerance = 1e-6)

  # two focal terms
  out <- estimate_means(m, c("Type", "Infl"), predict = "prob", backend = "emmeans")
  expect_identical(nrow(out), 36L)
  compared <- .compare_ordinal_backends(
    out,
    estimate_means(m, c("Type", "Infl")),
    c("Type", "Infl")
  )
  expect_identical(nrow(compared), 36L)
  expect_equal(compared$Probability.x, compared$Probability.y, tolerance = 1e-6)
})


test_that("estimate_means, backend emmeans, ordinal, default predict", {
  skip_if_not_installed("emmeans")
  data(housing, package = "MASS")
  m <- MASS::polr(Sat ~ Infl + Type + Cont, weights = Freq, data = housing)

  # default returns probabilities, same as predict = "prob"
  out_prob <- estimate_means(m, "Type", predict = "prob", backend = "emmeans")
  out_default <- estimate_means(m, "Type", backend = "emmeans")
  expect_identical(nrow(out_default), 12L)
  expect_identical(
    paste(out_default$Type, out_default$Response),
    paste(out_prob$Type, out_prob$Response)
  )
  expect_equal(out_default$Probability, out_prob$Probability, tolerance = 1e-6)

  # contrasts also default to probabilities, same as predict = "prob"
  out <- estimate_contrasts(m, "Type", backend = "emmeans")
  out_prob <- estimate_contrasts(m, "Type", predict = "prob", backend = "emmeans")
  expect_identical(nrow(out), 18L)
  expect_identical(
    paste(out$Level1, out$Level2, out$Response),
    paste(out_prob$Level1, out_prob$Level2, out_prob$Response)
  )
  expect_equal(out$Difference, out_prob$Difference, tolerance = 1e-6)
})


test_that("estimate_means, backend emmeans, ordinal, mean.class and latent", {
  skip_if_not_installed("emmeans")
  data(housing, package = "MASS")
  m <- MASS::polr(Sat ~ Infl + Type + Cont, weights = Freq, data = housing)

  modes <- c(mean.class = "Mean_class", latent = "Latent")
  for (mode in names(modes)) {
    out <- estimate_means(m, "Type", predict = mode, backend = "emmeans")
    expect_identical(nrow(out), 4L)
    expect_true(modes[[mode]] %in% colnames(out))
    expect_identical(attributes(out)$coef_name, modes[[mode]])
    expected <- as.data.frame(suppressMessages(emmeans::emmeans(m, "Type", mode = mode)))
    estimate_column <- setdiff(
      colnames(expected),
      c("Type", "SE", "df", "asymp.LCL", "asymp.UCL")
    )
    compared <- merge(
      data.frame(
        Type = as.character(out$Type),
        x = out[[modes[[mode]]]],
        stringsAsFactors = FALSE
      ),
      data.frame(
        Type = as.character(expected$Type),
        y = expected[[estimate_column]],
        stringsAsFactors = FALSE
      ),
      by = "Type"
    )
    expect_identical(nrow(compared), 4L)
    expect_equal(compared$x, compared$y, tolerance = 1e-6)
  }
})


test_that("estimate_means, backend emmeans, ordinal, threshold modes", {
  skip_if_not_installed("emmeans")
  data(housing, package = "MASS")
  m <- MASS::polr(Sat ~ Infl + Type + Cont, weights = Freq, data = housing)

  modes <- c(
    cum.prob = "Probability",
    exc.prob = "Probability",
    linear.predictor = "Linear_predictor"
  )
  for (mode in names(modes)) {
    out <- estimate_means(m, "Type", predict = mode, backend = "emmeans")
    expect_identical(nrow(out), 8L)
    expect_setequal(as.character(out$Threshold), c("Low|Medium", "Medium|High"))
    expect_true(modes[[mode]] %in% colnames(out))
    expect_identical(attributes(out)$coef_name, modes[[mode]])
    expected <- as.data.frame(suppressMessages(emmeans::emmeans(
      m,
      c("Type", "cut"),
      mode = mode
    )))
    estimate_column <- setdiff(
      colnames(expected),
      c("Type", "cut", "SE", "df", "asymp.LCL", "asymp.UCL")
    )
    compared <- merge(
      data.frame(
        Type = as.character(out$Type),
        Threshold = as.character(out$Threshold),
        x = out[[modes[[mode]]]],
        stringsAsFactors = FALSE
      ),
      data.frame(
        Type = as.character(expected$Type),
        Threshold = as.character(expected$cut),
        y = expected[[estimate_column]],
        stringsAsFactors = FALSE
      ),
      by = c("Type", "Threshold")
    )
    expect_identical(nrow(compared), 8L)
    expect_equal(compared$x, compared$y, tolerance = 1e-6)
  }
})


test_that("estimate_means, backend emmeans, ordinal, clm and glmmTMB", {
  skip_if_not_installed("emmeans")
  skip_if_not_installed("ordinal")
  data(housing, package = "MASS")
  m <- ordinal::clm(Sat ~ Infl + Type + Cont, weights = Freq, data = housing)
  out <- estimate_means(m, "Type", predict = "prob", backend = "emmeans")
  expect_identical(nrow(out), 12L)
  compared <- .compare_ordinal_backends(out, estimate_means(m, "Type"), "Type")
  expect_identical(nrow(compared), 12L)
  expect_equal(compared$Probability.x, compared$Probability.y, tolerance = 1e-6)

  # ordinal glmmTMB not supported in marginaleffects <= 1.0.0
  skip_if_not_installed("glmmTMB", minimum_version = "1.1.15.2")
  skip_if_not_installed("marginaleffects", minimum_version = "1.0.1")

  m <- glmmTMB::glmmTMB(
    Sat ~ Infl + Type + Cont,
    data = housing,
    family = glmmTMB::ordinal()
  )
  out <- estimate_means(m, "Type", predict = "prob", backend = "emmeans")
  expect_identical(nrow(out), 12L)
  compared <- .compare_ordinal_backends(out, estimate_means(m, "Type"), "Type")
  expect_identical(nrow(compared), 12L)
  expect_equal(compared$Probability.x, compared$Probability.y, tolerance = 1e-6)
})


test_that("estimate_means, backend emmeans, ordinal, transformed response", {
  skip_if_not_installed("emmeans")
  data(housing, package = "MASS")
  housing$SatNum <- as.integer(housing$Sat)
  # `Hess = TRUE`, else `vcov()` re-fits the model in an environment where
  # `SatNum` does not exist
  m <- MASS::polr(
    factor(SatNum) ~ Infl + Type + Cont,
    weights = Freq,
    data = housing,
    Hess = TRUE
  )
  out <- estimate_means(m, "Type", backend = "emmeans")
  expect_identical(nrow(out), 12L)
  expect_setequal(as.character(out$Response), c("1", "2", "3"))
  compared <- .compare_ordinal_backends(out, estimate_means(m, "Type"), "Type")
  expect_identical(nrow(compared), 12L)
  expect_equal(compared$Probability.x, compared$Probability.y, tolerance = 1e-6)
})


test_that("estimate_means, backend emmeans, clmm shows response level names", {
  skip_if_not_installed("emmeans")
  skip_if_not_installed("ordinal")
  data(wine, package = "ordinal")
  # emmeans codes the response categories and thresholds of clmm models as
  # 1, 2, 3, ..., so we check that each code is replaced by its level name
  text_levels <- c("vbad", "bad", "ok", "good", "vgood")
  number_levels <- c("2", "4", "6", "8", "10")
  reversed_levels <- c("5", "4", "3", "2", "1")
  wine$rating_text <- factor(wine$rating, labels = text_levels, ordered = TRUE)
  wine$rating_number <- factor(as.integer(wine$rating) * 2, ordered = TRUE)
  wine$rating_reversed <- factor(
    as.character(wine$rating),
    levels = reversed_levels,
    ordered = TRUE
  )
  all_levels <- list(
    rating_text = text_levels,
    rating_number = number_levels,
    rating_reversed = reversed_levels
  )

  for (response in names(all_levels)) {
    response_levels <- all_levels[[response]]
    threshold_names <- paste(response_levels[-5], response_levels[-1], sep = "|")
    f <- stats::as.formula(paste(response, "~ temp + contact + (1 | judge)"))
    m <- ordinal::clmm(f, data = wine)
    expect_identical(m$y.levels, response_levels)
    expect_identical(names(m$alpha), threshold_names)

    for (by in list("temp", c("temp", "contact"))) {
      # response categories
      out <- estimate_means(m, by, backend = "emmeans")
      expected <- as.data.frame(suppressMessages(emmeans::emmeans(
        m,
        stats::as.formula(paste("~", paste(by, collapse = "+"), "|", response)),
        mode = "prob"
      )))
      expect_identical(nrow(out), nrow(expected))
      expect_identical(
        as.character(out$Response),
        response_levels[as.integer(as.character(expected[[response]]))]
      )
      expect_identical(levels(out$Response), response_levels)
      expect_equal(out$Probability, expected$prob, tolerance = 1e-6)

      # thresholds
      modes <- c(
        cum.prob = "Probability",
        exc.prob = "Probability",
        linear.predictor = "Linear_predictor"
      )
      for (mode in names(modes)) {
        out <- estimate_means(m, by, predict = mode, backend = "emmeans")
        expected <- as.data.frame(suppressMessages(emmeans::emmeans(
          m,
          stats::as.formula(paste("~", paste(by, collapse = "+"), "| cut")),
          mode = mode
        )))
        code <- as.integer(sub("\\|.*", "", as.character(expected$cut)))
        expect_identical(nrow(out), nrow(expected))
        expect_identical(as.character(out$Threshold), threshold_names[code])
        expect_identical(levels(out$Threshold), threshold_names)
        estimate_column <- setdiff(
          colnames(expected),
          c(by, "cut", "SE", "df", "asymp.LCL", "asymp.UCL")
        )
        expect_equal(out[[modes[[mode]]]], expected[[estimate_column]], tolerance = 1e-6)
      }
    }
  }
})


test_that("estimate_means, backend emmeans, clmm threshold names, all threshold types", {
  skip_if_not_installed("emmeans")
  skip_if_not_installed("ordinal")
  data(wine, package = "ordinal")
  text_levels <- c("vbad", "bad", "ok", "good", "vgood")
  threshold_names <- c("vbad|bad", "bad|ok", "ok|good", "good|vgood")
  wine$rating_text <- factor(wine$rating, labels = text_levels, ordered = TRUE)

  # for "symmetric", "symmetric2" and "equidistant", the model parameters are
  # not one per threshold, but emmeans still returns one row per threshold
  for (threshold in c("flexible", "symmetric", "symmetric2", "equidistant")) {
    m <- ordinal::clmm(
      rating_text ~ temp + (1 | judge),
      data = wine,
      threshold = threshold
    )
    for (mode in c("cum.prob", "exc.prob", "linear.predictor")) {
      out <- estimate_means(m, "temp", predict = mode, backend = "emmeans")
      expected <- as.data.frame(suppressMessages(emmeans::emmeans(
        m,
        ~ temp | cut,
        mode = mode
      )))
      code <- as.integer(sub("\\|.*", "", as.character(expected$cut)))
      expect_identical(
        as.character(out$Threshold),
        threshold_names[code],
        info = paste(threshold, mode)
      )
      expect_identical(levels(out$Threshold), threshold_names, info = paste(threshold, mode))
    }
    out <- estimate_means(m, "temp", backend = "emmeans")
    expect_identical(levels(out$Response), text_levels, info = threshold)
  }
})


test_that("estimate_means, backend emmeans, ordinal level names unchanged", {
  skip_if_not_installed("emmeans")
  skip_if_not_installed("ordinal")
  data(housing, package = "MASS")
  housing$SatNum <- as.integer(housing$Sat)
  data(wine, package = "ordinal")

  sat_levels <- c("Low", "Medium", "High")
  models <- list(
    polr = list(
      MASS::polr(Sat ~ Infl + Type + Cont, weights = Freq, data = housing, Hess = TRUE),
      sat_levels
    ),
    polr_transformed = list(
      MASS::polr(factor(SatNum) ~ Infl + Type + Cont, weights = Freq, data = housing, Hess = TRUE),
      c("1", "2", "3")
    ),
    clm = list(
      ordinal::clm(Sat ~ Infl + Type + Cont, weights = Freq, data = housing),
      sat_levels
    ),
    clmm = list(
      ordinal::clmm(rating ~ temp + contact + (1 | judge), data = wine),
      c("1", "2", "3", "4", "5")
    )
  )
  # ordinal family in glmmTMB requires glmmTMB >= 1.1.15.2
  if (
    requireNamespace("glmmTMB", quietly = TRUE) &&
      utils::packageVersion("glmmTMB") >= "1.1.15.2"
  ) {
    models$glmmTMB <- list(
      glmmTMB::glmmTMB(
        Sat ~ Infl + Type + Cont,
        weights = Freq,
        data = housing,
        family = glmmTMB::ordinal()
      ),
      sat_levels
    )
  }

  for (i in names(models)) {
    m <- models[[i]][[1]]
    response_levels <- models[[i]][[2]]
    by <- if (i == "clmm") "temp" else "Type"
    k <- length(response_levels)
    threshold_names <- paste(response_levels[-k], response_levels[-1], sep = "|")

    out <- estimate_means(m, by, predict = "prob", backend = "emmeans")
    expect_identical(unique(as.character(out$Response)), response_levels, info = i)
    for (mode in c("cum.prob", "exc.prob", "linear.predictor")) {
      out <- estimate_means(m, by, predict = mode, backend = "emmeans")
      expect_identical(
        unique(as.character(out$Threshold)),
        threshold_names,
        info = paste(i, mode)
      )
    }
  }
})


test_that("estimate_means, print bracl", {
  skip_if_not_installed("brglm2")
  # required for the penguins dataset, which was added in R 4.5.0
  skip_if(getRversion() < "4.5.0")

  data(penguins, package = "datasets")

  m <- brglm2::bracl(species ~ island + sex, data = penguins)
  out <- estimate_means(m, by = "island")
  expect_snapshot(print(out, zap_small = TRUE), variant = "windows")

  m <- nnet::multinom(species ~ island + sex, data = penguins)
  out <- estimate_means(m, by = "island")
  expect_snapshot(print(out, zap_small = TRUE), variant = "windows")
})


# contrasts from emmeans, with the contrast split into `Level1` and `Level2`,
# and the values of the pseudo-factor (response categories or thresholds) in
# `group`
.emmeans_ordinal_contrasts <- function(m, specs, mode, group = NULL) {
  contrasts <- emmeans::contrast(
    suppressMessages(emmeans::emmeans(m, specs, mode = mode)),
    "revpairwise",
    adjust = "none"
  )
  out <- as.data.frame(contrasts)
  ci <- as.data.frame(stats::confint(contrasts, adjust = "none"))
  contrast_levels <- do.call(rbind, strsplit(as.character(out$contrast), " - ", fixed = TRUE))
  data.frame(
    Level1 = contrast_levels[, 1],
    Level2 = contrast_levels[, 2],
    group = if (is.null(group)) "" else as.character(out[[group]]),
    estimate = out$estimate,
    SE = out$SE,
    p = out$p.value,
    CI_low = ci$asymp.LCL,
    CI_high = ci$asymp.UCL,
    stringsAsFactors = FALSE
  )
}


# rows of `out` in the order of `expected`, matched by levels and the group
# column (`Response` or `Threshold`)
.match_ordinal_contrasts <- function(out, expected, group = NULL) {
  out_group <- if (is.null(group)) "" else as.character(out[[group]])
  out_keys <- paste(out$Level1, out$Level2, out_group)
  expected_keys <- paste(expected$Level1, expected$Level2, expected$group)
  as.data.frame(out)[match(expected_keys, out_keys), ]
}


test_that("estimate_contrasts, backend emmeans, ordinal, probability default", {
  skip_if_not_installed("emmeans")
  skip_if_not_installed("ordinal")
  data(housing, package = "MASS")
  data(wine, package = "ordinal")

  models <- list(
    polr = list(
      MASS::polr(Sat ~ Infl + Type + Cont, weights = Freq, data = housing, Hess = TRUE),
      "Infl",
      "Sat"
    ),
    clm = list(
      ordinal::clm(Sat ~ Infl + Type + Cont, weights = Freq, data = housing),
      "Infl",
      "Sat"
    ),
    clmm = list(
      ordinal::clmm(rating ~ temp + contact + (1 | judge), data = wine),
      "temp",
      "rating"
    )
  )
  # ordinal family in glmmTMB requires glmmTMB >= 1.1.15.2
  if (
    requireNamespace("glmmTMB", quietly = TRUE) &&
      utils::packageVersion("glmmTMB") >= "1.1.15.2"
  ) {
    models$glmmTMB <- list(
      glmmTMB::glmmTMB(
        Sat ~ Infl + Type + Cont,
        weights = Freq,
        data = housing,
        family = glmmTMB::ordinal()
      ),
      "Infl",
      "Sat"
    )
  }

  for (i in names(models)) {
    m <- models[[i]][[1]]
    focal <- models[[i]][[2]]
    response <- models[[i]][[3]]
    expected <- .emmeans_ordinal_contrasts(
      m,
      stats::as.formula(paste("~", focal, "|", response)),
      "prob",
      response
    )

    # differences of probabilities within each response category, for the
    # default and for predict = "response"
    for (predict in list(NULL, "response")) {
      out <- estimate_contrasts(m, focal, predict = predict, backend = "emmeans")
      expect_identical(nrow(out), nrow(expected), info = i)
      expect_true("Response" %in% colnames(out), info = i)
      matched <- .match_ordinal_contrasts(out, expected, "Response")
      expect_false(anyNA(matched$Difference), info = i)
      expect_equal(matched$Difference, expected$estimate, tolerance = 1e-6, info = i)
      expect_equal(matched$SE, expected$SE, tolerance = 1e-6, info = i)
      expect_equal(matched$p, expected$p, tolerance = 1e-6, info = i)
      expect_equal(matched$CI_low, expected$CI_low, tolerance = 1e-6, info = i)
      expect_equal(matched$CI_high, expected$CI_high, tolerance = 1e-6, info = i)
    }

    # same differences as the marginaleffects backend, for contrasts within one
    # response category. marginaleffects does not support clmm, and ordinal
    # glmmTMB requires marginaleffects >= 1.0.1
    if (i %in% c("polr", "clm")) {
      out <- as.data.frame(estimate_contrasts(m, focal))
      out <- out[as.character(out$Response1) == as.character(out$Response2), ]
      out$Response <- out$Response1
      matched <- .match_ordinal_contrasts(out, expected, "Response")
      expect_false(anyNA(matched$Difference), info = i)
      expect_equal(matched$Difference, expected$estimate, tolerance = 1e-4, info = i)
    }
  }
})


test_that("estimate_contrasts, backend emmeans, ordinal modes", {
  skip_if_not_installed("emmeans")
  data(housing, package = "MASS")
  m <- MASS::polr(Sat ~ Infl + Type + Cont, weights = Freq, data = housing, Hess = TRUE)

  # the column that holds the pseudo-factor, if any
  group_columns <- list(
    latent = NULL,
    linear.predictor = "Threshold",
    cum.prob = "Threshold",
    exc.prob = "Threshold",
    prob = "Response",
    mean.class = NULL
  )
  for (mode in names(group_columns)) {
    group <- group_columns[[mode]]
    pseudo_factor <- switch(mode, prob = "Sat", latent = , mean.class = NULL, "cut")
    specs <- if (is.null(pseudo_factor)) ~Infl else stats::as.formula(paste("~ Infl |", pseudo_factor))
    expected <- .emmeans_ordinal_contrasts(m, specs, mode, pseudo_factor)

    out <- estimate_contrasts(m, "Infl", predict = mode, backend = "emmeans")
    expect_identical(
      intersect(c("Response", "Threshold"), colnames(out)),
      if (is.null(group)) character(0) else group,
      info = mode
    )
    expect_identical(nrow(out), nrow(expected), info = mode)
    matched <- .match_ordinal_contrasts(out, expected, group)
    expect_false(anyNA(matched$Difference), info = mode)
    expect_equal(matched$Difference, expected$estimate, tolerance = 1e-6, info = mode)
    expect_equal(matched$SE, expected$SE, tolerance = 1e-6, info = mode)
  }
})


test_that("estimate_contrasts, backend emmeans, clmm shows level names", {
  skip_if_not_installed("emmeans")
  skip_if_not_installed("ordinal")
  data(wine, package = "ordinal")
  wine$rating_text <- factor(wine$rating, labels = letters[1:5], ordered = TRUE)
  all_levels <- list(rating = as.character(1:5), rating_text = letters[1:5])

  for (response in names(all_levels)) {
    response_levels <- all_levels[[response]]
    threshold_names <- paste(response_levels[-5], response_levels[-1], sep = "|")
    f <- stats::as.formula(paste(response, "~ temp + contact + (1 | judge)"))
    m <- ordinal::clmm(f, data = wine)

    out <- estimate_contrasts(m, "temp", backend = "emmeans")
    expect_identical(nrow(out), 5L, info = response)
    expect_setequal(as.character(out$Response), response_levels)
    expect_identical(levels(out$Response), response_levels, info = response)

    for (mode in c("cum.prob", "exc.prob", "linear.predictor")) {
      out <- estimate_contrasts(m, "temp", predict = mode, backend = "emmeans")
      expect_identical(nrow(out), 4L, info = paste(response, mode))
      expect_setequal(as.character(out$Threshold), threshold_names)
      expect_identical(levels(out$Threshold), threshold_names, info = paste(response, mode))
    }
  }
})


# polr fits with a numeric predictor `x` and a factor `g`, for slopes. For the
# second fit, `emmeans::emtrends(mode = "prob")` fails with "Irregular
# reference grid" (emmeans 2.0.4)
.ordinal_slope_models <- function() {
  data(housing, package = "MASS")
  set.seed(1)
  housing$x <- as.numeric(housing$Cont) + stats::rnorm(nrow(housing))
  housing$g <- housing$Infl
  set.seed(2)
  n <- 400
  simulated <- data.frame(
    x = stats::rnorm(n),
    g = factor(sample(c("a", "b", "c"), n, replace = TRUE))
  )
  simulated$y <- cut(
    simulated$x + as.numeric(simulated$g) + stats::rlogis(n),
    c(-Inf, 1, 2.5, Inf),
    labels = c("lo", "mid", "hi"),
    ordered_result = TRUE
  )
  list(
    housing = MASS::polr(Sat ~ g * x, weights = Freq, data = housing, Hess = TRUE),
    simulated = MASS::polr(y ~ x * g, data = simulated, Hess = TRUE)
  )
}


test_that("estimate_slopes, backend emmeans, ordinal", {
  skip_if_not_installed("emmeans")
  models <- .ordinal_slope_models()

  for (i in names(models)) {
    m <- models[[i]]

    # slopes of the probability of each response category, for the default
    # and for predict = "prob". P(y = k) = P(y <= k) - P(y <= k - 1), so each
    # slope is the slope of the cumulative probability at the threshold above
    # the category, less the one at the threshold below (0 if there is none)
    cumulative <- as.data.frame(suppressMessages(emmeans::emtrends(
      m,
      ~ g | cut,
      var = "x",
      mode = "cum.prob"
    )))
    response_levels <- m$lev
    n_cuts <- length(response_levels) - 1
    cumulative_slope <- function(g, k) {
      if (k < 1 || k > n_cuts) {
        return(0)
      }
      cumulative$x.trend[
        as.character(cumulative$g) == g & as.integer(cumulative$cut) == k
      ]
    }
    out_marginaleffects <- as.data.frame(estimate_slopes(m, "x", by = "g"))
    for (predict in list(NULL, "prob")) {
      out <- as.data.frame(estimate_slopes(
        m,
        "x",
        by = "g",
        predict = predict,
        backend = "emmeans"
      ))
      expect_identical(nrow(out), 9L, info = i)
      expect_true("Response" %in% colnames(out), info = i)
      expected <- mapply(
        function(g, response) {
          k <- match(response, response_levels)
          cumulative_slope(g, k) - cumulative_slope(g, k - 1)
        },
        as.character(out$g),
        as.character(out$Response)
      )
      expect_equal(out$Slope, unname(expected), tolerance = 1e-6, info = i)
      # marginaleffects computes the slopes with a smaller step than emtrends(),
      # which uses a forward difference with a step of 0.001 times the range
      # of `x`. For the simulated fit, single slopes differ by up to 1.2%
      # (emmeans 2.0.4, marginaleffects 1.0.0)
      matched <- out_marginaleffects[match(
        paste(out$g, out$Response),
        paste(out_marginaleffects$g, out_marginaleffects$Response)
      ), ]
      expect_false(anyNA(matched$Slope), info = i)
      expect_equal(out$Slope, matched$Slope, tolerance = 1e-2, info = i)
      expect_equal(out$SE, matched$SE, tolerance = 1e-2, info = i)
      # the probabilities sum to 1, so their slopes sum to 0
      expect_equal(
        as.vector(tapply(out$Slope, out$g, sum)),
        c(0, 0, 0),
        tolerance = 1e-8,
        info = i
      )
    }

    # modes with slopes from emtrends()
    for (mode in c("latent", "linear.predictor", "cum.prob", "exc.prob")) {
      specs <- if (mode == "latent") ~g else ~ g | cut
      expected <- as.data.frame(suppressMessages(emmeans::emtrends(
        m,
        specs,
        var = "x",
        mode = mode
      )))
      expected_group <- if (mode == "latent") "" else as.character(expected$cut)
      out <- as.data.frame(estimate_slopes(
        m,
        "x",
        by = "g",
        predict = mode,
        backend = "emmeans"
      ))
      expect_identical(
        intersect(c("Response", "Threshold"), colnames(out)),
        if (mode == "latent") character(0) else "Threshold",
        info = paste(i, mode)
      )
      out_group <- if (mode == "latent") "" else as.character(out$Threshold)
      matched <- out[match(
        paste(expected$g, expected_group),
        paste(out$g, out_group)
      ), ]
      expect_identical(nrow(out), nrow(expected), info = paste(i, mode))
      expect_false(anyNA(matched$Slope), info = paste(i, mode))
      expect_equal(matched$Slope, expected$x.trend, tolerance = 1e-6, info = paste(i, mode))
      expect_equal(matched$SE, expected$SE, tolerance = 1e-6, info = paste(i, mode))
    }

    # the mean class is 1 + the sum of the exceedance probabilities
    out <- as.data.frame(estimate_slopes(
      m,
      "x",
      by = "g",
      predict = "mean.class",
      backend = "emmeans"
    ))
    out_exceedance <- as.data.frame(estimate_slopes(
      m,
      "x",
      by = "g",
      predict = "exc.prob",
      backend = "emmeans"
    ))
    expect_identical(nrow(out), 3L, info = i)
    expect_length(intersect(c("Response", "Threshold"), colnames(out)), 0)
    sums <- tapply(out_exceedance$Slope, as.character(out_exceedance$g), sum)
    expect_equal(
      out$Slope,
      as.vector(sums[as.character(out$g)]),
      tolerance = 1e-6,
      info = i
    )
  }
})


test_that("estimate_contrasts, backend emmeans, ordinal slopes", {
  skip_if_not_installed("emmeans")
  models <- .ordinal_slope_models()

  for (i in names(models)) {
    m <- models[[i]]
    slopes <- as.data.frame(estimate_slopes(m, "x", by = "g", backend = "emmeans"))
    out <- as.data.frame(estimate_contrasts(m, "x", by = "g", backend = "emmeans"))
    expect_true("Response" %in% colnames(out), info = i)
    expect_identical(nrow(out), 9L, info = i)

    # each later level minus each earlier level ("revpairwise")
    g_levels <- levels(slopes$g)
    pairs <- utils::combn(seq_along(g_levels), 2)
    expect_setequal(
      paste(out$Level1, out$Level2),
      paste(g_levels[pairs[2, ]], g_levels[pairs[1, ]])
    )

    # differences of the slopes within each response category
    slope_of <- function(g, response) {
      slopes$Slope[as.character(slopes$g) == g & as.character(slopes$Response) == response]
    }
    expected <- mapply(
      function(level1, level2, response) {
        slope_of(level1, response) - slope_of(level2, response)
      },
      as.character(out$Level1),
      as.character(out$Level2),
      as.character(out$Response)
    )
    expect_equal(out$Difference, unname(expected), tolerance = 1e-6, info = i)
  }
})


test_that("estimate_slopes, backend emmeans, clmm", {
  skip_if_not_installed("emmeans")
  skip_if_not_installed("ordinal")
  data(wine, package = "ordinal")
  set.seed(3)
  wine$x <- stats::rnorm(nrow(wine)) + as.numeric(wine$temp)
  wine$rating_text <- factor(wine$rating, labels = letters[1:5], ordered = TRUE)
  all_levels <- list(rating = as.character(1:5), rating_text = letters[1:5])

  for (response in names(all_levels)) {
    response_levels <- all_levels[[response]]
    threshold_names <- paste(response_levels[-5], response_levels[-1], sep = "|")
    f <- stats::as.formula(paste(response, "~ x * contact + (1 | judge)"))
    m <- ordinal::clmm(f, data = wine)

    out <- as.data.frame(estimate_slopes(m, "x", by = "contact", backend = "emmeans"))
    expect_identical(nrow(out), 10L, info = response)
    expect_identical(levels(out$Response), response_levels, info = response)

    # slopes of the probabilities, compared with a central finite difference
    # of the probabilities at the mean of `x`
    h <- 1e-4
    probabilities <- function(x) {
      as.data.frame(suppressMessages(emmeans::emmeans(
        m,
        stats::as.formula(paste("~ contact |", response)),
        at = list(x = x),
        mode = "prob"
      )))
    }
    upper <- probabilities(mean(wine$x) + h)
    lower <- probabilities(mean(wine$x) - h)
    finite_difference <- (upper$prob - lower$prob) / (2 * h)
    # emmeans codes the response categories of clmm models as 1, 2, 3, ...
    upper_levels <- response_levels[as.integer(as.character(upper[[response]]))]
    matched <- out[match(
      paste(upper$contact, upper_levels),
      paste(out$contact, out$Response)
    ), ]
    expect_false(anyNA(matched$Slope), info = response)
    expect_equal(matched$Slope, finite_difference, tolerance = 1e-3, info = response)

    for (mode in c("cum.prob", "exc.prob", "linear.predictor")) {
      out <- as.data.frame(estimate_slopes(
        m,
        "x",
        by = "contact",
        predict = mode,
        backend = "emmeans"
      ))
      expect_identical(nrow(out), 8L, info = paste(response, mode))
      expect_identical(levels(out$Threshold), threshold_names, info = paste(response, mode))
    }

    # contrasts of slopes keep the level names
    out <- estimate_contrasts(m, "x", by = "contact", backend = "emmeans")
    expect_identical(levels(out$Response), response_levels, info = response)
    out <- estimate_contrasts(m, "x", by = "contact", predict = "cum.prob", backend = "emmeans")
    expect_identical(levels(out$Threshold), threshold_names, info = response)
  }
})
