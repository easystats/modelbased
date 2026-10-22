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
  text_levels <- c("vbad", "bad", "ok", "good", "vgood")
  threshold_names <- paste(text_levels[-5], text_levels[-1], sep = "|")
  wine$rating_text <- factor(wine$rating, labels = text_levels, ordered = TRUE)
  m <- ordinal::clmm(rating_text ~ temp + contact + (1 | judge), data = wine)

  # emmeans codes the response categories of clmm models as 1, 2, 3, ...
  out <- estimate_means(m, "temp", backend = "emmeans")
  expected <- as.data.frame(suppressMessages(emmeans::emmeans(
    m,
    ~ temp | rating_text,
    mode = "prob"
  )))
  expect_identical(
    as.character(out$Response),
    text_levels[as.integer(as.character(expected$rating_text))]
  )
  expect_identical(levels(out$Response), text_levels)
  out <- estimate_means(m, "temp", predict = "cum.prob", backend = "emmeans")
  expect_identical(levels(out$Threshold), threshold_names)

  # `names(m$alpha)` are not the thresholds for `threshold = "symmetric"`
  m <- ordinal::clmm(
    rating_text ~ temp + (1 | judge),
    data = wine,
    threshold = "symmetric"
  )
  out <- estimate_means(m, "temp", predict = "cum.prob", backend = "emmeans")
  expect_identical(levels(out$Threshold), threshold_names)
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


test_that("estimate_contrasts, backend emmeans, ordinal modes", {
  skip_if_not_installed("emmeans")
  data(housing, package = "MASS")
  m <- MASS::polr(Sat ~ Infl + Type + Cont, weights = Freq, data = housing, Hess = TRUE)
  emmeans_contrasts <- function(specs, mode) {
    as.data.frame(emmeans::contrast(
      suppressMessages(emmeans::emmeans(m, specs, mode = mode)),
      "revpairwise",
      adjust = "none"
    ))
  }

  out <- estimate_contrasts(m, "Infl", backend = "emmeans")
  expected <- emmeans_contrasts(~ Infl | Sat, "prob")
  expect_identical(as.character(out$Response), as.character(expected$Sat))
  expect_equal(out$Difference, expected$estimate, tolerance = 1e-6)
  expect_equal(out$SE, expected$SE, tolerance = 1e-6)

  out <- estimate_contrasts(m, "Infl", predict = "cum.prob", backend = "emmeans")
  expected <- emmeans_contrasts(~ Infl | cut, "cum.prob")
  expect_identical(as.character(out$Threshold), as.character(expected$cut))
  expect_equal(out$Difference, expected$estimate, tolerance = 1e-6)

  out <- estimate_contrasts(m, "Infl", predict = "mean.class", backend = "emmeans")
  expect_equal(out$Difference, c(0.5528118, 0.3039427, 0.2488691), tolerance = 1e-6)
})


test_that("estimate_contrasts, backend emmeans, ordinal, numeric by", {
  skip_if_not_installed("emmeans")
  data(housing, package = "MASS")
  set.seed(1)
  housing$x <- stats::rnorm(nrow(housing))
  m <- MASS::polr(Sat ~ Infl * x, weights = Freq, data = housing, Hess = TRUE)

  out <- estimate_contrasts(m, "Infl", by = "x", length = 3, backend = "emmeans")
  expected <- as.data.frame(emmeans::contrast(
    emmeans::emmeans(m, ~ Infl | x + Sat, at = list(x = unique(out$x)), mode = "prob"),
    "revpairwise",
    adjust = "none"
  ))
  # rows in the order of emmeans
  expect_identical(nrow(out), 27L)
  expect_equal(out$x, expected$x, tolerance = 1e-6)
  expect_equal(out$Difference, expected$estimate, tolerance = 1e-6)
  expect_equal(out$SE, expected$SE, tolerance = 1e-6)
})


test_that("estimate_contrasts and estimate_slopes, backend emmeans, clmm shows level names", {
  skip_if_not_installed("emmeans")
  skip_if_not_installed("ordinal")
  data(wine, package = "ordinal")
  set.seed(3)
  wine$x <- stats::rnorm(nrow(wine)) + as.numeric(wine$temp)
  levels(wine$rating) <- letters[1:5]
  threshold_names <- c("a|b", "b|c", "c|d", "d|e")
  m <- ordinal::clmm(rating ~ temp + x * contact + (1 | judge), data = wine)

  out <- estimate_contrasts(m, "temp", backend = "emmeans")
  expect_identical(levels(out$Response), letters[1:5])
  out <- estimate_contrasts(m, "temp", predict = "cum.prob", backend = "emmeans")
  expect_identical(levels(out$Threshold), threshold_names)

  out <- estimate_slopes(m, "x", by = "contact", backend = "emmeans")
  expect_identical(levels(out$Response), letters[1:5])
  out <- estimate_slopes(
    m,
    "x",
    by = "contact",
    predict = "cum.prob",
    backend = "emmeans"
  )
  expect_identical(levels(out$Threshold), threshold_names)
  out <- estimate_contrasts(m, "x", by = "contact", backend = "emmeans")
  expect_identical(levels(out$Response), letters[1:5])
})


# polr fit with a numeric predictor `x` and a factor `g`, for slopes.
# `emmeans::emtrends(mode = "prob")` fails for it with "Irregular reference
# grid" (emmeans 2.0.4)
.ordinal_slope_model <- function() {
  set.seed(2)
  n <- 400
  d <- data.frame(
    x = stats::rnorm(n),
    g = factor(sample(c("a", "b", "c"), n, replace = TRUE))
  )
  d$y <- cut(
    d$x + as.numeric(d$g) + stats::rlogis(n),
    c(-Inf, 1, 2.5, Inf),
    labels = c("lo", "mid", "hi"),
    ordered_result = TRUE
  )
  MASS::polr(y ~ x * g, data = d, Hess = TRUE)
}


test_that("estimate_slopes, backend emmeans, ordinal", {
  skip_if_not_installed("emmeans")
  m <- .ordinal_slope_model()

  # P(y = k) = P(y <= k) - P(y <= k - 1)
  cumulative <- as.data.frame(suppressMessages(emmeans::emtrends(
    m,
    ~ cut | g,
    var = "x",
    mode = "cum.prob"
  )))
  cumulative <- matrix(cumulative$x.trend, nrow = 2)
  out <- estimate_slopes(m, "x", by = "g", backend = "emmeans")
  expected <- rbind(cumulative[1, ], cumulative[2, ] - cumulative[1, ], -cumulative[2, ])
  expect_equal(
    out$Slope,
    expected[cbind(as.integer(out$Response), as.integer(out$g))],
    tolerance = 1e-6
  )

  out <- estimate_slopes(m, "x", backend = "emmeans")
  cumulative <- as.data.frame(suppressMessages(emmeans::emtrends(
    m,
    ~cut,
    var = "x",
    mode = "cum.prob"
  )))$x.trend
  expect_equal(
    out$Slope,
    c(cumulative[1], cumulative[2] - cumulative[1], -cumulative[2]),
    tolerance = 1e-6
  )

  for (mode in c("latent", "cum.prob")) {
    specs <- if (mode == "latent") ~g else ~ g | cut
    expected <- as.data.frame(suppressMessages(emmeans::emtrends(
      m,
      specs,
      var = "x",
      mode = mode
    )))
    out <- estimate_slopes(m, "x", by = "g", predict = mode, backend = "emmeans")
    expect_equal(out$Slope, expected$x.trend, tolerance = 1e-6, info = mode)
  }

  # the mean class is 1 + the sum of the exceedance probabilities
  out <- estimate_slopes(m, "x", by = "g", predict = "mean.class", backend = "emmeans")
  out_exceedance <- estimate_slopes(
    m,
    "x",
    by = "g",
    predict = "exc.prob",
    backend = "emmeans"
  )
  expect_equal(
    out$Slope,
    as.vector(tapply(out_exceedance$Slope, out_exceedance$g, sum)),
    tolerance = 1e-6
  )
  out <- estimate_slopes(m, "x", predict = "mean.class", backend = "emmeans")
  expect_equal(out$Slope, sum(out_exceedance$Slope) / 3, tolerance = 1e-6)
})


test_that("estimate_contrasts, backend emmeans, ordinal slopes", {
  skip_if_not_installed("emmeans")
  m <- .ordinal_slope_model()
  slopes <- estimate_slopes(m, "x", by = "g", backend = "emmeans")
  out <- estimate_contrasts(m, "x", by = "g", backend = "emmeans")
  slope_of <- function(g, response) {
    slopes$Slope[slopes$g == g & slopes$Response == response]
  }
  expected <- mapply(
    function(level1, level2, response) {
      slope_of(level1, response) - slope_of(level2, response)
    },
    as.character(out$Level1),
    as.character(out$Level2),
    as.character(out$Response)
  )
  expect_identical(nrow(out), 9L)
  expect_equal(out$Difference, unname(expected), tolerance = 1e-6)
})


test_that("estimate_contrasts and estimate_slopes, backend emmeans, ordinal footer", {
  skip_if_not_installed("emmeans")
  m <- .ordinal_slope_model()
  expect_output(
    print(estimate_contrasts(m, "g", backend = "emmeans")),
    "Contrasts are on the prob-scale.",
    fixed = TRUE
  )
  expect_output(
    print(estimate_slopes(m, "x", by = "g", backend = "emmeans")),
    "Slopes are on the prob-scale.",
    fixed = TRUE
  )

  # no scale line for the slopes of other models
  m <- lm(mpg ~ wt * factor(am), data = mtcars)
  out <- utils::capture.output(print(estimate_slopes(m, "wt", backend = "emmeans")))
  expect_false(any(grepl("Slopes are on the", out, fixed = TRUE)))
})


test_that("estimate_contrasts, backend emmeans, ordinal, latent contrasts as before", {
  skip_if_not_installed("emmeans")
  data(housing, package = "MASS")
  m <- MASS::polr(Sat ~ Infl + Type + Cont, weights = Freq, data = housing)
  # the former default, before probabilities became the default
  out <- estimate_contrasts(m, "Type", predict = "latent", backend = "emmeans")
  expect_identical(
    paste(out$Level1, out$Level2),
    c(
      "Apartment Tower",
      "Atrium Apartment",
      "Atrium Tower",
      "Terrace Apartment",
      "Terrace Atrium",
      "Terrace Tower"
    )
  )
  expect_equal(
    out$Difference,
    c(-0.5723501, 0.2061636, -0.3661866, -0.5186648, -0.7248283, -1.0910149),
    tolerance = 1e-6
  )
})


test_that("estimate_contrasts, backend emmeans, ordinal, names of response and predictors", {
  skip_if_not_installed("emmeans")
  data(housing, package = "MASS")
  set.seed(1)
  housing$x <- as.numeric(housing$Cont) + stats::rnorm(nrow(housing))
  sat_levels <- c("Low", "Medium", "High")

  # a response named like an emmeans output column
  housing$prob <- housing$Sat
  m <- MASS::polr(prob ~ Infl + Type + Cont, weights = Freq, data = housing, Hess = TRUE)
  out <- estimate_contrasts(m, "Infl", backend = "emmeans")
  expect_identical(levels(out$Response), sat_levels)
  m <- MASS::polr(prob ~ Infl * x, weights = Freq, data = housing, Hess = TRUE)
  out <- estimate_contrasts(m, "x", by = "Infl", backend = "emmeans")
  expect_identical(levels(out$Response), sat_levels)

  # a predictor named `Response` keeps its column
  housing$Response <- housing$Infl
  m <- MASS::polr(
    Sat ~ Response + Type + Cont,
    weights = Freq,
    data = housing,
    Hess = TRUE
  )
  out <- estimate_contrasts(m, "Type", by = "Response", backend = "emmeans")
  expect_identical(nrow(out), 54L)
  expect_identical(levels(out$Response.1), sat_levels)
})


# `by` filters on the response categories, emmeans backend --------------------

ordinal_filter_slope_model <- function() {
  data(housing, package = "MASS", envir = environment())
  set.seed(1)
  housing$x <- stats::rnorm(nrow(housing))
  MASS::polr(Sat ~ x * Infl + Type, data = housing, Hess = TRUE)
}


test_that("estimate_means and estimate_contrasts, backend emmeans, ordinal, by filter on the response", {
  skip_if_not_installed("emmeans")
  skip_if_not_installed("ordinal")
  data(housing, package = "MASS")
  m <- MASS::polr(Sat ~ Infl + Type + Cont, weights = Freq, data = housing, Hess = TRUE)

  out_all <- as.data.frame(estimate_means(m, "Infl", backend = "emmeans"))
  out <- estimate_means(m, c("Infl", "Sat='Low'"), backend = "emmeans")
  expect_equal(
    as.data.frame(out),
    out_all[out_all$Response == "Low", ],
    ignore_attr = TRUE
  )
  # the response without values keeps all categories, and does not undo a filter
  out <- estimate_means(m, c("Infl", "Sat"), backend = "emmeans")
  expect_equal(as.data.frame(out), out_all, ignore_attr = TRUE)
  out <- estimate_means(
    m,
    c("Infl", "Sat", "Sat='Low'", "Sat='High'"),
    backend = "emmeans"
  )
  expect_equal(
    as.data.frame(out),
    out_all[out_all$Response != "Medium", ],
    ignore_attr = TRUE
  )

  out_all <- as.data.frame(estimate_contrasts(
    m,
    "Infl",
    p_adjust = "holm",
    backend = "emmeans"
  ))
  out <- estimate_contrasts(
    m,
    "Infl",
    by = "Sat=c('Low','High')",
    p_adjust = "holm",
    backend = "emmeans"
  )
  expect_equal(
    as.data.frame(out),
    out_all[out_all$Response != "Medium", ],
    ignore_attr = TRUE
  )

  data(wine, package = "ordinal")
  levels(wine$rating) <- letters[1:5]
  m <- ordinal::clmm(rating ~ temp + contact + (1 | judge), data = wine)
  out_all <- as.data.frame(estimate_means(m, "temp", backend = "emmeans"))
  out <- estimate_means(m, c("temp", "rating='b'"), backend = "emmeans")
  expect_equal(as.data.frame(out), out_all[out_all$Response == "b", ], ignore_attr = TRUE)
})


test_that("estimate_slopes, backend emmeans, ordinal, by filter on the response", {
  skip_if_not_installed("emmeans")
  m <- ordinal_filter_slope_model()

  out_all <- as.data.frame(estimate_slopes(m, "x", by = "Infl", backend = "emmeans"))
  out <- estimate_slopes(m, "x", by = c("Infl", "Sat='Low'"), backend = "emmeans")
  expect_equal(
    as.data.frame(out),
    out_all[out_all$Response == "Low", ],
    ignore_attr = TRUE
  )

  out_all <- as.data.frame(estimate_contrasts(m, "x", by = "Infl", backend = "emmeans"))
  out <- estimate_contrasts(m, "x", by = c("Infl", "Sat='Low'"), backend = "emmeans")
  expect_equal(
    as.data.frame(out),
    out_all[out_all$Response == "Low", ],
    ignore_attr = TRUE
  )

  # slopes use `"cum.prob"` internally, but the mode check uses `predict`
  expect_error(
    estimate_slopes(
      m,
      "x",
      by = c("Infl", "Sat='Low'"),
      predict = "cum.prob",
      backend = "emmeans"
    ),
    "The response `Sat` can only be used in `by`",
    fixed = TRUE
  )
})


test_that("estimate_means and estimate_contrasts, backend emmeans, ordinal, errors for response filters", {
  skip_if_not_installed("emmeans")
  skip_if_not_installed("ordinal")
  data(housing, package = "MASS")
  m <- MASS::polr(Sat ~ Infl + Type + Cont, weights = Freq, data = housing, Hess = TRUE)

  expect_error(
    estimate_means(m, c("Infl", "Sat='Low'"), predict = "cum.prob", backend = "emmeans"),
    "The response `Sat` can only be used in `by`",
    fixed = TRUE
  )
  expect_error(
    estimate_contrasts(
      m,
      "Infl",
      by = "Sat='Low'",
      predict = "latent",
      backend = "emmeans"
    ),
    "The response `Sat` can only be used in `by`",
    fixed = TRUE
  )
  expect_error(
    estimate_means(m, c("Infl", "Sat='Lowest'"), backend = "emmeans"),
    "The response `Sat` has no category `Lowest`.",
    fixed = TRUE
  )
  expect_error(
    estimate_means(m, c("Infl", "Sat=c('Lowest','Highest','Low')"), backend = "emmeans"),
    "has no categories `Lowest` and `Highest`.",
    fixed = TRUE
  )
  expect_error(
    estimate_means(m, c("Infl", "Sat=[fivenum]"), backend = "emmeans"),
    "Could not read the response categories in `Sat=[fivenum]`.",
    fixed = TRUE
  )

  # a category of the data that the model does not have
  data(wine, package = "ordinal")
  wine$rating <- factor(wine$rating, levels = 1:6, ordered = TRUE)
  m <- ordinal::clm(rating ~ temp, data = wine)
  expect_error(
    estimate_means(m, c("temp", "rating='6'"), backend = "emmeans"),
    "The response `rating` has no category `6`.",
    fixed = TRUE
  )
})


test_that("estimate_contrasts, ordinal slopes, by filter that keeps one level", {
  d <- MASS::housing
  set.seed(1)
  d$x <- rnorm(nrow(d))
  m <- MASS::polr(Sat ~ x * Infl + Type, data = d, Hess = TRUE)
  keys <- c("Level1", "Response1", "Level2", "Response2")

  # one level: the contrasts between the response categories at that level
  for (estimate in c("typical", "average")) {
    out_all <- as.data.frame(estimate_contrasts(m, "x", by = "Infl", estimate = estimate))
    out <- as.data.frame(estimate_contrasts(
      m,
      "x",
      by = "Infl='Low'",
      estimate = estimate
    ))
    expected <- out_all[out_all$Level1 == "Low" & out_all$Level2 == "Low", ]
    expect_identical(
      lapply(out[keys], as.character),
      lapply(expected[keys], as.character)
    )
    expect_equal(out$Difference, expected$Difference, tolerance = 1e-8)
  }

  # response levels with parentheses keep a label
  d2 <- d
  levels(d2$Sat) <- c("Low (bad)", "Mid", "High (good)")
  m2 <- MASS::polr(Sat ~ x * Infl + Type, data = d2, Hess = TRUE)
  out <- estimate_contrasts(m2, "x", by = "Infl='Low'")
  expect_length(unique(c(as.character(out$Response1), as.character(out$Response2))), 3L)

  # custom hypotheses and group formulas are not labelled for one level
  expect_error(
    estimate_contrasts(m, "x", by = "Infl='Low'", comparison = "b1=b2"),
    "No contrasts to show",
    fixed = TRUE
  )
  expect_error(
    estimate_contrasts(m, "x", by = "Infl='Low'", comparison = ~ pairwise | Infl),
    "No contrasts to show",
    fixed = TRUE
  )

  # models without response categories still need two slopes
  mt <- transform(mtcars, g = factor(am))
  expect_error(
    estimate_contrasts(lm(mpg ~ wt * g, data = mt), "wt", by = "g='0'"),
    "The filter in `by` leaves fewer than two slopes to compare",
    fixed = TRUE
  )
})


test_that("estimate_contrasts, backend emmeans, ordinal, joint tests of the response categories", {
  skip_if_not_installed("emmeans")
  data(housing, package = "MASS")
  m <- MASS::polr(Sat ~ Infl + Type, weights = Freq, data = housing, Hess = TRUE)
  expect_no_warning({
    out <- estimate_contrasts(
      m,
      "Sat",
      by = "Infl",
      comparison = "joint",
      backend = "emmeans"
    )
  })
  # Wald statistics from `marginaleffects::hypotheses(joint = TRUE)`
  expect_equal(out$Chisq, c(45.096, 29.214, 90.344), tolerance = 1e-3)
})


test_that("estimate_contrasts, backend emmeans, ordinal, joint tests of a predictor", {
  skip_if_not_installed("emmeans")
  data(housing, package = "MASS")
  m <- MASS::polr(Sat ~ Infl + Type, weights = Freq, data = housing, Hess = TRUE)
  expected <- as.data.frame(emmeans::joint_tests(
    emmeans::contrast(emmeans::emmeans(m, ~ Infl | Sat, mode = "prob"), "pairwise"),
    by = "Sat"
  ))
  expect_no_warning({
    out <- estimate_contrasts(m, "Infl", comparison = "joint", backend = "emmeans")
  })
  expect_identical(as.character(out$Response), as.character(expected$Sat))
  expect_equal(out$Chisq, expected$Chisq, tolerance = 1e-3)

  # printed with two decimals, as the F column
  printed <- capture.output(print(out))
  expect_true(any(grepl(" 114.59 |", printed, fixed = TRUE)))

  out <- get_emcontrasts(m, "Infl", comparison = "joint")
  expect_identical(class(out), "data.frame")
  expect_null(attr(out, "digits"))
})


# `by` as list, data frame or formula, with the response -----------------------

test_that("estimate_means, backend emmeans, ordinal, list and data frame filter on the response", {
  skip_if_not_installed("emmeans")
  data(housing, package = "MASS")
  m <- MASS::polr(Sat ~ Infl + Type, weights = Freq, data = housing, Hess = TRUE)
  out_all <- estimate_means(m, "Infl", backend = "emmeans")

  out <- estimate_means(
    m,
    list(Infl = c("Low", "High"), Sat = "Low"),
    backend = "emmeans"
  )
  expect_identical(nrow(out), 2L)
  expect_equal(out$Probability, out_all$Probability[c(1, 3)], tolerance = 1e-10)
  out <- estimate_means(m, data.frame(Infl = "Low", Sat = "Low"), backend = "emmeans")
  expect_equal(out$Probability, out_all$Probability[1], tolerance = 1e-10)

  out <- estimate_contrasts(m, "Infl", by = "Sat", backend = "emmeans")
  out_list <- estimate_contrasts(
    m,
    "Infl",
    by = list(Sat = c("Low", "High")),
    backend = "emmeans"
  )
  expect_equal(
    out_list$Difference,
    out$Difference[out$Response != "Medium"],
    tolerance = 1e-10
  )

  m2 <- ordinal_filter_slope_model()
  out <- estimate_slopes(m2, "x", by = "Sat='Low'", backend = "emmeans")
  out_list <- estimate_slopes(m2, "x", by = list(Sat = "Low"), backend = "emmeans")
  expect_equal(out_list$Slope, out$Slope, tolerance = 1e-10)

  footer_lines <- function(out) {
    grep("^Predictors modulated", insight::trim_ws(utils::capture.output(print(out))), value = TRUE)
  }
  by_forms <- list(
    formula = ~ Infl + Sat,
    list = list(Infl = c("Low", "High"), Sat = "Low"),
    data_frame = data.frame(Infl = "Low", Sat = "Low")
  )
  for (form in names(by_forms)) {
    out <- estimate_means(m, by_forms[[form]], backend = "emmeans")
    expect_identical(footer_lines(out), "Predictors modulated: Infl", info = form)
  }

  expect_error(
    estimate_means(m, list(Infl = "Low", Sat = "Bogus"), backend = "emmeans"),
    "The response `Sat` has no category `Bogus`.",
    fixed = TRUE
  )
  expect_error(
    estimate_slopes(
      m2,
      "x",
      by = data.frame(Sat = "Low"),
      predict = "cum.prob",
      backend = "emmeans"
    ),
    "The response `Sat` can only be used in `by`",
    fixed = TRUE
  )
})


test_that("estimate_contrasts, backend emmeans, ordinal, contrast filter on the response", {
  skip_if_not_installed("emmeans")
  data(housing, package = "MASS")
  m <- MASS::polr(Sat ~ Infl + Type, weights = Freq, data = housing, Hess = TRUE)
  out_all <- estimate_contrasts(m, "Sat", by = "Infl", backend = "emmeans")

  out <- estimate_contrasts(m, "Sat=c('Low','High')", by = "Infl", backend = "emmeans")
  expected <- out_all[out_all$Level1 == "High" & out_all$Level2 == "Low", ]
  expect_identical(as.character(out$Infl), as.character(expected$Infl))
  expect_equal(out$Difference, expected$Difference, tolerance = 1e-10)

  # comparisons that are not pairwise use only the named categories
  out <- estimate_contrasts(
    m,
    "Sat=c('Low','Medium')",
    by = "Infl",
    comparison = "poly",
    backend = "emmeans"
  )
  expected <- out_all[out_all$Level1 == "Medium" & out_all$Level2 == "Low", ]
  expect_equal(out$Difference, expected$Difference, tolerance = 1e-10)

  expect_error(
    estimate_contrasts(m, "Sat='Low'", by = "Infl", backend = "emmeans"),
    "need at least two response categories",
    fixed = TRUE
  )
  expect_error(
    estimate_contrasts(
      m,
      "Sat=c('Low','High')",
      by = "Infl",
      predict = "cum.prob",
      backend = "emmeans"
    ),
    "can only be used in `contrast`",
    fixed = TRUE
  )
  expect_error(
    estimate_contrasts(m, "Sat=c('Low','High')", by = "Sat='Low'", backend = "emmeans"),
    "cannot be filtered in `by`",
    fixed = TRUE
  )

  out <- estimate_contrasts(
    m,
    "Sat=c('Low','High')",
    by = "Infl",
    comparison = "joint",
    backend = "emmeans"
  )
  estimated <- emmeans::emmeans(m, specs = c("Infl", "Sat"), mode = "prob")
  estimated <- emmeans::regrid(estimated[which(estimated@grid$Sat %in% c("Low", "High"))])
  expected <- as.data.frame(emmeans::test(
    emmeans::contrast(estimated, by = "Infl", method = "pairwise"),
    joint = TRUE,
    by = "Infl"
  ))
  expect_equal(out$Chisq, expected$Chisq, tolerance = 1e-6)
})


test_that("estimate_contrasts, backend emmeans, ordinal, contrast filter on the response of clmm", {
  skip_if_not_installed("emmeans")
  skip_if_not_installed("ordinal")
  data(wine, package = "ordinal")
  levels(wine$rating) <- letters[1:5]
  m <- ordinal::clmm(rating ~ temp + contact + (1 | judge), data = wine)
  out_all <- estimate_contrasts(m, "rating", by = "temp", backend = "emmeans")
  out <- estimate_contrasts(m, "rating=c('a','c')", by = "temp", backend = "emmeans")
  # emmeans codes the categories of clmm models as 1, 2, 3, ...
  expected <- out_all[out_all$Level1 == "rating3" & out_all$Level2 == "rating1", ]
  expect_equal(out$Difference, expected$Difference, tolerance = 1e-10)
})


# the rows of `out` must be the rows of `out_all` with the same values in the
# `keys` columns, with equal numeric columns
expect_matching_rows <- function(out, out_all, keys, columns, info = NULL) {
  key <- function(d) do.call(paste, c(lapply(d[keys], as.character), sep = "\r"))
  expect_gt(nrow(out), 0)
  matched <- match(key(out), key(out_all))
  expect_false(anyNA(matched), info = info)
  for (column in columns) {
    expect_equal(
      out[[column]],
      out_all[[column]][matched],
      tolerance = 1e-10,
      info = paste(info, column)
    )
  }
}


test_that("estimate_contrasts, backend emmeans, ordinal, slopes with a by filter that keeps one level", {
  skip_if_not_installed("emmeans")
  m2 <- ordinal_filter_slope_model()

  # slopes are contrasted within each response category, so one level of
  # `Infl` leaves nothing to compare
  err <- expect_error(estimate_contrasts(m2, "x", by = "Infl='Low'", backend = "emmeans"))
  expect_match(
    gsub("\\s+", " ", conditionMessage(err)),
    "The filter in `by` leaves fewer than two slopes to compare",
    fixed = TRUE
  )

  out_all <- estimate_contrasts(m2, "x", by = "Infl", backend = "emmeans")
  out <- estimate_contrasts(m2, "x", by = "Infl=c('Low','High')", backend = "emmeans")
  expect_identical(nrow(out), 3L)
  expect_identical(as.character(out$Level1), rep("High", 3))
  expect_identical(as.character(out$Level2), rep("Low", 3))
  expect_identical(as.character(out$Response), c("Low", "Medium", "High"))
  expect_matching_rows(
    out,
    out_all,
    c("Level1", "Level2", "Response"),
    c("Difference", "SE")
  )
})
