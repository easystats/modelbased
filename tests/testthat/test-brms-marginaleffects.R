skip_on_cran()
skip_if_not_installed("curl")
skip_if_offline()
skip_if_not_installed("brms")
skip_if_not_installed("BH")
skip_if_not_installed("RcppEigen")
skip_if_not_installed("marginaleffects")
skip_if_not_installed("httr2")
skip_if_not_installed("withr")


withr::with_options(
  list(modelbased_backend = "marginaleffects"),
  test_that("estimate_means - brms, categorical family", {
    data(mtcars)
    m <- insight::download_model("brms_categorical_1_num")
    skip_if(is.null(m))
    out <- estimate_means(m, "mpg = [fivenum]", backend = "marginaleffects")
    expect_named(
      out,
      c(
        "mpg", "ROPE_CI", "Response", "Probability", "CI_low", "CI_high",
        "pd", "ROPE_low", "ROPE_high", "ROPE_Percentage"
      )
    )
    expect_equal(
      out$Probability,
      c(
        0.97802, 0.83323, 0.45822, 0.12396, 0.00039, 0.00522, 0.06985,
        0.32089, 0.62049, 0.92419, 0.01218, 0.07694, 0.201, 0.23148,
        0.07274
      ),
      tolerance = 1e-4
    )
  })
)


withr::with_environment(
  new.env(),
  test_that("estimate_means - brms", {
    data(efc, package = "modelbased")
    efc <- datawizard::to_factor(efc, c("c161sex", "c172code", "e16sex", "e42dep"))
    levels(efc$c172code) <- c("low", "mid", "high")
    efc <<- efc

    m <- suppressWarnings(insight::download_model("brms_linear_1"))
    skip_if(is.null(m))


    # categorical -------------------------------------------------------------

    out <- estimate_means(m, "e42dep")
    expect_named(
      out,
      c(
        "e42dep", "ROPE_CI", "Median", "CI_low", "CI_high", "pd", "ROPE_low",
        "ROPE_high", "ROPE_Percentage"
      )
    )
    expect_equal(out$Median, c(91.17918, 83.40515, 71.75299, 34.28989), tolerance = 1e-4)

    out <- estimate_means(m, "e42dep", test = "ps")
    expect_named(out, c("e42dep", "Median", "CI_low", "CI_high", "ps"))
    expect_equal(out$Median, c(91.17918, 83.40515, 71.75299, 34.28989), tolerance = 1e-4)

    out <- estimate_means(m, "e42dep", test = "ps", centrality = "MAP")
    expect_named(out, c("e42dep", "MAP", "CI_low", "CI_high", "ps"))
    expect_equal(out$MAP, c(91.45967, 83.39408, 71.70011, 34.34934), tolerance = 1e-4)


    # two categorical -------------------------------------------------------------

    out <- estimate_means(m, c("e42dep", "e16sex"))
    expect_named(
      out,
      c(
        "e42dep", "e16sex", "ROPE_CI", "Median", "CI_low", "CI_high",
        "pd", "ROPE_low", "ROPE_high", "ROPE_Percentage"
      )
    )
    expect_equal(
      out$Median,
      c(90.62458, 91.7056, 82.8699, 83.93685, 71.22175, 72.29426, 33.74165, 34.80376),
      tolerance = 1e-4
    )

    out <- estimate_means(m, c("e42dep", "e16sex"), test = "ps")
    expect_named(out, c("e42dep", "e16sex", "Median", "CI_low", "CI_high", "ps"))


    # numeric -------------------------------------------------------------

    out <- estimate_means(m, "c12hour = [fivenum]")
    expect_named(
      out,
      c(
        "c12hour", "ROPE_CI", "Median", "CI_low", "CI_high", "pd", "ROPE_low",
        "ROPE_high", "ROPE_Percentage"
      )
    )
    expect_equal(out$Median, c(72.95178, 72.50849, 71.77866, 70.14772, 61.00972), tolerance = 1e-4)

    out <- estimate_means(m, "c12hour = [fivenum]", test = "ps")
    expect_named(out, c("c12hour", "Median", "CI_low", "CI_high", "ps"))
    expect_equal(out$Median, c(72.95178, 72.50849, 71.77866, 70.14772, 61.00972), tolerance = 1e-4)


    # numeric * categorical -------------------------------------------------

    out <- estimate_means(m, c("c160age = [sd]", "c172code"))
    expect_named(
      out,
      c(
        "c160age", "c172code", "ROPE_CI", "Median", "CI_low", "CI_high", "pd",
        "ROPE_low", "ROPE_high", "ROPE_Percentage"
      )
    )
    expect_equal(
      out$Median,
      c(
        69.05882, 72.05366, 71.01566, 69.2752, 71.00937, 70.1968, 69.41956,
        69.96445, 69.35416
      ),
      tolerance = 1e-4
    )
  })
)


withr::with_environment(
  new.env(),
  test_that("estimate_slopes - brms", {
    data(efc, package = "modelbased")
    efc <- datawizard::to_factor(efc, c("c161sex", "c172code", "e16sex", "e42dep"))
    levels(efc$c172code) <- c("low", "mid", "high")
    efc <<- efc

    m <- suppressWarnings(insight::download_model("brms_linear_1"))
    skip_if(is.null(m))


    # categorical -------------------------------------------------------------

    out <- estimate_slopes(m, "e42dep")
    expect_named(
      out,
      c(
        "Comparison", "ROPE_CI", "Median", "CI_low", "CI_high", "pd", "ROPE_low",
        "ROPE_high", "ROPE_Percentage"
      )
    )
    expect_equal(out$Median, c(-19.37705, -56.85365, -7.76557), tolerance = 1e-4)
    expect_identical(
      out$Comparison,
      c(
        "moderately dependent - independent", "severely dependent - independent",
        "slightly dependent - independent"
      )
    )

    out <- estimate_slopes(m, "e42dep", test = "ps", centrality = "MAP")
    expect_named(out, c("Comparison", "MAP", "CI_low", "CI_high", "ps"))
    expect_equal(out$MAP, c(-19.21678, -56.74753, -7.69892), tolerance = 1e-4)


    # two categorical -------------------------------------------------------------

    out <- estimate_slopes(m, "e42dep", by = "e16sex")
    expect_named(
      out,
      c(
        "e16sex", "Comparison", "ROPE_CI", "Median", "CI_low", "CI_high",
        "pd", "ROPE_low", "ROPE_high", "ROPE_Percentage"
      )
    )
    expect_equal(
      out$Median,
      c(-19.37705, -56.85365, -7.76557, -19.37705, -56.85365, -7.76557),
      tolerance = 1e-4
    )
    expect_identical(as.character(out$e16sex), c("male", "male", "male", "female", "female", "female"))


    # numeric -------------------------------------------------------------

    out <- estimate_slopes(m, trend = "c12hour")
    expect_named(
      out,
      c(
        "ROPE_CI", "Median", "CI_low", "CI_high", "pd", "ROPE_low",
        "ROPE_high", "ROPE_Percentage"
      )
    )
    expect_equal(out$Median, -0.07303, tolerance = 1e-4)


    # numeric * categorical -------------------------------------------------

    out <- estimate_slopes(m, trend = "c160age", by = "c172code")
    expect_named(
      out,
      c(
        "c172code", "ROPE_CI", "Median", "CI_low", "CI_high", "pd", "ROPE_low",
        "ROPE_high", "ROPE_Percentage"
      )
    )
    expect_equal(out$Median, c(0.01271, -0.0795, -0.0629), tolerance = 1e-4)
  })
)


withr::with_environment(
  new.env(),
  test_that("estimate_contrasts - brms, logistic", {
    data(efc, package = "modelbased")
    efc <- datawizard::to_factor(efc, c("c161sex", "c172code", "e16sex", "e42dep"))
    levels(efc$c172code) <- c("low", "mid", "high")
    efc <<- efc

    m <- insight::download_model("brms_logistic_1")
    skip_if(is.null(m))


    # categorical -------------------------------------------------------------

    out <- estimate_means(m, "e42dep")
    expect_named(
      out,
      c(
        "e42dep", "ROPE_CI", "Median", "CI_low", "CI_high", "pd", "ROPE_low",
        "ROPE_high", "ROPE_Percentage"
      )
    )
    expect_equal(out$Median, c(0.11468, 0.26105, 0.42753, 0.62302), tolerance = 1e-4)
  })
)
