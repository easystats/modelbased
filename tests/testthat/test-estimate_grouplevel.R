if (require("testthat") && require("modelbased") && require("lme4")) {
  test_that("estimate_grouplevel - lme4", {
    set.seed(333)
    data <- lme4::sleepstudy

    # Random intercept
    model <- lmer(Reaction ~ Days + (1 | Subject), data = data)
    random <- estimate_grouplevel(model)
    expect_equal(nrow(random), length(unique(data$Subject)))
    expect_equal(nrow(reshape_grouplevel(random)), nrow(data))

    # 2 random intercepts
    model <- lmer(mpg ~ wt + (1 | gear) + (1 | carb), data = mtcars)
    random <- estimate_grouplevel(model)
    expect_equal(nrow(random), length(c(unique(mtcars$gear), unique(mtcars$carb))))
    expect_equal(nrow(reshape_grouplevel(random)), nrow(mtcars))

    # Random slope and intercept
    model <- lmer(Reaction ~ Days + (1 + Days | Subject), data = data)
    random <- estimate_grouplevel(model)
    expect_equal(nrow(random), 2 * length(unique(data$Subject)))
    expect_equal(nrow(reshape_grouplevel(random)), nrow(data))

    # Nested random factors
    set.seed(33)
    data$grp <- sample(letters[1:5], size = 180, replace = TRUE)
    data$subgrp <- NA
    for (i in letters[1:5]) {
      filter_group <- data$grp == i
      data$subgrp[filter_group] <- sample(LETTERS, size = sum(filter_group), replace = TRUE)
    }
    model <- lmer(Reaction ~ Days + (1 | grp / subgrp) + (1 | Subject), data = data)
    random <- estimate_grouplevel(model)
    expect_equal(nrow(random), sum(sapply(coef(model), nrow)))

    reshaped <- reshape_grouplevel(random)
    expect_equal(nrow(reshaped), nrow(data))
    ref <- insight::get_data(model)[insight::find_random(model, split_nested = TRUE, flatten = TRUE)]
    all(reshaped$Subject == ref$Subject)
    all(reshaped$grp == ref$grp)
    all(reshaped$subgrp == ref$subgrp)
  })
}


# if (require("testthat") && require("modelbased") && require("brms")) {
#   test_that("estimate_grouplevel - brms", {
#
#     skip_on_cran()
#     set.seed(333)
#
#     # Reaction ~ Days + (1 + Days | Subject)
#     model <- insight::download_model("brms_mixed_2")
#     random <- estimate_grouplevel(model)
#     head(as.data.frame(random))
#
#   })
# }
