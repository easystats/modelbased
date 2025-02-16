#' @keywords internal
.get_marginaleffects_type_argument <- function(model,
                                               predict = NULL,
                                               comparison = NULL,
                                               model_info = NULL,
                                               ...) {
  dots <- list(...)

  # no transformation always returns link-scale
  if (identical(predict, "link")) {
    return(list(predict = "link", backtransform = FALSE, link_inverse = NULL))
  }

  # handle distributional parameters
  if (!is.null(predict) && predict %in% .brms_aux_elements()) {
    return(list(predict = predict, backtransform = FALSE, link_inverse = NULL))
  }

  # extract all valid types for model class
  valid_types <- .valid_marginaleffects_types(model)

  # check if user supplied type- or predict argument, and if it's valid
  if (!is.null(dots$type) && !dots$type %in% valid_types) {
    # if not, indicate wrong argument
    predict <- NA
    error_arg <- "type"
  } else if (!is.null(predict) && !predict %in% valid_types) {
    # if not, indicate wrong argument
    predict <- NA
    error_arg <- "predict"
  }

  if (isTRUE(is.na(predict))) {
    # add modelbased-options to valid types
    valid_types <- unique(c("response", "link", valid_types))
    insight::format_error(paste0(
      "The option provided in the `", error_arg, "` argument is not recognized.",
      " Valid options are: ",
      datawizard::text_concatenate(valid_types, enclose = "`"),
      "."
    ))
  }

  # return default type
  if (is.null(dots$type)) {
    link_inverse <- .link_inverse(model)
    if (is.null(predict)) {
      out <- valid_types[1]
    } else {
      out <- predict
    }

    # no transform if no "link" type available
    transform <- "link" %in% valid_types &&
      # no transform for linear models
      (isFALSE(model_info$is_linear) || isFALSE(model_info$is_tweedie))
      # only back-transform if "response" is requested
      out == "response" &&
      # only back-transform if we have a link-inverse function
      !is.null(link_inverse) &&
      # no back-transform for `estimate_contrasts()`
      is.null(comparison) &&
      # accurate link-inv not working for Gamma-family / inverse-link?
      !identical(model_info$link_function, "inverse") &&
      # some classes are currently broken for "link" type, see
      # https://github.com/vincentarelbundock/marginaleffects/issues/1391
      # https://github.com/vincentarelbundock/marginaleffects/issues/1392
      ## TODO: allow these classes once issues are fixed in marginaleffects
      !inherits(model, c("betareg", "brmsfit"))

    if (transform) {
      list(predict = "link", backtransform = TRUE, link_inverse = link_inverse)
    } else {
      list(predict = out, backtransform = FALSE, link_inverse = NULL)
    }
  } else {
    list(predict = dots$type, backtransform = FALSE, link_inverse = NULL)
  }
}


# return default "type" argument - this differs, depending on model class
.valid_marginaleffects_types <- function(model) {
  model_class <- class(model)[1]
  # for unrecognized model classes, return "response"
  if (!model_class %in% .typedic$class) {
    return("response")
  }
  # extract all valid types for model class
  .typedic$type[.typedic$class == model_class]
}


# all valid "type" arguments for each model class.
# Run "marginaleffects:::type_dictionary_build()" to update this list
.typedic <- data.frame(
  class = c(
    "bam", "bam", "bart", "bart", "betareg", "betareg", "betareg",
    "betareg", "betareg", "bife", "bife", "bracl", "brglmFit", "brglmFit",
    "brmsfit", "brmsfit", "brmsfit", "brmsfit", "brmultinom", "brmultinom",
    "clm", "clm", "clm", "clogit", "clogit", "clogit", "clogit",
    "coxph", "coxph", "coxph", "coxph", "coxph_weightit", "coxph_weightit",
    "coxph_weightit", "coxph_weightit", "crch", "crch", "crch", "crch",
    "hetprob", "hetprob", "hxlr", "hxlr", "hxlr", "hxlr", "ivpml",
    "ivpml", "flexsurvreg", "flexsurvreg", "flexsurvreg", "flexsurvreg",
    "flexsurvreg", "flexsurvreg", "flexsurvreg", "flexsurvreg", "flexsurvreg",
    "fixest", "fixest", "fixest", "hurdle", "hurdle", "hurdle", "hurdle",
    "iv_robust", "lm", "gam", "gam", "Gam", "Gam", "Gam", "geeglm",
    "geeglm", "Gls", "glimML", "glimML", "glm", "glm", "glm", "glmerMod",
    "glmerMod", "glmgee", "glmrob", "glmrob", "glmmTMB", "glmmTMB",
    "glmmTMB", "glmmTMB", "glmmTMB", "glmmTMB", "glmmPQL", "glmmPQL",
    "glmx", "glm_weightit", "glm_weightit", "glm_weightit", "glm_weightit",
    "glm_weightit", "ivreg", "lmerMod", "lmerModLmerTest", "lmrob",
    "lm_robust", "lrm", "lrm", "lrm", "mblogit", "mblogit", "mblogit",
    "mclogit", "mclogit", "mclogit", "MCMCglmm", "model_fit", "model_fit",
    "model_fit", "workflow", "workflow", "workflow", "multinom",
    "multinom", "multinom_weightit", "multinom_weightit", "multinom_weightit",
    "mhurdle", "mhurdle", "mhurdle", "mlogit", "mvgam", "mvgam",
    "mvgam", "mvgam", "mvgam", "negbin", "negbin", "negbin", "ols",
    "oohbchoice", "oohbchoice", "orm", "orm", "orm", "ordinal_weightit",
    "ordinal_weightit", "ordinal_weightit", "ordinal_weightit", "ordinal_weightit",
    "polr", "rendo.base", "rendo.base", "rlm", "selection", "selection",
    "selection", "speedlm", "speedglm", "speedglm", "stanreg", "stanreg",
    "survreg", "survreg", "survreg", "svyglm", "svyglm", "svyolr",
    "tobit", "tobit1", "tobit1", "tobit1", "zeroinfl", "zeroinfl",
    "zeroinfl", "zeroinfl"
  ),
  type = c(
    "response", "link", "ev", "ppd", "response", "link", "precision",
    "quantile", "variance", "response", "link", "probs", "response",
    "link", "response", "link", "prediction", "average", "probs",
    "class", "prob", "cum.prob", "linear.predictor", "expected",
    "lp", "risk", "survival", "survival", "expected", "lp", "risk",
    "survival", "expected", "lp", "risk", "response", "location",
    "scale", "density", "pr", "xb", "location", "cumprob", "scale",
    "density", "pr", "xb", "survival", "response", "mean", "link",
    "lp", "linear", "rmst", "hazard", "cumhaz", "invlink(link)",
    "response", "link", "response", "prob", "count", "zero", "response",
    "response", "response", "link", "invlink(link)", "response",
    "link", "response", "link", "lp", "response", "link", "invlink(link)",
    "response", "link", "response", "link", "response", "response",
    "link", "response", "link", "conditional", "zprob", "zlink",
    "disp", "response", "link", "response", "invlink(link)", "probs",
    "response", "lp", "link", "response", "response", "response",
    "response", "response", "fitted", "lp", "mean", "response", "latent",
    "link", "response", "latent", "link", "response", "numeric",
    "prob", "class", "numeric", "prob", "class", "probs", "latent",
    "probs", "response", "mean", "E", "Ep", "p", "response", "response",
    "link", "expected", "detection", "latent_N", "invlink(link)",
    "response", "link", "lp", "probability", "utility", "fitted",
    "mean", "lp", "probs", "response", "link", "lp", "mean", "probs",
    "response", "link", "response", "response", "link", "unconditional",
    "response", "response", "link", "response", "link", "response",
    "link", "quantile", "response", "link", "probs", "response",
    "expvalue", "linpred", "prob", "response", "prob", "count", "zero"
  ),
  stringsAsFactors = FALSE
)


# the default "type" arguments for each model class. Used to set the
# default type in "ggaverage()"
# Run following code to update this list:
# x <- marginaleffects:::type_dictionary_build()
# x[!duplicated(x$class), ]
# Finally, add "other" as first element to "class" and "response" to "type"
.default_type <- data.frame(
  class = c(
    "other",
    "bam", "bart", "betareg", "bife", "bracl",
    "brglmFit", "brmsfit", "brmultinom", "clm", "clogit", "coxph",
    "coxph_weightit", "crch", "hetprob", "hxlr", "ivpml", "flexsurvreg",
    "fixest", "hurdle", "iv_robust", "lm", "gam", "Gam", "geeglm",
    "Gls", "glimML", "glm", "glmerMod", "glmgee", "glmrob", "glmmTMB",
    "glmmPQL", "glmx", "glm_weightit", "ivreg", "lmerMod", "lmerModLmerTest",
    "lmrob", "lm_robust", "lrm", "mblogit", "mclogit", "MCMCglmm",
    "model_fit", "workflow", "multinom", "multinom_weightit", "mhurdle",
    "mlogit", "mvgam", "negbin", "ols", "oohbchoice", "orm", "ordinal_weightit",
    "polr", "rendo.base", "rlm", "selection", "speedlm", "speedglm",
    "stanreg", "survreg", "svyglm", "svyolr", "tobit", "tobit1",
    "zeroinfl"
  ),
  type = c(
    "response",
    "response", "ev", "response", "response",
    "probs", "response", "response", "probs", "prob", "expected",
    "survival", "survival", "response", "pr", "location", "pr", "survival",
    "invlink(link)", "response", "response", "response", "response",
    "invlink(link)", "response", "lp", "response", "invlink(link)",
    "response", "response", "response", "response", "response", "response",
    "invlink(link)", "response", "response", "response", "response",
    "response", "fitted", "response", "response", "response", "numeric",
    "numeric", "probs", "probs", "E", "response", "response", "invlink(link)",
    "lp", "probability", "fitted", "probs", "probs", "response",
    "response", "response", "response", "response", "response", "response",
    "response", "probs", "response", "expvalue", "response"
  ),
  stringsAsFactors = FALSE
)
