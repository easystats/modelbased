#' @keywords internal
.get_marginaleffects_type_argument <- function(model,
                                               predict = NULL,
                                               comparison = NULL,
                                               model_info = NULL,
                                               verbose = TRUE,
                                               ...) {
  dots <- list(...)

  # no transformation always returns link-scale
  if (identical(predict, "link")) {
    return(list(predict = "link", backtransform = FALSE, link_inverse = NULL))
  }

  # handle distributional parameters
  if (!is.null(predict) && predict %in% .brms_aux_elements(model)) {
    return(list(predict = predict, backtransform = FALSE, link_inverse = NULL))
  }

  # extract all valid types and the default type for model class
  valid_types <- .valid_marginaleffects_types(model)
  default_type <- .default_marginaleffects_types(model)

  # find link-types - we need link-type when user wants bias-correction
  link_types <- c("link", "linear.predictor", "lp")
  link_type <- link_types[which(link_types %in% valid_types)[1]]
  # by default, no prediction on the link-scale with back-transformation
  inv_link <- FALSE

  # check if user supplied type- or predict argument, and if it's valid
  if (!is.null(dots$type) && !dots$type %in% valid_types) {
    # if not, indicate wrong argument
    predict <- NA
    error_arg <- "type"
  } else if (!is.null(predict) && !predict %in% c("inverse_link", valid_types)) {
    # if not, indicate wrong argument
    predict <- NA
    error_arg <- "predict"
  } else if (identical(predict, "inverse_link")) {
    # handle special type of predict-scale. we allow predictions on the
    # link-scale with manual back-transformation, similar to {marginaleffects}'
    # invlink(link) option. but only, if we have a link-type.
    if (!is.na(link_type)) {
      predict <- "response"
      inv_link <- TRUE
    } else {
      # if not, indicate wrong argument
      predict <- NA
      error_arg <- "predict"
    }
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

  # if we have bias-correction, and we are able to get predictions on the
  # link-scale, we set `predict` to "response" - only in this case,
  # a back transformation is applied
  if (isTRUE(dots$bias_correction) && !is.na(link_type)) {
    predict <- "response"
    inv_link <- TRUE
  }

  # return default type
  if (is.null(dots$type)) {
    link_inverse <- .link_inverse(model, verbose = verbose, ...)
    if (is.null(predict)) {
      out <- default_type
    } else {
      out <- predict
    }

    # usually, predictions on the response scale for each observation, and then
    # aggregated by groups, is less biased. However, this may result in confidence
    # intervals that are outside plausible bounds. In this case, we can force
    # to calculate predictions on the link-scale using `inverse_link`, and then
    # back-tranform manually. However, in particular for mixed models, this does
    # *not* accurately average across random effects. So we have to decide whether
    # we want more accurate *predictions* or more accurate *uncertainty*.

    # no transform if no "link" type available and if not explicitly requested by user
    transform <- isTRUE(inv_link) &&
      # only back-transform if we have a link-inverse function
      !is.null(link_inverse) &&
      # no back-transform for `estimate_contrasts()`
      is.null(comparison)

    if (transform) {
      list(predict = link_type, backtransform = TRUE, link_inverse = link_inverse)
    } else {
      list(predict = out, backtransform = FALSE, link_inverse = NULL)
    }
  } else {
    list(predict = dots$type, backtransform = FALSE, link_inverse = NULL)
  }
}


# return all "type" arguments
.valid_marginaleffects_types <- function(model) {
  model_class <- class(model)[1]
  # for unrecognized model classes, return "response"
  if (!model_class %in% .typedic$class) {
    return("response")
  }
  # extract all valid types for model class
  .typedic$type[.typedic$class == model_class]
}


# return default type argument for model class, as defined in marginaleffecs
# we can overwrite the default, e.g. using "inverse_link" as an option when
# not provided by marginaleffects, in the ".default_type" data frame
.default_marginaleffects_types <- function(model) {
  model_class <- class(model)[1]
  # for unrecognized model classes, return "response"
  if (!model_class %in% .default_type$class) {
    return("response")
  }
  # extract all valid types for model class
  .default_type$type[.default_type$class == model_class]
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
# default type for the marginaleffects-backend
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
