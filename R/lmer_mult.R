#'lmer_mult: Run multiple linear mixed effects models and output table
#'
#'Run multiple linear mixed effect models with the lme4 and lmerTest packages.
#'
#'The output contains some additional variables that are not in the standard mixed model output namely:
#'
#'isSingular = is TRUE if the model is ‘singular’. There is debate whether you can then use the model or not. See ?isSingular
#'
#'Loglik = the log likelihood of the model with which to compare models
#'
#'Deviance = -2 * loglik
#'
#'term_ranova = output of the ranova() function that performs the likelihood ratio test(LRT). This variable indicates for which term in the regression formula the LRT was performed.
#'
#'npar_ranova = output of the ranova() function performing the likelihood ratio test(LRT). This variable indicates the number of parameters in the model.
#'
#'p_ranova = output of the ranova() function performing the likelihood ratio test(LRT). This variable indicates the p value of the LRT.
#'
#'"std_beta" to obtain standardized regression coefficients may be implemented in the future, but is currently not available.
#'
#' @inheritParams construct_formulas
#' @inheritParams lm_mult
#' @param data data frame with outcomes, predictors, covariates and variables for random effects in long format with a row for every observation of the outcome variables. See [MS_trial_data] for an example dataset.
#' @param control This gives you access to the ‘control’ input of the [lme4::lmer()] function and [lme4::lmerControl()]. This allows you to bypass certain errors. For example, in case of error about number of observations, you can try control = lmerControl(check.nobs.vs.nRE = ‘ignore’).
#'
#' @returns Large table with results from all regression formulas
#' @export
#'
#' @examples
#'
#' lmer_mult(
#'  data = MS_trial_data,
#'  outcomes = c("EDSS", "SDMT"),
#'  predictors = c("intervention", "intervention * time"),
#'  covariates = c("", "+ age + gender"),
#'  randoms = c("+ (1|pat_id)")
#'  )
#'
lmer_mult <- function(data, outcomes = NULL, predictors=NULL, covariates="", randoms = NULL, formulas=NULL, control = lme4::lmerControl(), exp_log = FALSE, progress = FALSE) {

  #create data frame with regression formulas
  construct_formulas(outcomes = outcomes, predictors = predictors, covariates = covariates, randoms = randoms, formulas = formulas) -> df

  df %>%
    # run a lineair model and iterate through each formula
    dplyr::mutate(
      model = purrr::map(formula,
                         .f = purrr::safely(
                           ~lmerTest::lmer(formula = as.formula(.x), data = data, REML = FALSE, control = control),
                           otherwise = NA,
                           quiet = FALSE
                         ),
                         .progress = ifelse(progress == TRUE, "Running regressions", FALSE)
      )) %>% tidyr::unnest_wider(col=model, strict = TRUE, names_sep = "_") %>%

    #calculating additional handy model parameters
    dplyr::mutate(
      model_error = purrr::map(model_error, purrr::possibly(.f = ~conditionMessage(.x))),
      isSingular = purrr::map(model_result, .f = purrr::possibly(~lme4::isSingular(.x))),
      logLik = purrr::map(model_result, .f = purrr::possibly(~stats::logLik(.x) %>% as.numeric() %>%round(digits = 1))),
      deviance = purrr::map(model_result, .f = purrr::possibly(~-2*stats::logLik(.x) %>% as.numeric() %>% round(digits = 1))),
      ranova = purrr::map(model_result, .f = purrr::possibly(~lmerTest::ranova(.x)), .progress = ifelse(progress, "Doing LRT test", FALSE)),
      term_ranova = purrr::map(ranova, .f = purrr::possibly(~.x %>% attr("formulae") %>% names() %>% dplyr::nth(1))),
      npar_ranova = purrr::map(ranova, .f = purrr::possibly(~.x %>% dplyr::pull(npar) %>% dplyr::nth(2))),
      p_ranova = purrr::map(ranova, .f = purrr::possibly(~.x %>% dplyr::pull(`Pr(>Chisq)`) %>% dplyr::nth(2))),
      model_result = purrr::map(model_result, .f = purrr::possibly(~broom.mixed::tidy(.x, conf.int = TRUE)), .progress = ifelse(progress, "Making output pretty", FALSE))
    ) %>% dplyr::select(-ranova) %>%

    # unnest each model so all models form a single table
    tidyr::unnest(col = c(model_result, model_error, isSingular, logLik, deviance, term_ranova, npar_ranova, p_ranova)) %>%

    # round all numeric columns
    dplyr::mutate(
      dplyr::across(dplyr::where(is.numeric), ~round(.x, digits = 5)),
      model = "Linear mixed model",
      dplyr::across(c(estimate, conf.low, conf.high), ~dplyr::if_else(stringr::str_detect(outcome, "log\\(") & exp_log == TRUE, exp(.x), .x))
    ) %>%
    dplyr::relocate(model)
}

utils::globalVariables(c(
  "deviance", "isSingular", "logLik", "npar_ranova", "p_ranova", "random", "ranova", "term_ranova"
))
