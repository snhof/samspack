#'lmer_mult_m2p: Extract parameters from multiple linear mixed effect models and output table
#'
#' Extract parameters from dataframe with multiple linear mixed models (i.e. [lmerTest::lmer()]) generated with [lmer_mult_f2m()].
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
#' @inheritParams lm_mult_m2p
#' @param df_reg Dataframe created with [lmer_mult_f2m()] containing the lmer models to be processed in the column "model"
#' @returns Large table with results from all regression formulas
#'
#' @export
#'
#' @examples
#'
#' df_formulas <- construct_formulas(
#'   outcomes = c("EDSS", "SDMT"),
#'   predictors = c("intervention", "intervention * time"),
#'   covariates = c("", "+ age + gender"),
#'   randoms = c("+ (1|pat_id)")
#' )
#'
#' df_reg <- lmer_mult_f2m(df_formulas, data = MS_trial_data)
#'
#' lmer_mult_m2p(df_reg)

lmer_mult_m2p <- function(df_reg, exp_log = FALSE, progress = FALSE) {

  df_reg %>%
    #calculating additional handy model parameters
    dplyr::mutate(
      isSingular = purrr::map(model, .f = purrr::possibly(~lme4::isSingular(.x))),
      logLik = purrr::map(model, .f = purrr::possibly(~stats::logLik(.x) %>% as.numeric() %>%round(digits = 1))),
      deviance = purrr::map(model, .f = purrr::possibly(~-2*stats::logLik(.x) %>% as.numeric() %>% round(digits = 1))),
      ranova = purrr::map(model, .f = purrr::possibly(~lmerTest::ranova(.x)), .progress = ifelse(progress, "Doing LRT test", FALSE)),
      term_ranova = purrr::map(ranova, .f = purrr::possibly(~.x %>% attr("formulae") %>% names() %>% dplyr::nth(1))),
      npar_ranova = purrr::map(ranova, .f = purrr::possibly(~.x %>% dplyr::pull(npar) %>% dplyr::nth(2))),
      p_ranova = purrr::map(ranova, .f = purrr::possibly(~.x %>% dplyr::pull(`Pr(>Chisq)`) %>% dplyr::nth(2))),
      model = purrr::map(model, .f = purrr::possibly(~broom.mixed::tidy(.x, conf.int = TRUE)), .progress = ifelse(progress, "Making output pretty", FALSE))
    ) %>% dplyr::select(-ranova) %>%

    # unnest each model so all models form a single table
    tidyr::unnest(col = c(model, model_error, isSingular, logLik, deviance, term_ranova, npar_ranova, p_ranova)) %>%
    dplyr::select(-dplyr::any_of(c('x'))) %>%

    # round all numeric columns
    dplyr::mutate(
      dplyr::across(dplyr::where(is.numeric), ~round(.x, digits = 5)),
      model = "Linear mixed model",
      dplyr::across(c(estimate, conf.low, conf.high), ~dplyr::if_else(stringr::str_detect(outcome, "log\\(") & exp_log == TRUE, exp(.x), .x))
    ) %>%
    dplyr::relocate(model)
}
