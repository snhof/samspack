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
#' @inheritParams lmer_mult_f2m
#' @inheritParams lmer_mult_m2p
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
lmer_mult <- function(data, outcomes = NULL, predictors=NULL, covariates="", randoms = NULL, formulas=NULL, REML = FALSE, control = lme4::lmerControl(), exp_log = FALSE, progress = FALSE, quiet = FALSE) {

  #create data frame with regression formulas
  df_formulas <- construct_formulas(outcomes = outcomes, predictors = predictors, covariates = covariates, randoms = randoms, formulas = formulas)

  # run a lineair model and iterate through each formula
  df_reg <- df_formulas %>% lmer_mult_f2m(data = data, REML = REML, control = control, progress = progress, quiet = quiet)

  df_reg %>% lmer_mult_m2p(exp_log = exp_log, progress = progress)
}

utils::globalVariables(c(
  "deviance", "isSingular", "logLik", "npar_ranova", "p_ranova", "random", "ranova", "term_ranova"
))
