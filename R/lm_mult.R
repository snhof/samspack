#' lm_mult: Run multiple linear regression models and output table
#'
#'These functions allow you to specify multiple outcome measures, predictors and covariates.
#'The function then creates regression formulas from all combinations of these variables.
#'The output is a large table with results from all regression formulas.
#'This is a wrapper around calling [construct_formulas()], [lm_mult_f2m()] and [lm_mult_m2p()], sequentially.
#'
#' @inheritParams construct_formulas
#' @inheritParams lm_mult_f2m
#' @inheritParams lm_mult_m2p
#' @param std_beta Do you also want standardized betas in the output, set this to TRUE.
#'
#' @returns Large table with results from all regression formulas
#' @export
#'
#' @examples
#' lm_mult(
#' data = MS_trial_data,
#' outcomes = c("EDSS", "SDMT"),
#' predictors = "age",
#' covariates = c("", " + gender")
#' )
#'
lm_mult <- function(data, outcomes, predictors, covariates="", formulas = NULL, std_beta = FALSE, exp_log = FALSE, progress = FALSE, quiet = FALSE) {
  #create data frame with regression formulas
  df_formulas <- construct_formulas(outcomes = outcomes, predictors = predictors, covariates = covariates, formulas=formulas, randoms = "")

  # run a lineair model and iterate through each formula
  df_reg <- lm_mult_f2m(df_formulas, data = data, std_models = std_beta, progress = progress, quiet=quiet)

 # Extract parameters from models
  lm_mult_m2p(df_reg, exp_log = exp_log, progress = progress)
}

utils::globalVariables(c(
  "conf.high", "conf.low", "covariate", "estimate",
  "formula", "model", "model_error", "model_result", "outcome", "predictor",
  "std_vars", "std_data", "std_model", "term"
  ))
