#'  glm_log_mult: run multiple logistic regressions and output in table
#'
#' @inheritParams construct_formulas
#' @inheritParams glm_log_mult_f2m
#' @inheritParams glm_log_mult_m2p
#' @param data Dataframe containing all variables for regression analyses, where each row is an observation of the outcome variable.
#' @param std_odds Do you want to standardize the log(odds)? Default is FALSE.
#'
#' @returns Dataframe with results from multiple logistic regression analyses.
#' @export
#'
#' @examples
#'
#' glm_log_mult(
#' data = MS_trial_data,
#' outcomes = "INO",
#' predictors = c("intervention", "age"),
#' covariates = c("", "+ gender")
#' )
#'
glm_log_mult <- function(data, outcomes, predictors, covariates="", formulas = NULL, exponentiate = TRUE, progress = FALSE, std_odds = FALSE, quiet = FALSE) {
  #create data frame with regression formulas
  df_formulas <- construct_formulas(outcomes = outcomes, predictors = predictors, covariates = covariates, formulas = formulas, randoms = "")

  # run a lineair model and iterate through each formula
  df_reg <- glm_log_mult_f2m(df_formulas, data = data, std_models = std_odds, progress = progress, quiet = quiet)

  #Get parameters
  glm_log_mult_m2p(df_reg, exponentiate = exponentiate, progress = progress)

}

utils::globalVariables(c(
  "model_confint", "std_confint"
))
