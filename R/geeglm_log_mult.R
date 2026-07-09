#' geeglm_log_mult: run multiple logistic GEE regressions and output in table
#'
#' @inheritParams construct_formulas
#' @inheritParams geeglm_log_mult_f2m
#' @inheritParams geeglm_log_mult_m2p
#' @param data data frame in long format with a row for every observation of the outcome variables. See [MS_trial_data] for an example dataset.
#'
#' @returns Dataframe with results from multiple logistic GEE regression analyses. This is a wrapper around calling [construct_formulas()], [geeglm_log_mult_f2m()] and [geeglm_log_mult_m2p()], sequentially.
#' @export
#'
#' @examples
#'
#' geeglm_log_mult(
#' data = MS_trial_data,
#' outcomes = "INO",
#' predictors = c("intervention", "intervention * time"),
#' covariates = c("", "+ gender + age"),
#' id = "pat_id"
#' )
#'
geeglm_log_mult <- function(data, outcomes, predictors, covariates="", formulas = NULL, id, corstr = "exchangeable", exponentiate = TRUE, progress = FALSE) {
  #create data frame with regression formulas
  df_formulas <- construct_formulas(outcomes = outcomes, predictors = predictors, covariates = covariates, formulas = formulas, randoms = "")

  # run logistic GEE models for each formula
  df_reg <- geeglm_log_mult_f2m(df_formulas, data = data, id = id, corstr = corstr, progress = progress)

  # extract parameters from models
  geeglm_log_mult_m2p(df_reg, exponentiate = exponentiate, progress = progress)
}
