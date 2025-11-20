#' geeglm_log_mult: run multiple logistic GEE regressions and output in table
#'
#' @param data data frame in long format with a row for every observation of the outcome variables. See [MS_trial_data] for an example dataset.
#' @param outcomes a vector with your outcome variables (dependent variables), where the names of your variables are in quotes. For example: c(‘outcome1’, ‘outcome2’, ‘outcome3’).
#' @param predictors A vector containing your predictors (independent variables), where the names of your variables are in quotes. For example: c(‘predictor1’, ‘predictor2’, ‘predictor3’). It is possible to directly recode binary or categorical variables and get an output for both encodings, e.g. c(‘predictor1’, ‘fct_rev(predictor1)’). It is also possible to specify interaction terms, e.g. c(‘predictor1 * predictor2’, ‘predictor2 * predictor 3’).
#' @param covariates A vector containing your covariates (variables you want to correct for), where the entire enumeration of covariates per model are in quotes and different covariates in the same model are separated by a plus. Also, covariates should be preceded by a plus. For example: c(‘+ covariate 1 + covariate 2’, ‘+ covariate2 + covariate3’). If you want to run a model with and without covariates then specify it like this: c(‘’, ‘+ covariate 1 + covariate 2’)
#' @param id Variable used to identify each participant, e.g. pat_id.
#' @param corstr The correlation structure for your GEE analysis. Default is "echangeable". See ?geeglm [geepack::geeglm()] for more information.
#' @param exponentiate Do you want to exponentiate the output (e^x) so that you get the odds ratio? Default is TRUE.
#'
#' @returns Dataframe with results from multiple logistic GEE regression analyses.
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
geeglm_log_mult <- function(data, outcomes, predictors, covariates="", id, corstr = "exchangeable", exponentiate = TRUE) {
  #create data frame with regression formulas
  tidyr::expand_grid(
    outcome = outcomes,
    predictor = predictors,
    covariate = covariates
  ) %>%
    dplyr::mutate(formula=paste0(paste(outcome, predictor, sep = "~"), covariates)) %>%

    # run a lineair model and iterate through each formula
    dplyr::mutate(
      model = purrr::map(formula, .f = ~geepack::geeglm(formula = as.formula(.x), data = data, id = eval(as.symbol(id)), family = binomial, corstr = corstr)) %>%
        purrr::map(.f = ~broom::tidy(.x, exponentiate = exponentiate, conf.int = TRUE))
    ) %>%

    # unnest each model so all models form a single table
    tidyr::unnest(col = model) %>%

    # round all numeric columns
    dplyr::mutate(
      dplyr::across(dplyr::where(is.numeric), ~round(.x, digits = 5)),
      model = "Logistic GEE"
    ) %>%
    dplyr::relocate(model)
}
