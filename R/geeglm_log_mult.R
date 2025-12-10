#' geeglm_log_mult: run multiple logistic GEE regressions and output in table
#'
#' @inheritParams construct_formulas
#' @inheritParams glm_log_mult
#' @param data data frame in long format with a row for every observation of the outcome variables. See [MS_trial_data] for an example dataset.
#'
#' @param id Variable used to identify each participant, e.g. pat_id.
#' @param corstr The correlation structure for your GEE analysis. Default is "echangeable". See ?geeglm [geepack::geeglm()] for more information.
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
geeglm_log_mult <- function(data, outcomes, predictors, covariates="", formulas = NULL, id, corstr = "exchangeable", exponentiate = TRUE, progress = FALSE) {
  #create data frame with regression formulas
  df_formulas <- construct_formulas(outcomes = outcomes, predictors = predictors, covariates = covariates, formulas = formulas, randoms = "")

  df_formulas %>%
    dplyr::mutate(formula=paste0(paste(outcome, predictor, sep = "~"), covariates)) %>%

    # run a lineair model and iterate through each formula
    dplyr::mutate(
      model = purrr::map(
        formula,
        .f = ~geepack::geeglm(formula = as.formula(.x), data = data, id = eval(as.symbol(id)), family = binomial, corstr = corstr),
        .progress = ifelse(progress, "Running logistic GEE regressions", FALSE)
      ) %>%
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
