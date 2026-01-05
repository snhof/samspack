#'lmer_mult: Run multiple linear mixed effects models and output table
#'
#'Run multiple linear mixed effect models with the lme4 and lmerTest packages.
#'
#'"std_models" to obtain standardized regression coefficients may be implemented in the future, but is currently not available.
#'
#' @inheritParams lm_mult_f2m
#' @param data data frame with outcomes, predictors, covariates and variables for random effects in long format with a row for every observation of the outcome variables. See [MS_trial_data] for an example dataset.
#' @param REML logical scalar - Should the estimates be chosen to optimize the REML criterion (as opposed to the log-likelihood)?
#' @param control This gives you access to the ‘control’ input of the [lme4::lmer()] function and [lme4::lmerControl()]. This allows you to bypass certain errors. For example, in case of error about number of observations, you can try control = lmerControl(check.nobs.vs.nRE = ‘ignore’).
#'
#' @returns Dataframe with linear mixed effects models appended as a list-column in the "model" column and error messages in the "model_error" column.
#' @export
#'
#' @examples
#' df_formulas <- construct_formulas(
#'  outcomes = c("EDSS", "SDMT"),
#'  predictors = c("intervention", "intervention * time"),
#'  covariates = c("", "+ age + gender"),
#'  randoms = c("+ (1|pat_id)")
#'  )
#'
#'  lmer_mult_f2m(df_formulas, data = MS_trial_data)
#'
lmer_mult_f2m <- function(df_formulas, data, REML = FALSE, control = lme4::lmerControl(), progress = FALSE, quiet = FALSE) {

  df_formulas %>%
    # run a lineair model and iterate through each formula
    dplyr::mutate(
      model = purrr::map(formula,
                         .f = purrr::safely(
                           ~lmerTest::lmer(formula = as.formula(.x), data = data, REML = REML, control = control),
                           otherwise = NA,
                           quiet = quiet
                         ),
                         .progress = ifelse(progress == TRUE, "Running regressions", FALSE)
      )) %>%
    tidyr::unnest_wider(col=model, strict = TRUE, names_sep = "_") %>%
    dplyr::rename(model = model_result) %>%
    dplyr::mutate(
      #Extract error messages and put in data frame
      model_error = purrr::map(model_error, purrr::possibly(.f = ~conditionMessage(.x)))
    ) %>%
    # unnest model_error while keeping nested
    tidyr::unnest(col = model_error, keep_empty = TRUE) -> df_reg

  return(df_reg)
}
