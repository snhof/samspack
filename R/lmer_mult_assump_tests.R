#' lmer_mult_assump_tests: Generate plots for checking linear mixed model assumptions for multiple models
#'
#' Runs multiple linear regression models, similar to [lmer_mult()], and passes each model to [lmer_assump_tests()] to generate plots for checking normality of residuals and linearity.
#' Plots for homoscedasticity currently not available.
#'
#' @inheritParams lmer_mult
#'
#' @returns A list of plots for checking linear mixed model assumptions for each model.
#' @export
#'
#' @examples
#'
#' lmer_mult_assump_tests(
#'  data = MS_trial_data,
#'  outcomes = c("EDSS", "SDMT"),
#'  predictors = c("intervention", "intervention * time"),
#'  covariates = c("", "+ age + gender"),
#'  randoms = c("+ (1|pat_id)")
#'  )
#'
lmer_mult_assump_tests <- function(data, outcomes = NULL, predictors=NULL, covariates="", randoms = NULL, formula=NULL, control = lme4::lmerControl()) {

  if(!is.null(outcomes) & !is.null(predictors)){
    #create data frame with regression formulas
    dplyr::bind_rows(
      tidyr::expand_grid(
        outcome = outcomes,
        predictor = predictors,
        covariate = covariates,
        random = randoms
      ) %>%
        dplyr::mutate(formula = paste0(paste(outcome, predictor, sep = "~"), covariate, random)),

      # add regressions defined by formula
      dplyr::tibble(formula = formula)
    ) -> df

  }  else {
    # Only include regression defined by formula
    dplyr::tibble(formula = formula) -> df
  }

  df %>%
    # run a lineair model and iterate through each formula
    dplyr::mutate(
      model = purrr::map(formula, .f = purrr::possibly(~lmerTest::lmer(formula = as.formula(.x), data = data, REML = FALSE, control = control), otherwise = NA, quiet = FALSE)),
      plots = purrr::map(.x = model, .f = ~lmer_assump_tests(.x))
    ) %>% dplyr::pull(plots)

}
