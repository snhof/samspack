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
lmer_mult_assump_tests <- function(data, outcomes = NULL, predictors=NULL, covariates="", randoms = NULL, formulas=NULL, control = lme4::lmerControl()) {

df_formulas <- construct_formulas(outcomes = outcomes, predictors = predictors, covariates = covariates, randoms = randoms, formulas = formulas)

  df_formulas %>%
    # run a lineair model and iterate through each formula
    dplyr::mutate(
      model = purrr::map(formula, .f = purrr::possibly(~lmerTest::lmer(formula = as.formula(.x), data = data, REML = FALSE, control = control), otherwise = NA, quiet = FALSE)),
      plots = purrr::map(.x = model, .f = ~lmer_assump_tests(.x))
    ) %>% dplyr::pull(plots)

}
