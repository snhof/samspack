#' lm_mult_assump_tests: Generate plots for checking linear regression assumptions for multiple models
#'
#' Runs multiple linear regression models, similar to [lm_mult()], and passes each model to [lm_assump_tests()] to generate plots for checking normality of residuals, linearity, and homoscedasticity.
#'
#' @inheritParams lm_mult
#'
#' @returns A list of plots for checking linear regression assumptions for each model.
#' @export
#'
#' @examples
#'
#' lm_mult_assump_tests(
#' data = MS_trial_data,
#' outcomes = c("EDSS", "SDMT"),
#' predictors = "age",
#' covariates = c("", " + gender")
#' )
#'
lm_mult_assump_tests <- function(data, outcomes, predictors, covariates="") {
  #create data frame with regression formulas
  #Making all possible combinations of outcomes, predictors and covariates
  tidyr::expand_grid(
    outcome = outcomes,
    predictor = predictors,
    covariate = covariates
  ) %>%
    #pasting outcomes, predictors and covariates together to create formula.
    dplyr::mutate(formula=paste0(paste(outcome, predictor, sep = "~"), covariate)) -> df

  # run a lineair model and iterate through each formula
  df %>%
    dplyr::mutate(
      model = purrr::map(formula, .f = purrr::possibly(~lm(formula = as.formula(.x), data = data), otherwise = NA, quiet = FALSE)),
      plots = purrr::map(.x = model, .f = ~lm_assump_tests(.x))
    ) %>% dplyr::pull(plots)
}

utils::globalVariables(c(
  "plots"
))
