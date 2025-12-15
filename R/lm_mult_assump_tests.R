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
lm_mult_assump_tests <- function(data, outcomes, predictors, covariates="", formulas = NULL) {
  #create data frame with regression formulas
df_formulas <- construct_formulas(outcomes = outcomes, predictors = predictors, covariates = covariates, formulas = formulas, randoms = "")

df_reg <- lm_mult_f2m(df_formulas, data = data, std_models = FALSE, progress = FALSE, quiet=TRUE)

df_reg %>%
    dplyr::mutate(
      plots = purrr::map(.x = model, .f = ~lm_assump_tests(.x))
    ) %>% dplyr::pull(plots)
}

utils::globalVariables(c(
  "plots"
))
