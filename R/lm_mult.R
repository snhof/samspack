#' lm_mult: Run multiple linear regression models and output table
#'
#'These functions allow you to specify multiple outcome measures, predictors and covariates.
#'The function then creates regression formulas from all combinations of these variables.
#'The output is a large table with results from all regression formulas.
#'
#' @inheritParams construct_formulas
#' @param data data frame where each row represents an observation of the outcome variable.
#' @param std_beta Do you also want standardized betas in the output, set this to TRUE.
#' @param exp_log Do you want to exponentiate the results for log transformed outcomes, set this to TRUE.
#' @param progress Whether to show a progress bar. Use TRUE to turn on a progress bar, default is FALSE.
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
lm_mult <- function(data, outcomes, predictors, covariates="", formulas = NULL, std_beta = FALSE, exp_log = FALSE, progress = FALSE) {
  #create data frame with regression formulas
  df_formulas <- construct_formulas(outcomes = outcomes, predictors = predictors, covariates = covariates, formulas=formulas, randoms = "")

  # run a lineair model and iterate through each formula
  df_formulas %>%
    dplyr::mutate(
      model = purrr::map(
        formula,
        .f = purrr::safely(~stats::lm(formula = as.formula(.x), data = data), otherwise = NA, quiet = FALSE),
        .progress = ifelse(progress, "Running regressions", FALSE)
      )
    ) %>% tidyr::unnest_wider(col=model, strict = TRUE, names_sep = "_") %>%
    dplyr::mutate(
      #Extract errror messages and put in data frame
      model_error = purrr::map(model_error, purrr::possibly(.f = ~conditionMessage(.x))),
      #Put regression results into tidy data frame
      model_result = purrr::map(model_result, .f = purrr::possibly(~broom::tidy(.x, conf.int = TRUE)), .progress = ifelse(progress, "Making output pretty", FALSE))
    ) %>%

    # unnest each model so all models form a single table
    tidyr::unnest(col = c(model_result, model_error)) -> df_reg

  if(std_beta == TRUE){
    # generate additional dataframe with standardized regressions and append to normal regressions
    df_reg %>% dplyr::left_join(
      df_formulas %>%
        dplyr::mutate(
          # extract variables used in regression formula
          std_vars = stringr::str_extract_all(formula, "([^ \\(\\)\\*\\+~\\|]+)"),
          #filter dataset for used variables, drop rows containing NA and standardize all numerical variables. Be careful with numeric subject numbers, these will also be standardized if not defined as factor.
          std_data = purrr::map(std_vars, .f = purrr::possibly( ~ data %>% dplyr::select(dplyr::any_of(.x)) %>% na.omit() %>% dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~scale(.x)))), otherwise = NA),
          #run standardized regressions and put into tidy data frames
          std_model = purrr::map2(.x = formula, .y = std_data,
                           .f = purrr::possibly(~stats::lm(formula = as.formula(.x), data =  .y), otherwise = NA, quiet = FALSE),
                           .progress = ifelse(progress, "Running standardized regressions", FALSE)) %>%
            purrr::map(purrr::possibly(~broom::tidy(.x, conf.int = TRUE)))
          # unnest each model so all models form a single table
        ) %>% tidyr::unnest(std_model) %>%
        #Only select estimate and confidence interval en give unique name
        dplyr::select(dplyr::any_of(c("outcome", "predictor", "covariate", "formula", "term", "estimate", "conf.low", "conf.high"))) %>% dplyr::rename_with(~paste0("std_", .x), .cols = c(estimate, conf.low, conf.high)) %>%
        dplyr::filter(term != "(Intercept)"),
      by = c("term", "outcome", "predictor", "formula", df_formulas %>% dplyr::select(dplyr::any_of(c("covariate"))) %>% names())
    ) -> df_reg
  }

  df_reg %>%
    dplyr::mutate(
      # round all numeric columns
      dplyr::across(dplyr::where(is.numeric), ~round(.x, digits = 5)),
      #define used statistical method
      model = "Linear regression",
      #exponentiate for log transformed outcomes
      dplyr::across(c(estimate, conf.low, conf.high), ~dplyr::if_else(stringr::str_detect(outcome, "log\\(") & exp_log == TRUE, exp(.x), .x))
    ) %>%
    dplyr::relocate(model)
}

utils::globalVariables(c(
  "conf.high", "conf.low", "covariate", "estimate",
  "formula", "model", "model_error", "model_result", "outcome", "predictor",
  "std_vars", "std_data", "std_model", "term"
  ))
