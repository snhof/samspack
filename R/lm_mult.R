#' lm_mult: Run multiple linear regression models and output table
#'
#'These functions allow you to specify multiple outcome measures, predictors and covariates.
#'The function then creates regression formulas from all combinations of these variables.
#'The output is a large table with results from all regression formulas.
#'
#' @param data data frame with all your data.
#' @param outcomes a vector with your outcome variables (dependent variables), where the names of your variables are in quotes. For example: c(‘outcome1’, ‘outcome2’, ‘outcome3’).
#' @param predictors A vector containing your predictors (independent variables), where the names of your variables are in quotes. For example: c(‘predictor1’, ‘predictor2’, ‘predictor3’). It is possible to directly recode binary or categorical variables and get an output for both encodings, e.g. c(‘predictor1’, ‘fct_rev(predictor1)’). It is also possible to specify interaction terms, e.g. c(‘predictor1 * predictor2’, ‘predictor2 * predictor 3’).
#' @param covariates a vector containing your covariates (variables you want to correct for), where the entire enumeration of covariates per model are in quotes and different covariates in the same model are separated by a plus. Also, covariates should be preceded by a plus. For example: c(‘+ covariate 1 + covariate 2’, ‘+ covariate2 + covariate3’). If you want to run a model with and without covariates then specify it like this: c(‘’, ‘+ covariate 1 + covariate 2’)
#' @param std_beta Do you also want standardized betas in the output, set this to TRUE.
#' @param exp_log Do you want to exponentiate the results for log transformed outcomes, set this to TRUE.
#' @param show_progress Do you want to see the progress of the function, set this to TRUE.
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
lm_mult <- function(data, outcomes, predictors, covariates="", std_beta = FALSE, exp_log = FALSE, show_progress = FALSE) {
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
      model = purrr::map(
        formula,
        .f = purrr::safely(~stats::lm(formula = as.formula(.x), data = data), otherwise = NA, quiet = FALSE),
        .progress = ifelse(show_progress, "Running regressions", FALSE)
      )
    ) %>% tidyr::unnest_wider(col=model, strict = TRUE, names_sep = "_") %>%
    dplyr::mutate(
      #Extract errror messages and put in data frame
      model_error = purrr::map(model_error, purrr::possibly(.f = ~conditionMessage(.x))),
      #Put regression results into tidy data frame
      model_result = purrr::map(model_result, .f = purrr::possibly(~broom::tidy(.x, conf.int = TRUE)), .progress = ifelse(show_progress, "Making output pretty", FALSE))
    ) %>%

    # unnest each model so all models form a single table
    tidyr::unnest(col = c(model_result, model_error)) -> df_reg

  if(std_beta == TRUE){
    # generate additional dataframe with standardized regressions and append to normal regressions
    df_reg %>% dplyr::left_join(
      df %>%
        dplyr::mutate(
          # extract variables used in regression formula
          std_vars = stringr::str_extract_all(formula, "([^ \\(\\)\\*\\+~\\|]+)"),
          #filter dataset for used variables, drop rows containing NA and standardize all numerical variables. Be careful with numeric subject numbers, these will also be standardized if not defined as factor.
          std_data = purrr::map(std_vars, .f = purrr::possibly( ~ data %>% dplyr::select(dplyr::any_of(.x)) %>% na.omit() %>% dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~scale(.x)))), otherwise = NA),
          #run standardized regressions and put into tidy data frames
          std_model = purrr::map2(.x = formula, .y = std_data,
                           .f = purrr::possibly(~stats::lm(formula = as.formula(.x), data =  .y), otherwise = NA, quiet = FALSE),
                           .progress = ifelse(show_progress, "Running standardized regressions", FALSE)) %>%
            purrr::map(purrr::possibly(~tidy(.x, conf.int = TRUE)))
          # unnest each model so all models form a single table
        ) %>% tidyr::unnest(std_model) %>%
        #Only select estimate and confidence interval en give unique name
        dplyr::select(outcome, predictor, covariate, formula, term, estimate, conf.low, conf.high) %>% dplyr::rename_with(~paste0("std_", .x), .cols = c(estimate, conf.low, conf.high)) %>%
        dplyr::filter(term != "(Intercept)"),
      by = dplyr::join_by(outcome, predictor, covariate, formula, term)
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
