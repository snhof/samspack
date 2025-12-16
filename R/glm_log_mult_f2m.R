#' glm_log_mult_f2m: Formula dataframe to dataframe with logistic regression models
#'
#'  Run and append logistic regression models based on a formula dataframe created with [construct_formulas()].
#'  The output can be used as input to [glm_log_mult_m2p()] to extract parameters in a tidy format.
#'
#' @inheritParams lm_mult_f2m
#' @param std_models Do you want to run standardized versions of the models as well? This allows for standardized log(odds). Default is FALSE. For logistic regression only predictors and covariates are standardized as outcome is already on the same scale (OR). Be careful with interpretation.
#'
#' @returns Dataframe provided as "df_formulas" with appended columns containing logistic regression models and error messages.
#' @export
#'
#' @examples
#'
#' df_formulas <- construct_formulas(
#' outcomes = "INO",
#' predictors = c("intervention", "age"),
#' covariates = c("", "+ gender")
#' )
#'
#' glm_log_mult_f2m(df_formulas, data = MS_trial_data)
#'
glm_log_mult_f2m <- function(df_formulas, data, std_models = FALSE, progress = FALSE, quiet = FALSE) {

  # run a lineair model for each formula
  df_formulas %>%
    dplyr::mutate(
      model = purrr::map(
        formula,
        .f = purrr::safely(~glm(formula = as.formula(.x), data = data, family = binomial), otherwise = NA, quiet = quiet),
        .progress = ifelse(progress, "Running logistic regressions", FALSE)
      )
    ) %>% tidyr::unnest_wider(col=model, strict = TRUE, names_sep = "_") %>%
    dplyr::rename(model = model_result) %>%
    dplyr::mutate(
      #Extract error messages and put in data frame
      model_error = purrr::map(model_error, purrr::possibly(.f = ~conditionMessage(.x)))
    ) %>%
    # unnest model_error while keeping nested
    tidyr::unnest(col = model_error, keep_empty = TRUE) -> df_reg

  #For logistic regression only predictors and covariates are standardized as outcome is already on the same scale (OR). Be careful with interpretation.
  if(std_models == TRUE){
    # generate additional dataframe with standardized regressions and append to normal regressions
    df_reg %>% dplyr::left_join(
      df_reg %>%
        dplyr::mutate(
          # extract variables used in regression formula
          std_vars = formula %>% stringr::str_extract("(?<=~).*") %>% stringr::str_extract_all("\\b[[:alnum:]_]+\\b"),

          #filter dataset for used variables, drop rows containing NA and standardize all numerical variables. Be careful with numeric subject numbers, these will also be standardized if not defined as factor.
          std_data = purrr::map(std_vars, .f =  ~ data %>% dplyr::select(dplyr::any_of(c(outcome, .x))) %>% na.omit() %>% dplyr::mutate(dplyr::across(dplyr::any_of(.x) & dplyr::where(is.numeric), ~scale(.x)))),
          #run standardized regressions and put into tidy data frames
          std_model = purrr::map2(.x = formula, .y = std_data,
                                  .f = purrr::possibly(~glm(formula = as.formula(.x), data = .y, family = binomial), otherwise = NA, quiet = FALSE),
                                  .progress = ifelse(progress, "Running logistic regression models with standardized predictors", FALSE)),
          # unnest each model so all models form a single table
        ) %>% dplyr::select(-c(std_vars, std_data))
    ) %>% dplyr::relocate(std_model, .after = model) -> df_reg
  }

  return(df_reg)
}

utils::globalVariables(c(
  "model_confint", "std_confint"
))
