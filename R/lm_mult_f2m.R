#' lm_mult_f2m: Formula dataframe to dataframe with linear regression models
#'
#' Run and append linear regression models based on a formula dataframe created with [construct_formulas()].
#' The output can be used as input for [lm_mult_m2p()] to extract regression parameters into a tidy dataframe.
#'
#' @param df_formulas Dataframe containing regression formulas in a column named 'formula', created with [construct_formulas()].
#' @param data dataframe where each row represents an observation of the outcome variable.
#' @param progress Whether to show a progress bar. Use TRUE to turn on a progress bar, default is FALSE.
#' @param std_models Do you want to run standardized versions of the models as well? Default is FALSE.
#' @param quiet Whether to suppress error messages from model fitting. Default is FALSE. Error messages are always captured and stored in the output dataframe.
#'
#' @returns Dataframe provided as "df_formulas" with appended columns containing linear regression models and error messages.
#' @export
#'
#' @examples
#' df_formulas <- construct_formulas(
#' outcomes = c("EDSS", "SDMT"),
#' predictors = "age",
#' covariates = c("", "+ gender")
#' )
#'
#' lm_mult_f2m(df_formulas, data = MS_trial_data)
#'
lm_mult_f2m <- function(df_formulas, data, std_models = FALSE, progress = FALSE, quiet = FALSE) {
  # run a lineair model for each formula
  df_formulas %>%
    dplyr::mutate(
      model = purrr::map(
        formula,
        .f = purrr::safely(~stats::lm(formula = as.formula(.x), data = data), otherwise = NA, quiet = quiet),
        .progress = ifelse(progress, "Running regressions", FALSE)
      )
    ) %>% tidyr::unnest_wider(col=model, strict = TRUE, names_sep = "_") %>%
    dplyr::rename(model = model_result) %>%
    dplyr::mutate(
      #Extract error messages and put in data frame
      model_error = purrr::map(model_error, purrr::possibly(.f = ~conditionMessage(.x)))
    ) %>%
    # unnest model_error while keeping nested
    tidyr::unnest(col = model_error, keep_empty = TRUE) -> df_reg

  if(std_models == TRUE){
    # generate additional dataframe with standardized regressions and append to normal regressions
    df_reg %>% dplyr::left_join(
      df_reg %>%
        dplyr::mutate(
          # extract variables used in regression formula
          std_vars = stringr::str_extract_all(formula, "([^ \\(\\)\\*\\+~\\|]+)"),
          #filter dataset for used variables, drop rows containing NA and standardize all numerical variables. Be careful with numeric subject numbers, these will also be standardized if not defined as factor.
          std_data = purrr::map(std_vars, .f = purrr::possibly( ~ data %>% dplyr::select(dplyr::any_of(.x)) %>% na.omit() %>% dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~scale(.x)))), otherwise = NA),
          #run standardized regressions and put into tidy data frames
          std_model = purrr::map2(.x = formula, .y = std_data,
                                  .f = purrr::possibly(~stats::lm(formula = as.formula(.x), data =  .y), otherwise = NA, quiet = quiet),
                                  .progress = ifelse(progress, "Running standardized regressions", FALSE))
        ) %>% dplyr::select(-c(std_vars, std_data))
    ) %>%  dplyr::relocate(std_model, .after = model) -> df_reg
  }

  return(df_reg)

}

