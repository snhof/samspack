#' lm_mult_f2m: Formula dataframe to dataframe with linear regression models
#'
#'Run and append linear regression models based on a formula dataframe created with [construct_formulas()].
#'
#' @param df_formulas Dataframe containing regression formulas in a column named 'formula', created with [construct_formulas()].
#' @param data dataframe where each row represents an observation of the outcome variable.
#' @param progress Whether to show a progress bar. Use TRUE to turn on a progress bar, default is FALSE.
#'
#' @returns Datafram provided as "df_formulas" with appended columns containing linear regression models.
#' @export
#'
#' @examples
#' df_formulas <- construct_formulas(
#' outcomes = c("EDSS", "SDMT"),
#' predictors = "age",
#' covariates = c("", "+ gender")
#' )
#'
#' lm_mult_f2m(
#' df_formulas = df_formulas,
#' data = MS_trial_data
#' )
#'
lm_mult_f2m <- function(df_formulas, data, progress = FALSE) {
  # run a lineair model for each formula
  df_formulas %>%
    dplyr::mutate(
      model = purrr::map(
        formula,
        .f = purrr::safely(~stats::lm(formula = as.formula(.x), data = data), otherwise = NA, quiet = FALSE),
        .progress = ifelse(progress, "Running regressions", FALSE)
      )
    ) %>% tidyr::unnest_wider(col=model, strict = TRUE, names_sep = "_") %>%
    dplyr::mutate(
      #Extract error messages and put in data frame
      model_error = purrr::map(model_error, purrr::possibly(.f = ~conditionMessage(.x)))
    ) %>%
    # unnest model_error while keeping nested
    tidyr::unnest(col = model_error, keep_empty = TRUE)

  # %>%
  #   dplyr::mutate(
  #     #Extract errror messages and put in data frame
  #     model_error = purrr::map(model_error, purrr::possibly(.f = ~conditionMessage(.x))),
  #     #Put regression results into tidy data frame
  #     model_result = purrr::map(model_result, .f = purrr::possibly(~broom::tidy(.x, conf.int = TRUE)), .progress = ifelse(progress, "Making output pretty", FALSE))
  #   ) %>%
  #
  #   # unnest each model so all models form a single table
  #   tidyr::unnest(col = c(model_result, model_error)) -> df_reg
  #
  # if(std_beta == TRUE){
  #   # generate additional dataframe with standardized regressions and append to normal regressions
  #   df_reg %>% dplyr::left_join(
  #     df %>%
  #       dplyr::mutate(
  #         # extract variables used in regression formula
  #         std_vars = stringr::str_extract_all(formula, "([^ \\(\\)\\*\\+~\\|]+)"),
  #         #filter dataset for used variables, drop rows containing NA and standardize all numerical variables. Be careful with numeric subject numbers, these will also be standardized if not defined as factor.
  #         std_data = purrr::map(std_vars, .f = purrr::possibly( ~ data %>% dplyr::select(dplyr::any_of(.x)) %>% na.omit() %>% dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~scale(.x)))), otherwise = NA),
  #         #run standardized regressions and put into tidy data frames
  #         std_model = purrr::map2(.x = formula, .y = std_data,
  #                                 .f = purrr::possibly(~stats::lm(formula = as.formula(.x), data =  .y), otherwise = NA, quiet = FALSE),
  #                                 .progress = ifelse(progress, "Running standardized regressions", FALSE)) %>%
  #           purrr::map(purrr::possibly(~broom::tidy(.x, conf.int = TRUE)))
  #         # unnest each model so all models form a single table
  #       ) %>% tidyr::unnest(std_model) %>%
  #       #Only select estimate and confidence interval en give unique name
  #       dplyr::select(dplyr::any_of(c("outcome", "predictor", "covariate", "formula", "term", "estimate", "conf.low", "conf.high"))) %>% dplyr::rename_with(~paste0("std_", .x), .cols = c(estimate, conf.low, conf.high)) %>%
  #       dplyr::filter(term != "(Intercept)"),
  #     by = c("term", "outcome", "predictor", "formula", df %>% dplyr::select(dplyr::any_of(c("covariate"))) %>% names())
  #   ) -> df_reg
  # }

}

