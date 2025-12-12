#' lm_mult_m2p: Extract parameters from multiple linear models in a dataframe into a tidy table
#'
#' @inheritParams lm_mult
#' @param df_reg Dataframe containing multiple linear models in a column named 'model' and, optionally standardized models in a column named "std_model". This dataframe can be created with [lm_mult_f2m()].
#' @param exp_log Do you want to exponentiate the results for log transformed outcomes, set this to TRUE. Default is FALSE.
#' @param progress Whether to show a progress bar. Use TRUE to turn on a progress bar, default is FALSE.
#'
#' @returns Dataframe with results from multiple linear regression analyses.
#' @export
#'
#' @examples
#' df_formulas <- construct_formulas(
#' outcomes = c("EDSS", "SDMT"),
#' predictors = "age",
#' covariates = c("", "+ gender")
#' )
#'
#' df_reg <- lm_mult_f2m(df_formulas, data = MS_trial_data)
#'
#' lm_mult_m2p(df_reg)
#'
#'
lm_mult_m2p <- function(df_reg, exp_log = FALSE, progress = FALSE) {
  # run a lineair model and iterate through each formula
  df_reg %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::any_of(c("model", "std_model")),
        ~ifelse(
          is.na(.x),
          .x,
          purrr::map(
            .x,
            .f = purrr::possibly(
              ~broom::tidy(.x, conf.int = TRUE),
              quiet = FALSE
            ),
            .progress = ifelse(progress, "Extracting parameters from models", FALSE)
          )
        )
      )
    ) %>%
    # unnest each model so all models form a single table
    tidyr::unnest(col = dplyr::any_of(c(c("model", "std_model"))), keep_empty = TRUE, names_sep = "_") %>%
    dplyr::select(-(dplyr::starts_with("std_model")&!dplyr::ends_with("estimate"))) %>%
    dplyr::rename_with(~stringr::str_remove(.x, "model_")) %>%
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
