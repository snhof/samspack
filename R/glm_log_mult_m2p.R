#'  glm_log_mult_m2p: extract parameters from multiple logistic regression models to tidy table
#'
#' @inheritParams construct_formulas
#' @inheritParams lm_mult
#' @param df_reg Dataframe output from [glm_log_mult_f2m()].
#' @param exponentiate Do you want to exponentiate the output (e^x) so that you get the odds ratio? Default is TRUE.
#'
#' @returns Dataframe with results from multiple logistic regression analyses.
#' @export
#'
#' @examples
#'df_formulas <- construct_formulas(
#'outcomes = "INO",
#'predictors = c("intervention", "age"),
#'covariates = c("", "+ gender")
#')
#'
#'df_reg <- glm_log_mult_f2m(df_formulas, data = MS_trial_data)
#'
#'glm_log_mult_m2p(df_reg)
#'
glm_log_mult_m2p <- function(df_reg, exponentiate = TRUE, progress = FALSE) {

  df_reg %>%
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::any_of(c("model", "std_model")),
        .fns = list(
          result = ~purrr::map(.x,
                               .f = purrr::possibly(~broom::tidy(.x, exponentiate = exponentiate, conf.int = FALSE)),
                               .progress = ifelse(progress, "Making output pretty", FALSE)),
          confint = ~purrr::map(.x, .f = purrr::possibly(~confint.default(.x)))
        ),
        .names = "{.col}_{.fn}"
      )
    ) %>%
    dplyr::select(-dplyr::any_of(c("model", "std_model"))) %>%
    # unnest each model so all models form a single table
    tidyr::unnest(col = dplyr::any_of(c("model_result", "model_confint", "std_model_result", "std_model_confint")),
                  keep_empty = TRUE, names_sep = "_") %>%
    dplyr::select(-(dplyr::starts_with("std_model")&!dplyr::ends_with("estimate"))) %>%
    dplyr::rename_with(~stringr::str_remove(.x, "model_") %>% stringr::str_remove("result_")) %>%
    dplyr::mutate(confint = asplit(confint, 1)) %>%
    tidyr::unnest_wider(confint, names_sep = "_")%>%
    dplyr::mutate(conf.low = as.numeric(confint_1), conf.high = as.numeric(confint_2)) %>%
    dplyr::select(-c(confint_1, confint_2)) %>%
    dplyr::mutate(
      dplyr::across(c(conf.low, conf.high), ~dplyr::case_when(
        exponentiate == TRUE ~ exp(.x),
        exponentiate == FALSE ~ .x)), #exponentiate confidence intervals if exponentiate is TRUE
      # round all numeric columns
      dplyr::across(dplyr::where(is.numeric), ~round(.x, digits = 5)),
      #define used statistical method
      model = "Logistic regression"
    ) %>%
    dplyr::relocate(model)
}

utils::globalVariables(c(
  "model_confint", "std_confint", "confint", "confint_1", "confint_2"
))
