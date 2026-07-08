#' geeglm_log_mult_m2p: extract parameters from multiple logistic GEE models to tidy table
#'
#' @inheritParams lm_mult
#' @param df_reg Dataframe output from [geeglm_log_mult_f2m()].
#' @param exponentiate Do you want to exponentiate the output (e^x) so that you get the odds ratio? Default is TRUE.
#'
#' @returns Dataframe with results from multiple logistic GEE regression analyses.
#' @export
#'
#' @examples
#' df_formulas <- construct_formulas(
#' outcomes = "INO",
#' predictors = c("intervention", "intervention * time"),
#' covariates = c("", "+ gender + age")
#' )
#'
#' df_reg <- geeglm_log_mult_f2m(df_formulas, data = MS_trial_data, id = "pat_id")
#'
#' geeglm_log_mult_m2p(df_reg)
#'
geeglm_log_mult_m2p <- function(df_reg, exponentiate = TRUE, progress = FALSE) {

  df_reg %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::any_of("model"),
        ~ifelse(
          is.na(.x),
          .x,
          purrr::map(
            .x,
            .f = purrr::possibly(
              ~broom::tidy(.x, exponentiate = exponentiate, conf.int = TRUE),
              quiet = FALSE
            ),
            .progress = ifelse(progress, "Extracting parameters from models", FALSE)
          )
        )
      )
    ) %>%
    # unnest each model so all models form a single table
    tidyr::unnest(col = model, keep_empty = TRUE) %>%
    dplyr::mutate(
      # round all numeric columns
      dplyr::across(dplyr::where(is.numeric), ~round(.x, digits = 5)),
      #define used statistical method
      model = "Logistic GEE"
    ) %>%
    dplyr::relocate(model)
}
