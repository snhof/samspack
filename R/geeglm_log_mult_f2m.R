#' geeglm_log_mult_f2m: Formula dataframe to dataframe with logistic GEE models
#'
#' Run and append logistic GEE models based on a formula dataframe created with [construct_formulas()].
#' The output can be used as input to [geeglm_log_mult_m2p()] to extract parameters in a tidy format.
#'
#' @inheritParams lm_mult_f2m
#' @param df_formulas Dataframe created with [construct_formulas()] containing formulas for regression analyses.
#' @param data data frame in long format with a row for every observation of the outcome variables. See [MS_trial_data] for an example dataset.
#' @param id Variable used to identify each participant, e.g. pat_id.
#' @param corstr The correlation structure for your GEE analysis. Default is "echangeable". See ?geeglm [geepack::geeglm()] for more information.
#'
#' @returns Dataframe provided as "df_formulas" with appended columns containing logistic GEE models and error messages.
#' @export
#'
#' @examples
#' df_formulas <- construct_formulas(
#' outcomes = "INO",
#' predictors = c("intervention", "intervention * time"),
#' covariates = c("", "+ gender + age")
#' )
#'
#' geeglm_log_mult_f2m(df_formulas, data = MS_trial_data, id = "pat_id")
#'
geeglm_log_mult_f2m <- function(df_formulas, data, id, corstr = "exchangeable", progress = FALSE, quiet = FALSE) {

  if (!id %in% names(data)) {
    stop("Column specified in 'id' was not found in 'data'.")
  }

  # run a logistic GEE model for each formula
  df_formulas %>%
    dplyr::mutate(
      model = purrr::map(
        formula,
        .f = purrr::safely(
          ~{
            model_formula <- as.formula(.x)
            model_vars <- unique(c(all.vars(model_formula), id))
            complete_rows <- stats::complete.cases(data[, model_vars, drop = FALSE])
            model_data <- data[complete_rows, , drop = FALSE]

            geepack::geeglm(
              formula = model_formula,
              data = model_data,
              id = model_data[[id]],
              family = binomial,
              corstr = corstr
            )
          },
          otherwise = NA,
          quiet = quiet
        ),
        .progress = ifelse(progress, "Running logistic GEE regressions", FALSE)
      )
    ) %>%
    tidyr::unnest_wider(col = model, strict = TRUE, names_sep = "_") %>%
    dplyr::rename(model = model_result) %>%
    dplyr::mutate(
      #Extract error messages and put in data frame
      model_error = purrr::map(model_error, purrr::possibly(.f = ~conditionMessage(.x)))
    ) %>%
    # unnest model_error while keeping nested
    tidyr::unnest(col = model_error, keep_empty = TRUE) -> df_reg

  return(df_reg)
}
