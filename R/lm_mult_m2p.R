lm_mult_m2p <- function(df_reg, std_beta = FALSE, progress = FALSE, data = NULL) {
  # run a lineair model and iterate through each formula
  df_reg %>%
    dplyr::mutate(
      #Put regression results into tidy data frame
      model_result = purrr::map(model_result, .f = purrr::possibly(~broom::tidy(.x, conf.int = TRUE)), .progress = ifelse(progress, "Extracting parameters from models", FALSE))
    ) %>%
    # unnest each model so all models form a single table
    tidyr::unnest(col = model_result) -> df_reg

  if(std_beta == TRUE){
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
                                  .f = purrr::possibly(~stats::lm(formula = as.formula(.x), data =  .y), otherwise = NA, quiet = FALSE),
                                  .progress = ifelse(progress, "Running standardized regressions", FALSE)) %>%
            purrr::map(purrr::possibly(~broom::tidy(.x, conf.int = TRUE)))
          # unnest each model so all models form a single table
        ) %>% tidyr::unnest(std_model) %>%
        #Only select estimate and confidence interval en give unique name
        dplyr::select(dplyr::any_of(c("outcome", "predictor", "covariate", "formula", "term", "estimate", "conf.low", "conf.high"))) %>% dplyr::rename_with(~paste0("std_", .x), .cols = c(estimate, conf.low, conf.high)) %>%
        dplyr::filter(term != "(Intercept)"),
      by = c("term", "outcome", "predictor", "formula", df %>% dplyr::select(dplyr::any_of(c("covariate"))) %>% names())
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
