#'  glm_log_mult: run multiple logistic regressions and output in table
#'
#' @inheritParams construct_formulas
#' @param data Dataframe containing all variables for regression analyses.
#' @param exponentiate Do you want to exponentiate the output (e^x) so that you get the odds ratio? Default is TRUE.
#' @param show_progress Do you want to see the progress of running the models? Default is FALSE.
#' @param std_odds Do you want to standardize the log(odds)? Default is FALSE.
#'
#' @returns Dataframe with results from multiple logistic regression analyses.
#' @export
#'
#' @examples
#'
#' glm_log_mult(
#' data = MS_trial_data,
#' outcomes = "INO",
#' predictors = c("intervention", "age"),
#' covariates = c("", "+ gender")
#' )
#'
# glm_log_mult <- function(data, outcomes, predictors, covariates="", exponentiate = TRUE, show_progress = FALSE, std_odds = FALSE) {
#   #create data frame with regression formulas
#   #Making all possible combinations of outcomes, predictors and covariates
#   tidyr::expand_grid(
#     outcome = outcomes,
#     predictor = predictors,
#     covariate = covariates
#   ) %>%
#     #pasting outcomes, predictors and covariates together to create formula.
#     dplyr::mutate(formula=paste0(paste(outcome, predictor, sep = "~"), covariate)) -> df
#
#   # run a lineair model and iterate through each formula
#   df %>%
#     dplyr::mutate(
#       model = purrr::map(
#         formula,
#         .f = purrr::safely(~glm(formula = as.formula(.x), data = data, family = binomial), otherwise = NA, quiet = FALSE),
#         .progress = ifelse(show_progress, "Running logistic regressions", FALSE))
#     ) %>% tidyr::unnest_wider(col=model, strict = TRUE, names_sep = "_") %>%
#     dplyr::mutate(
#       #Extract error messages and put in data frame
#       model_error = purrr::map(model_error, purrr::possibly(.f = ~conditionMessage(.x))),
#       #Obtain confidence intervals from models using confint.default()
#       model_confint = purrr::map(model_result, .f = purrr::possibly(~confint.default(.x))),
#       #Put regression results into tidy data frame
#       model_result = purrr::map(model_result, .f = purrr::possibly(~tidy(.x, exponentiate = exponentiate, conf.int = FALSE)), .progress = ifelse(show_progress, "Making output pretty", FALSE))
#
#     ) %>%
#
#     # unnest each model so all models form a single table
#     tidyr::unnest(col = c(model_result, model_error,model_confint)) %>%
#     dplyr::mutate(model_confint = dplyr::as_tibble(model_confint) %>%
#              dplyr::rename_with( ~dplyr::case_when(
#                stringr::str_detect(.x, "2.5 %|V1") ~ "conf.low",
#                stringr::str_detect(.x, "97.5 %|V2") ~ "conf.high"
#              ))
#     ) %>% tidyr::unnest_wider(model_confint) -> df_reg
#
#   #For logistic regression only predictors and covariates are standardized as outcome is already on the same scale (OR). Be careful with interpretation.
#   if(std_odds == TRUE){
#     # generate additional dataframe with standardized regressions and append to normal regressions
#     df_reg %>% dplyr::left_join(
#       df %>%
#         dplyr::mutate(
#           # extract variables used in regression formula
#           std_vars = stringr::str_extract_all(paste(covariate, predictor), "([^ \\(\\)\\*\\+\\|]+)"),
#           #filter dataset for used variables, drop rows containing NA and standardize all numerical variables. Be careful with numeric subject numbers, these will also be standardized if not defined as factor.
#           std_data = purrr::map(std_vars, .f =  ~ data %>% dplyr::select(dplyr::any_of(c(outcome, .x))) %>% na.omit() %>% dplyr::mutate(dplyr::across(dplyr::any_of(.x) & dplyr::where(is.numeric), ~scale(.x)))),
#           #run standardized regressions and put into tidy data frames
#           std_model = purrr::map2(.x = formula, .y = std_data,
#                            .f = purrr::possibly(~glm(formula = as.formula(.x), data = .y, family = binomial), otherwise = NA, quiet = FALSE),
#                            .progress = ifelse(show_progress, "Adding standardized predictors", FALSE)),
#           std_confint = purrr::map(std_model, .f = purrr::possibly(~confint.default(.x))),
#           std_model = purrr::map(std_model, purrr::possibly(~tidy(.x, exponentiate = FALSE, conf.int = FALSE)))
#           # unnest each model so all models form a single table
#         ) %>% tidyr::unnest(cols = c(std_model, std_confint)) %>%
#         dplyr::mutate(std_confint = dplyr::as_tibble(std_confint) %>%
#                  dplyr::rename_with( ~dplyr::case_when(
#                    stringr::str_detect(.x, "2.5 %|V1") ~ "conf.low",
#                    stringr::str_detect(.x, "97.5 %|V2") ~ "conf.high"
#                  ))
#         ) %>% tidyr::unnest_wider(std_confint) %>%
#         #Only select estimate and confidence interval en give unique name
#         dplyr::select(outcome, predictor, covariate, formula, term, estimate, conf.low, conf.high) %>% dplyr::rename_with(~paste0("std_", .x), .cols = c(estimate, conf.low, conf.high)) %>%
#         dplyr::filter(term != "(Intercept)"),
#       by = dplyr::join_by(outcome, predictor, covariate, formula, term)
#     ) -> df_reg
#   }
#
#   df_reg %>%
#     dplyr::mutate(
#       dplyr::across(c(conf.low, conf.high), ~dplyr::case_when(
#         exponentiate == TRUE ~ exp(.x),
#         exponentiate == FALSE ~ .x)), #exponentiate confidence intervals if exponentiate is TRUE
#       # round all numeric columns
#       dplyr::across(dplyr::where(is.numeric), ~round(.x, digits = 5)),
#       #define used statistical method
#       model = "Logistic regression"
#     ) %>%
#     dplyr::relocate(model)
# }
#
# utils::globalVariables(c(
#   "model_confint", "std_confint"
# ))
