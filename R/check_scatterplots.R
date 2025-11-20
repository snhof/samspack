#' check_scatterplots: Create scatterplots for outcome-predictor combinations with outliers labeled
#'
#' @param data dataframe containing the variables
#' @param outcomes vector of outcome variable names
#' @param predictors vector of predictor variable names
#' @param id variable used to identify each participant, e.g. pat_id
#'
#' @returns A list of scatterplots for each outcome-predictor combination with outliers labeled.
#' @export
#'
#' @examples
#'
#' check_scatterplots(
#' data = MS_trial_data,
#' outcomes = c("EDSS", "SDMT"),
#' predictors = c("age", "time"),
#' id = "pat_id"
#' )
#'
check_scatterplots <- function(data, outcomes, predictors, id) {
  {{data}} %>%
    dplyr::mutate(
      dplyr::across(c({{outcomes}}, {{predictors}}), ~dplyr::if_else(is_outlier(.x), TRUE, FALSE), .names = "{.col}_outlier")
    ) -> data_outlier

  tidyr::expand_grid(
    outcome = outcomes,
    predictor = predictors
  ) -> grid

  purrr::map2(.x = grid$predictor, .y = grid$outcome, .f =
         ~ {{data}} %>%
         ggplot2::ggplot(ggplot2::aes(x = !!dplyr::sym(.x), y = !!dplyr::sym(.y)))+
         ggplot2::geom_point()+
         ggplot2::geom_smooth(method="lm")+
         ggrepel::geom_text_repel(
           data = data_outlier %>% dplyr::filter(!!dplyr::sym(paste0(.x, "_outlier")), !!dplyr::sym(paste0(.y, "_outlier"))),
           ggplot2::aes(label = {{id}}), color = "red")+
         ggrepel::geom_text_repel(
           data = data_outlier %>% dplyr::filter(xor(!!dplyr::sym(paste0(.x, "_outlier")), !!dplyr::sym(paste0(.y, "_outlier")))),
           ggplot2::aes(label = {{id}}))

  )
}
