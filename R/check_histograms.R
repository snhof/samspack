#' check_histograms: Check normality for multiple variables in dataframe
#'
#' @param data Dataframe containing numeric variables to check for normality.
#' @inheritParams histogram_sp
#'
#' @returns List of ggplot2 histogram objects for each numeric variable, with outliers labeled.
#' @export
#' @importFrom magrittr %>%
#' @examples
#' check_histograms(data = MS_trial_data, id = pat_id, facet_cols = gender)
#'
check_histograms <- function (data, facet_rows = NULL, facet_cols = NULL, id=NULL, bins_method = "sturges") {
purrr::map(
  .x = data %>% dplyr::select(dplyr::where(is.numeric), -{{id}}) %>% names(),
  .f = ~histogram_sp(data=data, var = !!dplyr::sym(.x), id = {{id}}, bins_method = bins_method, facet_cols = {{facet_cols}}, facet_rows = {{facet_rows}})
)
}
