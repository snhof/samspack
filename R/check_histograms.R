#' check_histograms: Check normality for multiple variables in dataframe
#'
#' @param data Dataframe containing numeric variables to check for normality.
#' @param facets_rows Variable to split data by in rows for faceted plots.
#' @param facets_cols Variable to split data by in columns for faceted plots.
#' @param id Identifier for labeling outliers in plots, default is row number.
#'
#' @returns List of ggplot2 histogram objects for each numeric variable, with outliers labeled.
#' @export
#' @importFrom magrittr %>%
#' @examples
#' check_histograms(data = MS_trial_data, id = pat_id, facets_cols = gender)
#'
check_histograms <- function (data, facets_rows = NULL, facets_cols = NULL, id=dplyr::row_number()) {
  # Pre-compute outliers once for all cases
  {{data}} %>%
    dplyr::mutate(
      dplyr::across(dplyr::where(is.numeric), ~dplyr::if_else(samspack::is_outlier(.x), TRUE, FALSE), .names = "{.col}_outlier")
    ) -> data_outlier
  
  if(!missing(facets_rows) & !missing(facets_cols)){
    data %>%
      dplyr::group_by({{facets_rows}}, {{facets_cols}}) %>%
      dplyr::mutate(
        # Combine mean and median calculations in one pass
        dplyr::across(dplyr::where(is.numeric), list(
          facet_mean = ~mean(., na.rm=TRUE),
          facet_median = ~median(., na.rm=TRUE)
        ))
      ) %>% dplyr::ungroup() -> data_hist

    purrr::map(
      .x = data_hist %>% dplyr::select(dplyr::where(is.numeric),-dplyr::ends_with("_facet_mean"), -dplyr::ends_with("_facet_median"), -{{id}}) %>% names(),
      .f = ~ data_hist %>%
        ggplot2::ggplot(ggplot2::aes(x=get(.x))) +
        ggplot2::geom_histogram(color=1, fill="white")+
        ggplot2::geom_boxplot(position = ggplot2::position_nudge(y=-1))+
        ggplot2::geom_vline(ggplot2::aes(xintercept = get(paste0(.x, "_facet_median")), group = interaction({{facets_rows}}, {{facets_cols}}), color = "median"))+
        ggplot2::geom_vline(ggplot2::aes(xintercept = get(paste0(.x, "_facet_mean")), group = interaction({{facets_rows}}, {{facets_cols}}), color = "mean"))+
        ggplot2::facet_grid(rows = ggplot2::vars({{facets_rows}}), cols = ggplot2::vars({{facets_cols}}))+
        ggplot2::scale_color_manual(name = "statistics", values = c(median = "green", mean = "red"))+
        ggplot2::labs(x=.x)+
        ggrepel::geom_text_repel(
          data = data_outlier %>% dplyr::filter(!!rlang::sym(paste0(.x, "_outlier"))),
          y=-1,
          ggplot2::aes(label = {{id}}))
    )

  } else if(!missing(facets_cols)){

    data %>%
      dplyr::group_by({{facets_cols}}) %>%
      dplyr::mutate(
        # Combine mean and median calculations in one pass
        dplyr::across(dplyr::where(is.numeric), list(
          facet_mean = ~mean(., na.rm=TRUE),
          facet_median = ~median(., na.rm=TRUE)
        ))
      ) %>% dplyr::ungroup() -> data_hist

    purrr::map(
      .x = data_hist %>% dplyr::select(dplyr::where(is.numeric),-dplyr::ends_with("_facet_mean"), -dplyr::ends_with("_facet_median"), -{{id}}) %>% names(),
      .f = ~ data_hist %>%
        ggplot2::ggplot(ggplot2::aes(x=get(.x))) +
        ggplot2::geom_histogram(color=1, fill="white")+
        ggplot2::geom_boxplot(position = ggplot2::position_nudge(y=-1))+
        ggplot2::geom_vline(ggplot2::aes(xintercept = get(paste0(.x, "_facet_median")), group = {{facets_cols}}, color = "median"))+
        ggplot2::geom_vline(ggplot2::aes(xintercept = get(paste0(.x, "_facet_mean")), group = {{facets_cols}}, color = "mean"))+
        ggplot2::facet_wrap(facets = ggplot2::vars({{facets_cols}}))+
        ggplot2::scale_color_manual(name = "statistics", values = c(median = "green", mean = "red"))+
        ggplot2::labs(x=.x)+
        ggrepel::geom_text_repel(
          data = data_outlier %>% dplyr::filter(!!rlang::sym(paste0(.x, "_outlier"))),
          y=-1,
          ggplot2::aes(label = {{id}}))
    )

  } else {

    data %>%
      dplyr::mutate(
        # Combine mean and median calculations in one pass
        dplyr::across(dplyr::where(is.numeric), list(
          hist_mean = ~mean(., na.rm=TRUE),
          hist_median = ~median(., na.rm=TRUE)
        ))
      ) %>% dplyr::ungroup() -> data_hist

    purrr::map(
      .x = data_hist %>% dplyr::select(dplyr::where(is.numeric),-dplyr::ends_with("_hist_mean"), -dplyr::ends_with("_hist_median"),-{{id}}) %>% names(),
      .f = ~ data_hist %>%
        ggplot2::ggplot(ggplot2::aes(x=get(.x))) +
        ggplot2::geom_histogram(color=1, fill="white")+
        ggplot2::geom_boxplot(position = ggplot2::position_nudge(y=-1))+
        ggplot2::geom_vline(ggplot2::aes(xintercept = get(paste0(.x, "_hist_median")), color = "median"))+
        ggplot2::geom_vline(ggplot2::aes(xintercept = get(paste0(.x, "_hist_mean")), color = "mean"))+
        ggplot2::scale_color_manual(name = "statistics", values = c(median = "green", mean = "red"))+
        ggplot2::labs(x=.x)+
        ggrepel::geom_text_repel(
          data = data_outlier %>% dplyr::filter(!!rlang::sym(paste0(.x, "_outlier"))),
          y=-1,
          ggplot2::aes(label = {{id}}))
    )
  }
}
