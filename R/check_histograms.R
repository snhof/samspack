#' check_histograms: Check normality for multiple variables in dataframe
#'
#' @param data Dataframe containing numeric variables to check for normality.
#' @param facets_rows Variable to split data by in rows for faceted plots.
#' @param facets_cols Variable to split data by in columns for faceted plots.
#' @param id Identifier for labeling outliers in plots, default is row number.
#' @param bins_method Method for determining histogram bin width. Choices are "sturges", "scott", "fd" (Freedman-Diaconis) or a numeric value specifying the number of bins. Default is "sturges".
#'
#' @returns List of ggplot2 histogram objects for each numeric variable, with outliers labeled.
#' @export
#' @importFrom magrittr %>%
#' @examples
#' check_histograms(data = MS_trial_data, id = pat_id, facets_cols = gender)
#'
check_histograms <- function (data, facets_rows = NULL, facets_cols = NULL, id=dplyr::row_number(), bins_method = "sturges") {

  binwidth <- function(x) {bins_method(x, method = bins_method)}

  if(!missing(facets_rows) & !missing(facets_cols)){
    data %>%
      dplyr::group_by({{facets_rows}}, {{facets_cols}}) %>%
      dplyr::mutate(
        dplyr::across(dplyr::where(is.numeric), ~mean(., na.rm=TRUE), .names = "{.col}_facet_mean"),
        dplyr::across(dplyr::where(is.numeric), ~median(., na.rm=TRUE), .names = "{.col}_facet_median"),
        dplyr::across(dplyr::where(is.numeric), ~mean(., na.rm=TRUE) + 2 * sd(., na.rm = TRUE), .names = "{.col}_facet_plus2sd"),
        dplyr::across(dplyr::where(is.numeric), ~mean(., na.rm=TRUE) - 2 * sd(., na.rm = TRUE), .names = "{.col}_facet_min2sd"),
        dplyr::across(dplyr::where(is.numeric), ~dplyr::if_else(samspack::is_outlier(.x), TRUE, FALSE), .names = "{.col}_outlier")
      ) %>% dplyr::ungroup() -> data_hist

    histogram_sp <- function(x, data_hist, binwidth, facets_rows, facets_cols){
      data_hist %>%
        ggplot2::ggplot(ggplot2::aes(x=get(x))) +
        ggplot2::geom_histogram(color=1, fill="white", binwidth = binwidth)+
        ggplot2::geom_density(ggplot2::aes(y = ggplot2::after_stat(count)), color = "red")+
        ggplot2::geom_vline(ggplot2::aes(xintercept = get(paste0(x, "_facet_median")), group = interaction({{facets_rows}}, {{facets_cols}}), color = "median"), linetype = "dashed")+
        ggplot2::geom_vline(ggplot2::aes(xintercept = get(paste0(x, "_facet_mean")), group = interaction({{facets_rows}}, {{facets_cols}}), color = "mean"), linetype = "dashed")+
        ggplot2::geom_vline(ggplot2::aes(xintercept = get(paste0(x, "_facet_min2sd")), group = interaction({{facets_rows}}, {{facets_cols}}), color = "min2sd"), linetype = "dashed")+
        ggplot2::geom_vline(ggplot2::aes(xintercept = get(paste0(x, "_facet_plus2sd")), group = interaction({{facets_rows}}, {{facets_cols}}), color = "plus2sd"), linetype = "dashed")+
        ggplot2::facet_grid(rows = ggplot2::vars({{facets_rows}}), cols = ggplot2::vars({{facets_cols}}))+
        ggplot2::scale_color_manual(name = "statistics", values = c(median = "green", mean = "red", min2sd = "blue", plus2sd = "blue"), labels = c(median = "Median", mean = "Mean", min2sd = "-2 SD", plus2sd = "+2 SD"))+
        ggplot2::labs(x=x) -> gobj

      ymax <- ggplot2::ggplot_build(gobj)$layout$panel_params[[1]]$y.range[2]
      boxplot_width <- ymax * 0.1

      gobj +
        ggplot2::geom_boxplot(position = ggplot2::position_nudge(y= -boxplot_width), width = boxplot_width, varwidth = TRUE)+
        ggrepel::geom_text_repel(
          data = data_hist %>% dplyr::filter(!!rlang::sym(paste0(x, "_outlier"))),
          y= -boxplot_width,
          size = 3,
          ggplot2::aes(label = {{id}}))
    }

    purrr::map(
      .x = data_hist %>% dplyr::select(dplyr::where(is.numeric),-dplyr::ends_with(c("_facet_mean", "_facet_median", "_facet_plus2sd", "_facet_min2sd")), -{{id}}) %>% names(),
      .f = ~histogram_sp(.x, data_hist = data_hist, binwidth = binwidth, facets_rows = {{facets_rows}}, facets_cols = {{facets_cols}})
    )

  } else if(!missing(facets_cols)){

    data %>%
      dplyr::group_by({{facets_cols}}) %>%
      dplyr::mutate(
        dplyr::across(dplyr::where(is.numeric), ~mean(., na.rm=TRUE), .names = "{.col}_facet_mean"),
        dplyr::across(dplyr::where(is.numeric), ~median(., na.rm=TRUE), .names = "{.col}_facet_median"),
        dplyr::across(dplyr::where(is.numeric), ~mean(., na.rm=TRUE) + 2 * sd(., na.rm = TRUE), .names = "{.col}_facet_plus2sd"),
        dplyr::across(dplyr::where(is.numeric), ~mean(., na.rm=TRUE) - 2 * sd(., na.rm = TRUE), .names = "{.col}_facet_min2sd"),
        dplyr::across(dplyr::where(is.numeric), ~dplyr::if_else(is_outlier(.x), TRUE, FALSE), .names = "{.col}_outlier")
      ) %>% dplyr::ungroup() -> data_hist


    histogram_sp <- function(x, data_hist, binwidth, facets_cols){
      data_hist %>%
        ggplot2::ggplot(ggplot2::aes(x=get(x))) +
        ggplot2::geom_histogram(color=1, fill="white", binwidth = binwidth) +
        ggplot2::geom_density(ggplot2::aes(y = ggplot2::after_stat(count)), color = "red")+
        ggplot2::geom_vline(ggplot2::aes(xintercept = get(paste0(x, "_facet_median")), group = {{facets_cols}}, color = "median"), linetype = "dashed")+
        ggplot2::geom_vline(ggplot2::aes(xintercept = get(paste0(x, "_facet_mean")), group = {{facets_cols}}, color = "mean"), linetype = "dashed")+
        ggplot2::geom_vline(ggplot2::aes(xintercept = get(paste0(x, "_facet_min2sd")), group = {{facets_cols}}, color = "min2sd"), linetype = "dashed")+
        ggplot2::geom_vline(ggplot2::aes(xintercept = get(paste0(x, "_facet_plus2sd")), group = {{facets_cols}}, color = "plus2sd"), linetype = "dashed")+
        ggplot2::facet_wrap(facets = ggplot2::vars({{facets_cols}}))+
        ggplot2::scale_color_manual(name = "statistics", values = c(median = "green", mean = "red", min2sd = "blue", plus2sd = "blue"), labels = c(median = "Median", mean = "Mean", min2sd = "-2 SD", plus2sd = "+2 SD"))+
        ggplot2::labs(x=x) -> gobj

      ymax <- ggplot2::ggplot_build(gobj)$layout$panel_params[[1]]$y.range[2]
      boxplot_width <- ymax * 0.1

      gobj +
        ggplot2::geom_boxplot(position = ggplot2::position_nudge(y= -boxplot_width), width = boxplot_width, varwidth = TRUE)+
        ggrepel::geom_text_repel(
          data = data_hist %>% dplyr::filter(!!rlang::sym(paste0(x, "_outlier"))),
          y= -boxplot_width,
          size = 3,
          ggplot2::aes(label = {{id}}))
    }

    purrr::map(
      .x = data_hist %>% dplyr::select(dplyr::where(is.numeric),-dplyr::ends_with(c("_facet_mean", "_facet_median", "_facet_plus2sd", "_facet_min2sd")), -{{id}}) %>% names(),
      .f = ~histogram_sp(.x, data_hist = data_hist, binwidth = binwidth, facets_cols = {{facets_cols}})
    )

  } else {

    data %>%
      dplyr::mutate(
        dplyr::across(dplyr::where(is.numeric), ~mean(., na.rm=TRUE), .names = "{.col}_facet_mean"),
        dplyr::across(dplyr::where(is.numeric), ~median(., na.rm=TRUE), .names = "{.col}_facet_median"),
        dplyr::across(dplyr::where(is.numeric), ~mean(., na.rm=TRUE) + 2 * sd(., na.rm = TRUE), .names = "{.col}_facet_plus2sd"),
        dplyr::across(dplyr::where(is.numeric), ~mean(., na.rm=TRUE) - 2 * sd(., na.rm = TRUE), .names = "{.col}_facet_min2sd"),
        dplyr::across(dplyr::where(is.numeric), ~dplyr::if_else(is_outlier(.x), TRUE, FALSE), .names = "{.col}_outlier")
      ) %>% dplyr::ungroup() -> data_hist

    histogram_sp <- function(x, data_hist, binwidth){
      data_hist %>%
        ggplot2::ggplot(ggplot2::aes(x=get(x))) +
        ggplot2::geom_histogram(color=1, fill="white", binwidth = binwidth)+
        ggplot2::geom_density(ggplot2::aes(y = ggplot2::after_stat(count)), color = "red")+
        ggplot2::geom_vline(ggplot2::aes(xintercept = get(paste0(x, "_facet_median")), color = "median"), linetype = "dashed")+
        ggplot2::geom_vline(ggplot2::aes(xintercept = get(paste0(x, "_facet_mean")), color = "mean"), linetype = "dashed")+
        ggplot2::geom_vline(ggplot2::aes(xintercept = get(paste0(x, "_facet_min2sd")), color = "min2sd"), linetype = "dashed")+
        ggplot2::geom_vline(ggplot2::aes(xintercept = get(paste0(x, "_facet_plus2sd")), color = "plus2sd"), linetype = "dashed")+
        ggplot2::scale_color_manual(name = "statistics", values = c(median = "green", mean = "red", min2sd = "blue", plus2sd = "blue"), labels = c(median = "Median", mean = "Mean", min2sd = "-2 SD", plus2sd = "+2 SD"))+
        ggplot2::labs(x=x) -> gobj

      ymax <- ggplot2::ggplot_build(gobj)$layout$panel_params[[1]]$y.range[2]
      boxplot_width <- ymax * 0.1

      gobj +
        ggplot2::geom_boxplot(position = ggplot2::position_nudge(y= -boxplot_width), width = boxplot_width, varwidth = TRUE)+
        ggrepel::geom_text_repel(
          data = data_hist %>% dplyr::filter(!!rlang::sym(paste0(x, "_outlier"))),
          y= -boxplot_width,
          size = 3,
          ggplot2::aes(label = {{id}}))
    }

    purrr::map(
      .x = data_hist %>% dplyr::select(dplyr::where(is.numeric),-dplyr::ends_with(c("_facet_mean", "_facet_median", "_facet_plus2sd", "_facet_min2sd")), -{{id}}) %>% names(),
      .f = ~histogram_sp(.x, data_hist = data_hist, binwidth = binwidth)
    )

  }
}
