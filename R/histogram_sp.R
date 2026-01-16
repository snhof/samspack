#' Histogram with mean, median, SD, boxplot and outliers
#'
#' @param data Dataframe containing numeric variables to check for normality.
#' @param var Numeric variable to create histogram for.
#' @param id Identifier for labeling outliers in plots, default is row number.
#' @param bins_method Method for determining histogram bin width. Choices are "sturges", "scott", "fd" (Freedman-Diaconis) or a numeric value specifying the number of bins. Default is "sturges".
#' @param facet_cols Variable to split data by in rows for faceted plots.
#' @param facet_rows Variable to split data by in columns for faceted plots.
#'
#' @returns ggplot2 histogram object with outliers labeled.
#' @export
#' @importFrom magrittr %>%
#' @examples
#'
#' histogram_sp(
#' data = MS_trial_data,
#' var = SDMT,
#' id = pat_id,
#' facet_cols = intervention,
#' facet_rows = INO
#' )
#'
histogram_sp <- function(data, var, id = NULL, bins_method = "sturges", facet_cols = NULL, facet_rows = NULL) {
  data %>%
    ggplot2::ggplot(ggplot2::aes(x = {{var}})) +
    ggplot2::geom_histogram(color=1, fill="white", binwidth = ~bins_method(.x, method))+
    ggplot2::geom_density(ggplot2::aes(y = ggplot2::after_stat(count)), color = "red")+
    ggplot2::geom_vline(ggplot2::aes(x = median({{var}}, na.rm = TRUE), y = {{var}}, xintercept = ggplot2::after_stat(y), color = "mean"), stat = "summary", fun = ~mean(.x), linetype = "dashed", na.rm = TRUE)+
    ggplot2::geom_vline(ggplot2::aes(x = median({{var}}, na.rm = TRUE), y = {{var}}, xintercept = ggplot2::after_stat(y), color = "median"), stat = "summary", fun = ~median(.x), linetype = "dashed", na.rm = TRUE)+
    ggplot2::geom_vline(ggplot2::aes(x = median({{var}}, na.rm = TRUE), y = {{var}}, xintercept = ggplot2::after_stat(y), color = "plus2sd"), stat = "summary", fun = ~mean(.x) + 2*sd(.x), linetype = "dashed", na.rm = TRUE)+
    ggplot2::geom_vline(ggplot2::aes(x = median({{var}}, na.rm = TRUE), y = {{var}}, xintercept = ggplot2::after_stat(y), color = "min2sd"), stat = "summary", fun = ~mean(.x) - 2*sd(.x), linetype = "dashed", na.rm = TRUE)+
    ggplot2::scale_color_manual(name = "statistics", values = c(
      median = "green",
      mean = "red",
      min2sd = "blue",
      plus2sd = "blue"
    ),
    labels = c(median = "Median", mean = "Mean", min2sd = "-2 SD", plus2sd = "+2 SD"))+
    ggplot2::facet_grid(rows = dplyr::vars({{facet_rows}}), cols = dplyr::vars({{facet_cols}})) +
    ggplot2::labs(x = dplyr::quo({{var}}))  -> gobj

  ymax <- ggplot2::ggplot_build(gobj)$layout$panel_params[[1]]$y.range[2]
  boxplot_width <- ymax * 0.1

  gobj +
    ggplot2::geom_boxplot(position = ggplot2::position_nudge(y= -boxplot_width), width = boxplot_width, varwidth = TRUE)+
    ggrepel::geom_text_repel(
      data = data %>% dplyr::group_by({{facet_rows}}, {{facet_cols}}) %>% dplyr::filter(is_outlier({{var}})),
      y= -boxplot_width,
      size = 3,
      ggplot2::aes(label = ifelse(is.null({{id}}), rownames(data), {{id}}))
    )
}

utils::globalVariables(c("count", "y"))
