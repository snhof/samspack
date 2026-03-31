#' Histogram with mean, median, SD, boxplot and outliers
#'
#' @param data Dataframe containing numeric variables to check for normality.
#' @param var Numeric variable to create histogram for.
#' @param id Identifier for labeling outliers in plots, default is row number.
#' @param bins_method Method for determining histogram bin width. Choices are "sturges", "scott", "fd" (Freedman-Diaconis) or a numeric value specifying the number of bins. Default is "sturges".
#' @param facet_cols Variable to split data by in rows for faceted plots.
#' @param facet_rows Variable to split data by in columns for faceted plots.
#' @param max.overlaps Maximum number of overlaps allowed for outlier labels before suppression. Default is 20. Use "Inf" to disable suppression entirely.
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
histogram_sp <- function(data, var, id = dplyr::row_number(), bins_method = "sturges", facet_cols = NULL, facet_rows = NULL, max.overlaps = 20) {
  gobj <- data %>%
    ggplot2::ggplot(ggplot2::aes(x = {{var}})) +
    #Histogram
    ggplot2::geom_histogram(color = 1, fill = "lightgrey", binwidth = ~ bins_method(.x, bins_method)) +
    #Mean line
    ggplot2::geom_vline(
      ggplot2::aes(
        x = median({{var}}, na.rm = TRUE),  y = {{var}},
        xintercept = ggplot2::after_stat(y), color = "mean"),
      stat = "summary",
      fun = ~ mean(.x),
      linetype = "dashed",
      na.rm = TRUE
    ) +
    # Median line
    ggplot2::geom_vline(
      ggplot2::aes(x = median({{var}}, na.rm = TRUE), y = {{var}}, xintercept = ggplot2::after_stat(y), color = "median"),
      stat = "summary",
      fun = ~ median(.x),
      linetype = "dashed",
      na.rm = TRUE
    ) +
    # +2 SD line
    ggplot2::geom_vline(
      ggplot2::aes(x = median({{var}}, na.rm = TRUE), y = {{var}}, xintercept = ggplot2::after_stat(y), color = "plus2sd"),
      stat = "summary",
      fun = ~ mean(.x) + 2 * sd(.x),
      linetype = "dashed",
      na.rm = TRUE
    ) +
    # -2 SD line
    ggplot2::geom_vline(
      ggplot2::aes(x = median({{var}}, na.rm = TRUE), y = {{var}}, xintercept = ggplot2::after_stat(y), color = "min2sd"),
      stat = "summary",
      fun = ~ mean(.x) - 2 * sd(.x),
      linetype = "dashed",
      na.rm = TRUE
    ) +
    # Color scale
    ggplot2::scale_color_manual(
      name = "statistics",
      values = c(median = "green", mean = "red", min2sd = "blue", plus2sd = "blue"),
      labels = c(median = "Median", mean = "Mean", min2sd = "-2 SD", plus2sd = "+2 SD")
    ) +
    # Facets if specified by facet_rows and/or facet_cols
    ggplot2::facet_grid(rows = dplyr::vars({{ facet_rows }}), cols = dplyr::vars({{ facet_cols }})) +
    # Var name as x-axis label
    ggplot2::labs(x = dplyr::quo({{var}}), y = "count")

  build <- ggplot2::ggplot_build(gobj)
  ymax <- max(build$data[[1]]$count, na.rm = TRUE)
  #ymax <- ggplot2::ggplot_build(gobj)$layout$panel_params[[1]]$y.range[2] # get y-axis maximum
  boxplot_width <- ymax * 0.1 # set boxplot width as 10% of y-axis maximum

  # Add boxplot and outlier labels
  gobj + ggplot2::geom_boxplot(
    position = ggplot2::position_nudge(y = -boxplot_width),
    width = boxplot_width,
    varwidth = TRUE
  ) +
    #Density plot
    ggplot2::geom_density(ggplot2::aes(y = ggplot2::after_stat(scaled) * ymax), color = "red") +
    ggplot2::scale_y_continuous(sec.axis = ggplot2::sec_axis(~ . / ymax, name = "density")) +
    ggplot2::coord_cartesian(ylim = c(-boxplot_width * 1.5, ymax * 1.05))+
    ggrepel::geom_text_repel(
      data = data %>% dplyr::group_by({{ facet_rows }}, {{ facet_cols }}) %>%
        dplyr::mutate(
          # Create id labels for outliers
          id = dplyr::if_else(is_outlier({{var}}), as.character({{id}}), ""), # If outlier, use provided id variable, otherwise no label)
        ) %>% dplyr::ungroup(),
      y = -boxplot_width,
      size = 2,
      ggplot2::aes(label = id),
      max.overlaps = max.overlaps
    )+
    #Styling
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.y.right = ggplot2::element_text(colour="red"),
      axis.title.y.right = ggplot2::element_text(colour="red")
    )

}


utils::globalVariables(c("count", "y", "scaled"))
