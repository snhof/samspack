#' lmer_assump_tests: Generate plots for checking linear mixed model assumptions
#'
#' Generate plots for checking linear mixed model assumptions: normality of residuals, linearity, and homoscedasticity.
#'
#' @param model A linear mixed model object created with \code{lme4::lmer()}.
#'
#' @returns Plots for checking linear mixed model assumptions: a histogram of residuals for normality, a residuals vs fitted plot for linearity, and a scale-location plot for homoscedasticity.
#' @export
#'
#' @examples
#'
#' model <- lme4::lmer(SDMT ~ intervention + (1|pat_id), data = MS_trial_data, REML = FALSE)
#' lmer_assump_tests(model)
#'
lmer_assump_tests <- function(model) {
  data <- broom.mixed::augment(model) %>%
    dplyr::mutate(
      residual_type = dplyr::case_when(
        .resid < 0 ~ "negative",
        .resid > 0 ~ "positive",
      )
    )

  #Histogram for checking normality residuals
  resid_histogram <- histogram_sp(data = data, var = .resid) +
    ggplot2::labs(x="Residuals") +
    ggplot2::theme(
      legend.position="bottom",
      legend.text = ggplot2::element_text(size=8),
      legend.title = ggplot2::element_text(size=8),
      legend.box.spacing = ggplot2::unit(0.1, "pt")
    )

  #Residuals vs fitted for checking linearity and homoscedasticity
  resid_fitted_plot <- ggplot2::ggplot(data = data, ggplot2::aes(x = .fitted, y = .resid)) +
    ggplot2::geom_point(alpha = 0.3) +
    ggplot2::geom_smooth(method = 'loess', formula = "y ~ x", ggplot2::aes(color = "linear")) +
    ggplot2::geom_smooth(method = 'lm', formula = "y ~ x", ggplot2::aes(color = "homosced", group = residual_type)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::scale_color_manual(
      name = "Assumptions",
      values = c(linear = "blue", homosced = "red"),
      labels = c(linear = "Linearity: line flat and horizontal", homosced = "Homoscedasticity: lines parallel", min2sd = "-2 SD", plus2sd = "+2 SD")
    )+
    ggplot2::labs(x = "Fitted values", y = "Residuals") +
    ggplot2::theme_minimal()+
    ggplot2::theme(
      axis.text = ggplot2::element_text(size=6),
      axis.title = ggplot2::element_text(size=8),
      legend.text = ggplot2::element_text(size=8),
      legend.title = ggplot2::element_text(size=8),
      legend.box.spacing = ggplot2::unit(0.1, "pt"),
      legend.position="bottom"
    )

  # QQ-plot random effects
  ranef_resid_plot_pre <- try(performance::check_normality(model, effects = "random") %>% plot())

  ranef_resid_plot<- ranef_resid_plot_pre[[1]]+
    ggplot2::theme(
      axis.text = ggplot2::element_text(size=6),
      axis.title = ggplot2::element_text(size=8),
      plot.title = ggplot2::element_text(size=9),
      plot.subtitle = ggplot2::element_text(size=8)
    )


  patchwork::wrap_plots(
    purrr::compact(c(resid_histogram, resid_fitted_plot, ranef_resid_plot)),
    design = "12
              13"
  ) +
    patchwork::plot_annotation(
    title = formula(model) %>% format() %>% paste0(collapse = "") %>% stringr::str_squish()
  )
}
