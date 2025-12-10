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
#' model <- lme4::lmer(SDMT ~ age + (1|gender), data = MS_trial_data, REML = FALSE)
#' lmer_assump_tests(model)
#'
lmer_assump_tests <- function(model) {
  ggpubr::ggarrange(
    #Normality residuals
    ggplot2::ggplot(data = broom.mixed::augment(model), ggplot2::aes(x=.resid)) +
      ggplot2::geom_histogram(fill = "white", color = "black", bins = 30) +
      ggplot2::geom_vline(ggplot2::aes(xintercept = mean(.resid, na.rm=TRUE), color = "mean")) +
      ggplot2::geom_vline(ggplot2::aes(xintercept = median(.resid), color="median"))+
      ggplot2::scale_color_manual(name = "statistics", values = c(median = "blue", mean = "red"))+
      ggplot2::labs(x="Residuals") +
      ggplot2::theme(
        legend.position="bottom",
        axis.title.y = ggplot2::element_blank(),
        legend.title = ggplot2::element_blank()
      ),

    ggpubr::ggarrange(
      #Residuals vs fitted for checking linearity
      ggplot2::ggplot(data = broom.mixed::augment(model), ggplot2::aes(x = .fitted, y = .resid)) +
        ggplot2::geom_point(alpha = 0.3) +
        ggplot2::geom_smooth(method = 'loess', formula = "y ~ x") +
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
        ggplot2::labs(x = "Fitted values", y = "Residuals") +
        ggplot2::theme(
          axis.text = ggplot2::element_text(size=6),
          axis.title = ggplot2::element_text(size=8)
        ),

      #Square root of absolute standardized residuals vs fitted for checking homoscedasity (scale-location plot)
      #Currently does not work because of missing standardized residuals
      # ggplot2::ggplot(data = broom.mixed::augment(model), ggplot2::aes(x = .fitted, y = sqrt(abs(.std.resid)))) +
      #   ggplot2::geom_point(alpha = 0.3) +
      #   ggplot2::geom_smooth(method = 'loess') +
      #   ggplot2::labs(x = "Fitted values", y = "Sqrt abs std. Residuals") +
      #   ggplot2::theme(
      #     axis.text = ggplot2::element_text(size=6),
      #     axis.title = ggplot2::element_text(size=8)
      #   ),

      nrow = 2
    ),

    ncol = 2
  ) %>%
    ggpubr::annotate_figure(top = ggpubr::text_grob(
      formula(model) %>% format() %>% paste0(collapse = "") %>% stringr::str_squish(),
      just = "left", x = 0, size = 9
    ))
}
