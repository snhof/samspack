#' lm_assump_tests: Generate plots for checking linear regression assumptions
#'
#' Generate plots for checking linear regression assumptions: normality of residuals, linearity, and homoscedasticity.
#'
#' @param model A linear model object created with \code{lm()}.
#'
#' @returns Plots for checking linear regression assumptions: a histogram of residuals for normality, a residuals vs fitted plot for linearity, and a scale-location plot for homoscedasticity.
#' @export
#'
#' @examples
#'
#' lm(SDMT ~ age, data = MS_trial_data) -> model
#' lm_assump_tests(model)
#'
lm_assump_tests <- function(model) {
  ggpubr::ggarrange(
    #Normality residuals
    ggplot2::ggplot(data = broom::augment(model), ggplot2::aes(x=.resid)) +
      ggplot2::geom_histogram(fill = "white", color = "black", bins = 30) +
      ggplot2::geom_vline(ggplot2::aes(xintercept = mean(.resid, na.rm=TRUE), color = "mean")) +
      ggplot2::geom_vline(ggplot2::aes(xintercept = median(.resid), color="median"))+
      ggplot2::scale_color_manual(name = "statistics", values = c(median = "blue", mean = "red"))+
      ggplot2::labs(x="Residuals") +
      ggplot2::theme(
        legend.position="bottom",
        axis.title.y = ggplot2::element_blank(),
        legend.title=ggplot2::element_blank()
      ),

    ggpubr::ggarrange(
      #Residuals vs fitted for checking linearity
      ggplot2::ggplot(data = broom::augment(model), ggplot2::aes(x = .fitted, y = .resid)) +
        ggplot2::geom_point(alpha = 0.3) +
        ggplot2::geom_smooth(method = 'loess', formula = "y ~ x") +
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
        ggplot2::labs(x = "Fitted values", y = "Residuals") +
        ggplot2::theme(
          axis.text = ggplot2::element_text(size=6),
          axis.title = ggplot2::element_text(size=8)
        ),

      #Square root of absolute standardized residuals vs fitted for checking homoscedasity (scale-location plot)
      ggplot2::ggplot(data = broom::augment(model), ggplot2::aes(x = .fitted, y = sqrt(abs(.std.resid)))) +
        ggplot2::geom_point(alpha = 0.3) +
        ggplot2::geom_smooth(method = 'loess', formula = "y ~ x") +
        ggplot2::labs(x = "Fitted values", y = "Sqrt abs std. Residuals") +
        ggplot2::theme(
          axis.text = ggplot2::element_text(size=6),
          axis.title = ggplot2::element_text(size=8)
        ),

      nrow = 2
    ),

    ncol = 2
  ) %>%
    ggpubr::annotate_figure(top = ggpubr::text_grob(
      formula(model) %>% format() %>% paste0(collapse = "") %>% stringr::str_squish(),
      just = "left", x = 0, size = 9
    ))
}

utils::globalVariables(c(
  ".resid", "median", ".fitted", ".std.resid"
))
