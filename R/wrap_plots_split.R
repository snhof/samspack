#' wrap_plots_split: Combine a list of ggplot objects into multiple pages with a specified number of plots per page
#'
#' @param plotlist A list of ggplot objects to be combined into pages.
#' @param n_plots Number of plots to display per page. Default is 4.
#' @param guides How to handle legends across multiple pages. Options are "collect" to gather legends into a single page, "keep" to keep legends with each plot, or "auto" to let patchwork decide. Default is "collect".
#' @param ncol Number of columns to arrange the plots in each page. Default is 2.
#' @param nrow Number of rows to arrange the plots in each page. Default is 2.
#'
#' @returns A list of patchwork objects, each containing a page of combined ggplot objects according to the specified layout and legend handling.
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' plot_list <- list(
#'  ggplot(mtcars, aes(x = wt, y = mpg)) +
#'  geom_point(),
#'  ggplot(mtcars, aes(x = hp, y = mpg)) +
#'  geom_point(),
#'  ggplot(mtcars, aes(x = disp, y = mpg)) +
#'  geom_point(),
#'  ggplot(mtcars, aes(x = qsec, y = mpg)) +
#'  geom_point(),
#'  ggplot(mtcars, aes(x = drat, y = mpg)) +
#'  geom_point(),
#'  ggplot(mtcars, aes(x = cyl, y = mpg)) +
#'  geom_point()
#'  )
#'
#'  wrap_plots_split(plot_list)
#'
wrap_plots_split <- function(plotlist, n_plots = 4, guides = "collect", ncol = 2, nrow = 2) {

  plotlist_split <- unname(split(plotlist, rep(seq(1, ceiling(length(plotlist)/n_plots)), each = n_plots)))

  purrr::map(plotlist_split, ~patchwork::wrap_plots(.x, guides = guides, ncol = ncol, nrow = nrow))

}
