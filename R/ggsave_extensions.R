



#ggplot save image functions -----
# ggsavepp <- function(filename, path) ggplot2::ggsave(filename, width = 29.21, height = 14, units = "cm", path = path)
# ggsavepphalf <- function(filename, path) ggplot2::ggsave(filename, width = 29.21/2, height = 14, units = "cm", path = path)
# ggsaveppfull <- function(filename, path) ggplot2::ggsave(filename, width = 33, height = 18, units = "cm", path = path)

#' ggsave with defaults for exporting to powerpoint
#'
#' @param filename File path and name to save the plot.
#'
#' @returns Saves the current ggplot to a file with specified dimensions suitable for PowerPoint presentations.
#' @export
#'
ggsavepp <- function(filename) {ggplot2::ggsave(filename, width = 29.21, height = 14, units = "cm")}

#' @rdname ggsavepp
ggsavepphalf <- function(filename) {ggplot2::ggsave(filename, width = 29.21/2, height = 14, units = "cm")}
#' @rdname ggsavepp
ggsaveppfull <- function(filename) {ggplot2::ggsave(filename, width = 33, height = 18, units = "cm")}
#' @rdname ggsavepp
ggsaveppquart <- function(filename) {ggplot2::ggsave(filename, width = 33/2, height = 18/2, units = "cm")}


#' ggsave with magnification option
#'
#' @param filename File path and name to save the plot.
#' @param width Width of plot
#' @param height Height of plot
#' @param units Units of width and height. Default is "cm".
#' @param magnify Magnification factor to scale down the width and height for saving.
#' @param dpi Dots per inch for the saved plot.
#'
#' @returns Saves the current ggplot to a file with specified dimensions adjusted by the magnification factor.
#' @export
#'
ggsavemagnify <- function(filename, width, height, units = "cm", magnify, dpi) {
  ggplot2::ggsave(filename, width = width/magnify, height = height/magnify, units = units, dpi = dpi)
}
