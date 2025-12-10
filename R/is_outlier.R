#' is_outlier: Determine outliers according to Tukey method
#'
#' This function identifies whether a value is an outlier in a numeric vector based on the Tukey method, which defines outliers as values that fall below Q1 - 1.5*IQR or above Q3 + 1.5*IQR.
#'
#' @param x Numeric vector to check for outliers.
#'
#' @returns Logical vector indicating whether each element in `x` is an outlier (TRUE) or not (FALSE).
#' @export
#'
#' @examples
#' data_vector <- c(10, 12, 14, 15, 18, 20, 22, 100)
#' is_outlier(data_vector)
is_outlier <- function(x) {
  x < stats::quantile(x, .25, na.rm = TRUE) - 1.5*stats::IQR(x, na.rm = TRUE) | x > stats::quantile(x, .75, na.rm = TRUE) + 1.5*stats::IQR(x, na.rm = TRUE)
}
