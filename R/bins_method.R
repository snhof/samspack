#' Calculate bin width based on specified method
#'
#' @description Calculate the bin width for histogramming data based on different methods: Sturges, Scott, Freedman-Diaconis, or a user-defined number of bins.
#'
#' @param x numeric vector of data points
#' @param method method for calculating bin width; options are "sturges", "scott", "fd", or a numeric value specifying the number of bins
#'
#' @returns Numeric value representing the calculated bin width
#' @export
#'
#' @examples
#'
#' data <- rnorm(1000)
#' bins_method(data, method = "sturges")
#' bins_method(data, method = "scott")
#' bins_method(data, method = "fd")
#' bins_method(data, method = 30)  # User-defined number of bins
#'
bins_method <- function(x, method = "sturges") {
if(method == "sturges"){
  (max(x) - min(x)) / grDevices::nclass.Sturges(x)
} else if(method == "scott"){
  (max(x) - min(x)) / grDevices::nclass.scott(x)
} else if(method == "fd"){
  (max(x) - min(x)) / grDevices::nclass.FD(x)
} else if(is.numeric(method)){
  (max(x) - min(x)) / method
} else {
  stop("Invalid bins method. Use 'sturges', 'scott', 'fd', or a numeric value.")
}
}
