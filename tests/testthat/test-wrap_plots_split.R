test_that("wrap_plots_split returns a list", {
  plots <- lapply(1:6, function(i) {
    ggplot2::ggplot(mtcars, ggplot2::aes(x = wt, y = mpg)) + ggplot2::geom_point()
  })
  result <- wrap_plots_split(plots)
  expect_type(result, "list")
})

test_that("wrap_plots_split splits 6 plots into 2 pages with n_plots=4", {
  plots <- lapply(1:6, function(i) {
    ggplot2::ggplot(mtcars, ggplot2::aes(x = wt, y = mpg)) + ggplot2::geom_point()
  })
  result <- wrap_plots_split(plots, n_plots = 4)
  expect_length(result, 2)
})

test_that("wrap_plots_split produces a single page when n_plots >= total plots", {
  plots <- lapply(1:3, function(i) {
    ggplot2::ggplot(mtcars, ggplot2::aes(x = wt, y = mpg)) + ggplot2::geom_point()
  })
  result <- wrap_plots_split(plots, n_plots = 4)
  expect_length(result, 1)
})

test_that("wrap_plots_split produces correct number of pages for 9 plots with n_plots=3", {
  plots <- lapply(1:9, function(i) {
    ggplot2::ggplot(mtcars, ggplot2::aes(x = wt, y = mpg)) + ggplot2::geom_point()
  })
  result <- wrap_plots_split(plots, n_plots = 3)
  expect_length(result, 3)
})

test_that("wrap_plots_split each page element is a patchwork object", {
  plots <- lapply(1:4, function(i) {
    ggplot2::ggplot(mtcars, ggplot2::aes(x = wt, y = mpg)) + ggplot2::geom_point()
  })
  result <- wrap_plots_split(plots, n_plots = 4)
  expect_s3_class(result[[1]], "patchwork")
})

test_that("wrap_plots_split respects ncol and nrow parameters", {
  plots <- lapply(1:4, function(i) {
    ggplot2::ggplot(mtcars, ggplot2::aes(x = wt, y = mpg)) + ggplot2::geom_point()
  })
  expect_no_error(wrap_plots_split(plots, n_plots = 4, ncol = 1, nrow = 4))
})
