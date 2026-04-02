test_that("bins_method returns a positive numeric value for sturges method", {
  data <- rnorm(100)
  result <- bins_method(data, method = "sturges")
  expect_type(result, "double")
  expect_length(result, 1)
  expect_gt(result, 0)
})

test_that("bins_method returns a positive numeric value for scott method", {
  data <- rnorm(100)
  result <- bins_method(data, method = "scott")
  expect_type(result, "double")
  expect_length(result, 1)
  expect_gt(result, 0)
})

test_that("bins_method returns a positive numeric value for fd method", {
  data <- rnorm(100)
  result <- bins_method(data, method = "fd")
  expect_type(result, "double")
  expect_length(result, 1)
  expect_gt(result, 0)
})

test_that("bins_method returns correct value for a numeric number of bins", {
  data <- c(0, 10, 20, 30, 40, 50)
  result <- bins_method(data, method = 5)
  expect_equal(result, (50 - 0) / 5)
})

test_that("bins_method throws an error for an invalid method string", {
  data <- rnorm(100)
  expect_error(bins_method(data, method = "invalid_method"))
})

test_that("bins_method default method is sturges", {
  data <- rnorm(100)
  expect_equal(bins_method(data), bins_method(data, method = "sturges"))
})
