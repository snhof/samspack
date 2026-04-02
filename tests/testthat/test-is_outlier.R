test_that("is_outlier returns FALSE for values within normal range", {
  x <- c(10, 12, 14, 15, 18, 20, 22)
  result <- is_outlier(x)
  expect_type(result, "logical")
  expect_length(result, length(x))
  expect_true(all(!result))
})

test_that("is_outlier identifies a high outlier", {
  x <- c(10, 12, 14, 15, 18, 20, 22, 100)
  result <- is_outlier(x)
  expect_true(result[8])
  expect_true(all(!result[1:7]))
})

test_that("is_outlier identifies a low outlier", {
  x <- c(-100, 10, 12, 14, 15, 18, 20, 22)
  result <- is_outlier(x)
  expect_true(result[1])
  expect_true(all(!result[2:8]))
})

test_that("is_outlier handles NA values without error", {
  x <- c(10, 12, NA, 15, 18, 20, 22, 100)
  expect_no_error(is_outlier(x))
})

test_that("is_outlier works on a single repeated value (IQR = 0)", {
  x <- rep(5, 10)
  result <- is_outlier(x)
  expect_type(result, "logical")
  expect_length(result, 10)
})

test_that("is_outlier correctly uses Tukey's 1.5*IQR rule", {
  # Q1 = 10, Q3 = 20, IQR = 10, fence = [10 - 15, 20 + 15] = [-5, 35]
  x <- c(10, 12, 15, 18, 20)
  result <- is_outlier(x)
  expect_true(all(!result))

  x_out <- c(10, 12, 15, 18, 20, 36)
  result_out <- is_outlier(x_out)
  expect_true(result_out[6])
})
