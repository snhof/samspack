test_that("construct_formulas produces a dataframe with a formula column", {
  result <- construct_formulas(outcomes = "y", predictors = "x")
  expect_s3_class(result, "data.frame")
  expect_true("formula" %in% names(result))
})

test_that("construct_formulas creates correct number of formulas for outcomes × predictors", {
  result <- construct_formulas(
    outcomes = c("y1", "y2"),
    predictors = c("x1", "x2")
  )
  expect_equal(nrow(result), 4)
})

test_that("construct_formulas includes covariates in formula string", {
  result <- construct_formulas(
    outcomes = "y",
    predictors = "x",
    covariates = "+ age"
  )
  expect_true(grepl("age", result$formula[1]))
})

test_that("construct_formulas expands all combinations of outcomes, predictors and covariates", {
  result <- construct_formulas(
    outcomes = c("y1", "y2"),
    predictors = "x",
    covariates = c("", "+ age")
  )
  expect_equal(nrow(result), 4)
})

test_that("construct_formulas expands randoms correctly", {
  result <- construct_formulas(
    outcomes = "y",
    predictors = "x",
    randoms = c("", "+ (1|id)")
  )
  expect_equal(nrow(result), 2)
  expect_true(any(grepl("\\(1\\|id\\)", result$formula)))
})

test_that("construct_formulas returns only formulas when outcomes and predictors are NULL", {
  result <- construct_formulas(formulas = c("y ~ x + age", "y ~ x"))
  expect_equal(nrow(result), 2)
  expect_equal(result$formula, c("y ~ x + age", "y ~ x"))
})

test_that("construct_formulas combines outcomes/predictors with explicit formulas", {
  result <- construct_formulas(
    outcomes = "y",
    predictors = "x",
    formulas = "y ~ z"
  )
  expect_equal(nrow(result), 2)
  expect_true("y ~ z" %in% result$formula)
})

test_that("construct_formulas formula column is character type", {
  result <- construct_formulas(outcomes = "y", predictors = "x")
  expect_type(result$formula, "character")
})

test_that("construct_formulas creates correct formula string format", {
  result <- construct_formulas(outcomes = "EDSS", predictors = "age")
  expect_equal(result$formula[1], "EDSS ~ age")
})
