test_that("geeglm_log_mult returns a dataframe", {
  result <- geeglm_log_mult(
    data = MS_trial_data,
    outcomes = "INO",
    predictors = "age",
    id = "pat_id"
  )
  expect_s3_class(result, "data.frame")
})

test_that("geeglm_log_mult output contains expected columns", {
  result <- geeglm_log_mult(
    data = MS_trial_data,
    outcomes = "INO",
    predictors = "age",
    id = "pat_id"
  )
  expect_true("formula" %in% names(result))
  expect_true("term" %in% names(result))
  expect_true("estimate" %in% names(result))
  expect_true("p.value" %in% names(result))
  expect_true("conf.low" %in% names(result))
  expect_true("conf.high" %in% names(result))
})

test_that("geeglm_log_mult model column is 'Logistic GEE'", {
  result <- geeglm_log_mult(
    data = MS_trial_data,
    outcomes = "INO",
    predictors = "age",
    id = "pat_id"
  )
  expect_true(all(result$model == "Logistic GEE"))
})

test_that("geeglm_log_mult with exponentiate=TRUE returns odds ratios > 0", {
  result <- geeglm_log_mult(
    data = MS_trial_data,
    outcomes = "INO",
    predictors = "age",
    id = "pat_id",
    exponentiate = TRUE
  )
  expect_true(all(result$estimate[!is.na(result$estimate)] > 0))
})

test_that("geeglm_log_mult handles multiple predictors", {
  result <- geeglm_log_mult(
    data = MS_trial_data,
    outcomes = "INO",
    predictors = c("age", "intervention"),
    id = "pat_id"
  )
  expect_gte(length(unique(result$formula)), 2)
})
