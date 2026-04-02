test_that("glm_log_mult returns a dataframe", {
  result <- glm_log_mult(
    data = MS_trial_data,
    outcomes = "INO",
    predictors = "age"
  )
  expect_s3_class(result, "data.frame")
})

test_that("glm_log_mult output contains expected columns", {
  result <- glm_log_mult(
    data = MS_trial_data,
    outcomes = "INO",
    predictors = "age"
  )
  expect_true("formula" %in% names(result))
  expect_true("term" %in% names(result))
  expect_true("estimate" %in% names(result))
  expect_true("p.value" %in% names(result))
  expect_true("conf.low" %in% names(result))
  expect_true("conf.high" %in% names(result))
})

test_that("glm_log_mult model column is 'Logistic regression'", {
  result <- glm_log_mult(
    data = MS_trial_data,
    outcomes = "INO",
    predictors = "age"
  )
  expect_true(all(result$model == "Logistic regression"))
})

test_that("glm_log_mult with exponentiate=TRUE returns odds ratios > 0", {
  result <- glm_log_mult(
    data = MS_trial_data,
    outcomes = "INO",
    predictors = "age",
    exponentiate = TRUE
  )
  expect_true(all(result$estimate[!is.na(result$estimate)] > 0))
})

test_that("glm_log_mult with exponentiate=FALSE returns log-odds", {
  result_exp <- glm_log_mult(
    data = MS_trial_data,
    outcomes = "INO",
    predictors = "age",
    exponentiate = TRUE
  )
  result_log <- glm_log_mult(
    data = MS_trial_data,
    outcomes = "INO",
    predictors = "age",
    exponentiate = FALSE
  )
  expect_false(isTRUE(all.equal(result_exp$estimate, result_log$estimate)))
})

test_that("glm_log_mult_f2m returns model column with glm objects", {
  df_formulas <- construct_formulas(
    outcomes = "INO",
    predictors = "age"
  )
  result <- glm_log_mult_f2m(df_formulas, data = MS_trial_data)
  expect_s3_class(result, "data.frame")
  expect_true("model" %in% names(result))
  expect_s3_class(result$model[[1]], "glm")
})

test_that("glm_log_mult_m2p extracts tidy output from glm models", {
  df_formulas <- construct_formulas(
    outcomes = "INO",
    predictors = c("age", "intervention")
  )
  df_reg <- glm_log_mult_f2m(df_formulas, data = MS_trial_data)
  result <- glm_log_mult_m2p(df_reg)
  expect_s3_class(result, "data.frame")
  expect_true("estimate" %in% names(result))
})

test_that("glm_log_mult handles multiple outcomes and covariates", {
  result <- glm_log_mult(
    data = MS_trial_data,
    outcomes = "INO",
    predictors = c("intervention", "age"),
    covariates = c("", "+ gender")
  )
  expect_gte(length(unique(result$formula)), 4)
})
