test_that("lm_mult returns a dataframe", {
  result <- lm_mult(
    data = MS_trial_data,
    outcomes = "SDMT",
    predictors = "age"
  )
  expect_s3_class(result, "data.frame")
})

test_that("lm_mult output contains expected columns", {
  result <- lm_mult(
    data = MS_trial_data,
    outcomes = "SDMT",
    predictors = "age"
  )
  expect_true("formula" %in% names(result))
  expect_true("term" %in% names(result))
  expect_true("estimate" %in% names(result))
  expect_true("p.value" %in% names(result))
  expect_true("conf.low" %in% names(result))
  expect_true("conf.high" %in% names(result))
})

test_that("lm_mult produces rows for each outcome × predictor × covariate combination", {
  result <- lm_mult(
    data = MS_trial_data,
    outcomes = c("SDMT", "EDSS"),
    predictors = "age",
    covariates = c("", "+ gender")
  )
  formulas_expected <- 4
  expect_gte(length(unique(result$formula)), formulas_expected)
})

test_that("lm_mult with exp_log=TRUE exponentiates log-transformed outcomes", {
  result_no_exp <- lm_mult(
    data = MS_trial_data,
    outcomes = "log(SDMT)",
    predictors = "age",
    exp_log = FALSE
  )
  result_exp <- lm_mult(
    data = MS_trial_data,
    outcomes = "log(SDMT)",
    predictors = "age",
    exp_log = TRUE
  )
  # Exponentiated estimates should differ from raw estimates
  expect_false(isTRUE(all.equal(result_no_exp$estimate, result_exp$estimate)))
})

test_that("lm_mult with std_beta=TRUE includes std_estimate column", {
  result <- lm_mult(
    data = MS_trial_data,
    outcomes = "SDMT",
    predictors = "age",
    std_beta = TRUE
  )
  expect_true("std_estimate" %in% names(result))
})

test_that("lm_mult_f2m returns a dataframe with a model column", {
  df_formulas <- construct_formulas(outcomes = "SDMT", predictors = "age")
  result <- lm_mult_f2m(df_formulas, data = MS_trial_data)
  expect_s3_class(result, "data.frame")
  expect_true("model" %in% names(result))
})

test_that("lm_mult_f2m model column contains lm objects", {
  df_formulas <- construct_formulas(outcomes = "SDMT", predictors = "age")
  result <- lm_mult_f2m(df_formulas, data = MS_trial_data)
  expect_s3_class(result$model[[1]], "lm")
})

test_that("lm_mult_m2p returns a tidy dataframe from lm_mult_f2m output", {
  df_formulas <- construct_formulas(
    outcomes = c("SDMT", "EDSS"),
    predictors = "age"
  )
  df_reg <- lm_mult_f2m(df_formulas, data = MS_trial_data)
  result <- lm_mult_m2p(df_reg)
  expect_s3_class(result, "data.frame")
  expect_true("estimate" %in% names(result))
  expect_true("p.value" %in% names(result))
})

test_that("lm_mult handles formulas argument", {
  result <- lm_mult(
    data = MS_trial_data,
    outcomes = "SDMT",
    predictors = "age",
    formulas = "EDSS ~ age + gender"
  )
  expect_true(any(result$formula == "EDSS ~ age + gender"))
})
