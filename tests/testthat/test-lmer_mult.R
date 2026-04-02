test_that("lmer_mult returns a dataframe", {
  result <- lmer_mult(
    data = MS_trial_data,
    outcomes = "SDMT",
    predictors = "intervention",
    randoms = "+ (1|pat_id)"
  )
  expect_s3_class(result, "data.frame")
})

test_that("lmer_mult output contains expected columns", {
  result <- lmer_mult(
    data = MS_trial_data,
    outcomes = "SDMT",
    predictors = "intervention",
    randoms = "+ (1|pat_id)"
  )
  expect_true("formula" %in% names(result))
  expect_true("term" %in% names(result))
  expect_true("estimate" %in% names(result))
  expect_true("p.value" %in% names(result))
  expect_true("isSingular" %in% names(result))
})

test_that("lmer_mult output includes mixed model metadata columns", {
  result <- lmer_mult(
    data = MS_trial_data,
    outcomes = "SDMT",
    predictors = "intervention",
    randoms = "+ (1|pat_id)"
  )
  expect_true("logLik" %in% names(result))
  expect_true("deviance" %in% names(result))
  expect_true("p_ranova" %in% names(result))
})

test_that("lmer_mult_f2m returns model column with lmerMod objects", {
  df_formulas <- construct_formulas(
    outcomes = "SDMT",
    predictors = "intervention",
    randoms = "+ (1|pat_id)"
  )
  result <- lmer_mult_f2m(df_formulas, data = MS_trial_data)
  expect_s3_class(result, "data.frame")
  expect_true("model" %in% names(result))
  expect_s4_class(result$model[[1]], "lmerModLmerTest")
})

test_that("lmer_mult_m2p extracts parameters from lmer models", {
  df_formulas <- construct_formulas(
    outcomes = "SDMT",
    predictors = "intervention",
    randoms = "+ (1|pat_id)"
  )
  df_reg <- lmer_mult_f2m(df_formulas, data = MS_trial_data)
  result <- lmer_mult_m2p(df_reg)
  expect_s3_class(result, "data.frame")
  expect_true("estimate" %in% names(result))
  expect_true("p.value" %in% names(result))
})

test_that("lmer_mult handles multiple outcomes and covariates", {
  result <- lmer_mult(
    data = MS_trial_data,
    outcomes = c("SDMT", "EDSS"),
    predictors = "intervention",
    covariates = c("", "+ age"),
    randoms = "+ (1|pat_id)"
  )
  expect_gte(length(unique(result$formula)), 4)
})
