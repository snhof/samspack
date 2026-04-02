test_that("histogram_sp returns a ggplot object", {
  result <- histogram_sp(data = MS_trial_data, var = SDMT)
  expect_s3_class(result, "gg")
})

test_that("histogram_sp works with an id variable", {
  result <- histogram_sp(data = MS_trial_data, var = SDMT, id = pat_id)
  expect_s3_class(result, "gg")
})

test_that("histogram_sp works with facet_cols argument", {
  result <- histogram_sp(
    data = MS_trial_data,
    var = SDMT,
    facet_cols = intervention
  )
  expect_s3_class(result, "gg")
})

test_that("histogram_sp works with facet_rows argument", {
  result <- histogram_sp(
    data = MS_trial_data,
    var = SDMT,
    facet_rows = INO
  )
  expect_s3_class(result, "gg")
})

test_that("histogram_sp accepts different bins_method options", {
  expect_s3_class(
    histogram_sp(data = MS_trial_data, var = SDMT, bins_method = "scott"),
    "gg"
  )
  expect_s3_class(
    histogram_sp(data = MS_trial_data, var = SDMT, bins_method = "fd"),
    "gg"
  )
  expect_s3_class(
    histogram_sp(data = MS_trial_data, var = SDMT, bins_method = 20),
    "gg"
  )
})
