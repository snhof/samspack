# Minimal prosaccades data frame containing the columns required by
# demons_process_INO / demons_process_prosac.
make_prosac_data <- function() {
  data.frame(
    Research_number = c("S001", "S002", "S003"),
    T_Number_saccades = c(60L, 50L, 30L),
    L_Number_saccades = c(30L, 25L, 15L),
    L15_Number_saccades = c(15L, 12L, 7L),
    L15_VDI_AUC = c(0.9, 1.2, 1.5),
    L15_SD_VDI_AUC = c(0.05, 0.06, 0.07),
    L15_VDI_PvAm = c(0.8, 1.1, 1.3),
    L15_SD_VDI_PvAm = c(0.04, 0.05, 0.06),
    L15_VDI_peakvelocity = c(1.0, 1.1, 1.2),
    L15_SD_VDI_peakvelocity = c(0.1, 0.1, 0.1),
    R_Number_saccades = c(30L, 25L, 15L),
    R15_Number_saccades = c(15L, 12L, 7L),
    R15_VDI_AUC = c(0.8, 1.0, 1.2),
    R15_SD_VDI_AUC = c(0.05, 0.06, 0.07),
    R15_VDI_PvAm = c(0.7, 0.9, 1.25),
    R15_SD_VDI_PvAm = c(0.04, 0.05, 0.06),
    R15_VDI_peakvelocity = c(0.9, 1.0, 1.1),
    R15_SD_VDI_peakvelocity = c(0.1, 0.1, 0.1),
    stringsAsFactors = FALSE
  )
}

# ---- demons_process_INO ----

test_that("demons_process_INO returns a dataframe with INO columns", {
  data <- make_prosac_data()
  result <- demons_process_INO(data)
  expect_s3_class(result, "data.frame")
  ino_cols <- c(
    "L_INO_AUC", "R_INO_AUC", "L_INO_PvAm", "R_INO_PvAm",
    "INO_AUC", "INO_PvAm", "INO_OR", "INO_AND",
    "INO_leftwards", "INO_rightwards", "INO_bilateral",
    "INO_direction", "INO_nounibi"
  )
  expect_true(all(ino_cols %in% names(result)))
})

test_that("demons_process_INO correctly classifies INO_AUC positive case", {
  data <- make_prosac_data()
  # Subject 3 has L15_VDI_AUC = 1.5 > 1.174
  result <- demons_process_INO(data)
  expect_true(result$L_INO_AUC[3])
})

test_that("demons_process_INO correctly classifies INO_AUC negative case", {
  data <- make_prosac_data()
  # Subject 1 has L15_VDI_AUC = 0.9 < 1.174
  result <- demons_process_INO(data)
  expect_false(result$L_INO_AUC[1])
})

test_that("demons_process_INO returns NA INO_AUC when both VDI values are NA", {
  data <- make_prosac_data()
  data$L15_VDI_AUC[1] <- NA
  data$R15_VDI_AUC[1] <- NA
  result <- demons_process_INO(data)
  expect_true(is.na(result$INO_AUC[1]))
})

test_that("demons_process_INO adds log-transformed VDI columns", {
  data <- make_prosac_data()
  result <- demons_process_INO(data)
  expect_true("L15_VDI_AUC_ln" %in% names(result))
  expect_true("R15_VDI_AUC_ln" %in% names(result))
  expect_equal(result$L15_VDI_AUC_ln[1], log(data$L15_VDI_AUC[1]))
})

test_that("demons_process_INO adds fraction-of-saccades columns", {
  data <- make_prosac_data()
  result <- demons_process_INO(data)
  expect_true("T_Number_saccades_frac" %in% names(result))
  expect_equal(result$T_Number_saccades_frac[1], data$T_Number_saccades[1] / 60)
})

# ---- calcINO ----

test_that("calcINO produces same result as demons_process_INO", {
  data <- make_prosac_data()
  expect_equal(calcINO(data), demons_process_INO(data))
})

# ---- demons_process_prosac ----

test_that("demons_process_prosac returns a dataframe with QC columns", {
  data <- make_prosac_data()
  result <- demons_process_prosac(data)
  expect_s3_class(result, "data.frame")
  expect_true("QC_T_numsac" %in% names(result))
  expect_true("QC_L15_numsac" %in% names(result))
  expect_true("QC_R15_numsac" %in% names(result))
})

test_that("demons_process_prosac QC_T_numsac is TRUE when T_Number_saccades >= 30", {
  data <- make_prosac_data()
  # Add a subject with too few saccades to test the FALSE case
  data <- rbind(data, data.frame(
    Research_number = "S004",
    T_Number_saccades = 20L,
    L_Number_saccades = 10L, L15_Number_saccades = 5L,
    L15_VDI_AUC = 1.0, L15_SD_VDI_AUC = 0.05,
    L15_VDI_PvAm = 0.9, L15_SD_VDI_PvAm = 0.04,
    L15_VDI_peakvelocity = 1.0, L15_SD_VDI_peakvelocity = 0.1,
    R_Number_saccades = 10L, R15_Number_saccades = 5L,
    R15_VDI_AUC = 1.0, R15_SD_VDI_AUC = 0.05,
    R15_VDI_PvAm = 0.9, R15_SD_VDI_PvAm = 0.04,
    R15_VDI_peakvelocity = 1.0, R15_SD_VDI_peakvelocity = 0.1,
    stringsAsFactors = FALSE
  ))
  result <- demons_process_prosac(data)
  expect_true(result$QC_T_numsac[1])   # 60 >= 30 → TRUE
  expect_true(result$QC_T_numsac[3])   # 30 >= 30 → TRUE
  expect_false(result$QC_T_numsac[4])  # 20 < 30 → FALSE
})

test_that("demons_process_prosac with add_prefix=TRUE prefixes non-id columns", {
  data <- make_prosac_data()
  result <- demons_process_prosac(data, add_prefix = TRUE)
  non_id_cols <- setdiff(names(result), c("Research_number", "pat_id",
                                           "visit", "Visit", "visit_occasion",
                                           "occasion", "date", "Date"))
  expect_true(all(startsWith(non_id_cols, "prosac_")))
})

test_that("demons_process_prosac with keep_essential=TRUE reduces columns", {
  data <- make_prosac_data()
  full_result <- demons_process_prosac(data, keep_essential = FALSE)
  essential_result <- demons_process_prosac(data, keep_essential = TRUE)
  expect_lte(ncol(essential_result), ncol(full_result))
})

# ---- demons_process_fix ----

make_fix_data <- function() {
  data.frame(
    Research_number = c("S001", "S002"),
    T_Number_fixperiods = c(10L, 4L),
    C_MeanNr_SWJmin4_persec = c(0.5, 1.5),
    C_MeanNr_SWJplus4_persec = c(0.3, 0.2),
    C_Mean_amplitude_SWJ = c(0.8, 1.2),
    C_Mean_sacmin2_sec = c(0.4, 0.6),
    C_Mean_sacplus2_sec = c(0.3, 1.2),
    C_Mean_amplitude_sac = c(1.5, 2.5),
    C_Linfitcoef_X_gaze = c(0.1, 0.35),
    C_Lin_SE_X_gaze = c(0.2, 0.35),
    C_Linfitcoef_Y_gaze = c(-0.1, 0.3),
    C_Lin_SE_Y_gaze = c(0.25, 0.4),
    stringsAsFactors = FALSE
  )
}

test_that("demons_process_fix returns a dataframe with dichotomous columns", {
  data <- make_fix_data()
  result <- demons_process_fix(data)
  expect_s3_class(result, "data.frame")
  expect_true("C_SWJ_dich" %in% names(result))
  expect_true("C_sac_dich" %in% names(result))
  expect_true("C_drift_dich" %in% names(result))
  expect_true("QC_nfix" %in% names(result))
})

test_that("demons_process_fix QC_nfix is TRUE when T_Number_fixperiods >= 5", {
  data <- make_fix_data()
  result <- demons_process_fix(data)
  expect_true(result$QC_nfix[1])   # 10 >= 5
  expect_false(result$QC_nfix[2])  # 4 < 5
})

test_that("demons_process_fix with add_prefix=TRUE prefixes non-id columns", {
  data <- make_fix_data()
  result <- demons_process_fix(data, add_prefix = TRUE)
  non_id_cols <- setdiff(names(result), c("Research_number", "pat_id",
                                           "visit", "Visit", "visit_occasion",
                                           "occasion", "date", "Date"))
  expect_true(all(startsWith(non_id_cols, "fix_")))
})

# ---- demons_process_antisac ----

make_antisac_data <- function() {
  data.frame(
    Research_number = c("S001", "S002"),
    T_Number_saccades = c(40L, 19L),
    T_Proportion_errors = c(0.1, 0.3),
    stringsAsFactors = FALSE
  )
}

test_that("demons_process_antisac returns a dataframe with QC_numsac column", {
  data <- make_antisac_data()
  result <- demons_process_antisac(data)
  expect_s3_class(result, "data.frame")
  expect_true("QC_numsac" %in% names(result))
})

test_that("demons_process_antisac QC_numsac is TRUE when T_Number_saccades >= 20", {
  data <- make_antisac_data()
  result <- demons_process_antisac(data)
  expect_true(result$QC_numsac[1])   # 40 >= 20
  expect_false(result$QC_numsac[2])  # 19 < 20
})

test_that("demons_process_antisac with add_prefix=TRUE prefixes non-id columns", {
  data <- make_antisac_data()
  result <- demons_process_antisac(data, add_prefix = TRUE)
  non_id_cols <- setdiff(names(result), c("Research_number", "pat_id",
                                           "visit", "Visit", "visit_occasion",
                                           "occasion", "date", "Date"))
  expect_true(all(startsWith(non_id_cols, "antisac_")))
})
